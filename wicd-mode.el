;;; wicd-mode.el --- Client for the Wicd network connection manager

;;; Copyright (C) 2012, 2013 Raphaël Cauderlier <cauderlier@crans.org>

;;; Author: Raphaël Cauderlier <cauderlier@crans.org>

;;; Version: 1.2

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; This file is *NOT* part of GNU Emacs.

;;; Commentary:
;; Installation:
;; Add these lines to your Emacs init file :
; (add-to-list 'load-path "<path-to-wicd-mode>/")
; (require 'wicd-mode)

;; run this programme by the command
; M-x wicd

;; Description:
;; Wicd (https://launchpad.net/wicd), is a popular network
;; communication manager for linux.  It is composed of a daemon
;; running as root and of clients running as unprivileged users
;; daemon and clients communicate by the D-Bus
;; (http://www.freedesktop.org/wiki/Software/dbus) message bus
;; system.  This file implements a Wicd client for Emacs using
;; the D-Bus binding for Emacs Lisp.

;; Compatibility: GNU Emacs with D-Bus binding (version 23 or higher)

;;; Code:

(require 'dbus)

;; Customization

(defgroup wicd nil
  "Elisp client application for the wicd network connection manager."
  :group 'applications
  )

(defcustom wicd-buffer-name "*Wicd*"
  "Name of the buffer used for listing available connections."
  :group 'wicd
  )

(defgroup wicd-dbus nil
  "Parameters for communication with wicd-daemon using D-Bus."
  :group 'wicd
  )

(defcustom wicd-dbus-name "org.wicd.daemon"
  "Name of the main D-Bus object."
  :group 'wicd-dbus
  )

(defcustom wicd-dbus-path "/org/wicd/daemon"
  "Path of the main D-Bus object."
  :group 'wicd-dbus
  )

(defgroup wicd-wireless nil
  "Display of the list of available wireless connections."
  :group 'wicd
  )

(defcustom wicd-wireless-prop-list
  '(("#" :size 2 :prop "id" :type num)
    ("essid" :size 24 :align right)
    ("quality" :format "%s%%" :type num)
    ("strength" :format "%sdB" :type num)
    ("encryption" :prop "encryption_method")
    ("bssid" :size 17 :align right)
    ("channel" :type num)
    )
  "List of displayed fields.
Each cell is a list starting from a string, the name of the displayed column.
Next elements are a plist whose keys are
:prop for the name of the Wicd property, defaults to the name
:size for the width of the column, defaults to the length of name
:align for specifiying if the column should be left-aligned (symbol `left')
       or right-aligned (symbol `right'), defaults to `left'.
:type for the type of the entry,  either `str' or `num', defaults to `str'.
:format for a format string to apply to the entries, default to \"%s\"."
  :group 'wicd-wireless
  )

(defun wicd-wireless-prop-names-formats ()
  "Return the list of requested columns properties and formats."
  (mapcar
   (lambda (l)
     (cons
      (or (plist-get (cdr l) :prop) (car l))
      (or (plist-get (cdr l) :format) "%s")))
   wicd-wireless-prop-list)
  )

(defun wicd-id-comp (a b)
  "Compares the ids of entries A and B."
  (< (car a) (car b)))

(defun wicd-num-entry-comp (a b)
  "Compares entries A and B by their Ith field as a number."
  (let ((i (tabulated-list--column-number (car tabulated-list-sort-key))))
    (< (string-to-number (aref (cadr a) i))
       (string-to-number (aref (cadr b) i)))))

(defun wicd-wireless-prop-tabulated (name plist)
  "Convert an element of `wicd-wireless-prop-list' into a list used for building `tabulated-list-format'."
  (list name
        (or (plist-get plist :size) (length name))
        (if (eq (plist-get plist :type) 'num) 'wicd-num-entry-comp t)
        :right-align (eq (plist-get plist :align) 'right)))

(defun wicd-wireless-prop-vector ()
  "Vector of columns for use in `tabulated-list-format'."
  (apply 'vector
         (mapcar (lambda (l) (wicd-wireless-prop-tabulated (car l) (cdr l)))
                 wicd-wireless-prop-list)))

(defvar wicd-wireless-scanning nil
  "Whether we are waiting for Wicd daemon to finish the scanning of available networks.")

(defcustom wicd-wireless-scan-hook
  '(wicd-wireless-list wicd-wireless-display wicd-menu-refresh)
  "Hook run when the daemon signals that the scan is finished."
  :type 'hook
  )

(defgroup wicd-global-mode nil
  "Global minor mode for managing network connections on the mode-line using wicd."
  :group 'wicd
  :group 'mode-line
  )

;; Communication with wicd-daemon via D-BUS

(defun wicd-buffer ()
  "Return the buffer for the wicd application."
  (get-buffer-create wicd-buffer-name))

(defun wicd-dbus-name (obj)
  "Return name of dbus object OBJ.
OBJ is one of :deamon, :wired or :wireless"
  (cond
    ((eq obj :daemon) wicd-dbus-name)
    ((eq obj :wired) (concat wicd-dbus-name ".wired"))
    ((eq obj :wireless) (concat wicd-dbus-name ".wireless"))
    ))
(defun wicd-dbus-path (obj)
  "Return path of dbus object OBJ.
OBJ is one of :deamon, :wired or :wireless"
  (cond
    ((eq obj :daemon) wicd-dbus-path)
    ((eq obj :wired) (concat wicd-dbus-path "/wired"))
    ((eq obj :wireless) (concat wicd-dbus-path "/wireless"))
    ))

(defun wicd-method (obj method &rest l)
  "Call a dbus wicd method with arguments.
OBJ is one of :deamon, :wired or :wireless,
METHOD is a string, the name of a method of the DBus object OBJ,
L is a list of arguments to pass to METHOD."
  (apply 'dbus-call-method
         :system
         wicd-dbus-name
         (wicd-dbus-path obj)
         (wicd-dbus-name obj)
         method
         l))

(defvar wicd-wireless-max-id 10
  "If set to an integer, that many wireless networks at most will be displayed.")

(defun wicd-wireless-nb ()
  "Return the number of available connections."
  (let ((n (wicd-method :wireless "GetNumberOfNetworks")))
    (if wicd-wireless-max-id
        (min wicd-wireless-max-id n)
      n)))

(defun wicd-dbus-wireless-prop (id prop)
  "Ask the Wicd daemon for the value of a property.
ID is a wireless network id (an integer),
PROP is a string, the name of the property."
  (wicd-method :wireless "GetWirelessProperty" id prop))

(defun wicd-wireless-connected ()
  "Id of the current connection."
  (wicd-method :wireless "GetCurrentNetworkID"))

(defun wicd-wireless-connect (n)
  "Try to connect to wireless network number N."
  (interactive "nWireless network id: ")
  (wicd-method :wireless "ConnectWireless" n))

(defun wicd-wireless-disconnect ()
  "Disconnect from the current wireless network."
  (interactive)
  (wicd-method :wireless "DisconnectWireless"))

(defun wicd-wireless-scan ()
  "Ask the daemon to rescan available wireless networks."
  (interactive)
  (setq wicd-wireless-scanning t)
  (wicd-wireless-display)
  (wicd-method :wireless "Scan"))

;; Managing the list of available wireless connections

(defvar wicd-wireless-list nil
  "The list of available wireless networks.
Each element is an alist")

(defun wicd-wireless-list ()
  "Fill the variable `wicd-wireless-list'."
  (setq wicd-wireless-list nil)
  (let ((i 0)
        (n (wicd-wireless-nb))
        network)
    (while (< i n)
      (setq network (plist-put nil "id" i))
      (dolist (prop (cdr (wicd-wireless-prop-names)))
        (setq network (plist-put network prop (wicd-dbus-wireless-prop i prop))))
      (add-to-list 'wicd-wireless-list (cons i network))
      (prin1 network)
      (setq i (+ 1 i))
      )
    wicd-wireless-list))


;; (defun wicd-wireless-prop (id prop)
;;   "Like `wicd-dbus-wireless-prop' but using a cache.
;; This function returns the same value than `wicd-dbus-wireless-prop'
;; but reads it in `wicd-wireless-list' instead of asking the deamon.
;; ID is a wireless network id (an integer),
;; PROP is a string, the name of the property."
;;   "Return property PROP of wireless network ID."
;;   (lax-plist-get (cdr (assoc id wicd-wireless-list)) prop))
(defun wicd-wireless-prop (id prop)
  "Alias for `wicd-dbus-wireless-prop'."
  (wicd-dbus-wireless-prop id prop))



;;; Major Mode

(defun wicd-wireless-display ()
  "Redisplay wireless network list."
  (interactive)
  (with-current-buffer (wicd-buffer)
    (tabulated-list-print)))

(defun wicd-wireless-connect-current-line ()
  "Try to connect to wireless network displayed on the current line of the wicd buffer."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (network (wicd-wireless-prop id "essid")))
    (message "Connecting to wireless network %s with id %d." network id)
    (wicd-wireless-connect (with-current-buffer wicd-buffer-name id))))

;; Mode definition

(defcustom wicd-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "r" 'wicd-wireless-scan)
    (define-key map "g" 'wicd-wireless-display)
    (define-key map "c" 'wicd-wireless-connect-current-line)
    (define-key map "d" 'wicd-wireless-disconnect)
    (define-key map (kbd "RET") 'wicd-wireless-connect-current-line)
    map)
  "Keymap for `wicd-mode'."
  :group 'wicd
  )

(defun wicd-wireless-cell (cell)
  "Ignore CELL."
  (when cell
    (cons (format "%s" (cadr cell)) (wicd-wireless-cell (cddr cell)))))

(defun wicd-fill ()
  "Fill wicd-wireless-list."
;  (wicd-wireless-list)
  (mapcar
   (lambda (x)
     (let ((i (car x))
           (cell (cdr x)))
       (list i
             (apply 'vector (wicd-wireless-cell cell)))))
   wicd-wireless-list))


(define-derived-mode
  wicd-mode
  tabulated-list-mode
  "Wicd"
  :group 'wicd
  :keymap 'wicd-mode-map
  (setq tabulated-list-format
        (wicd-wireless-prop-vector)
        tabulated-list-entries
        (lambda () (wicd-fill))
        )
  (tabulated-list-init-header)
  )

(add-hook 'wicd-mode-hook 'wicd-wireless-scan)

(defun wicd ()
  "Run a wicd client in a buffer."
  (interactive)
  (pop-to-buffer wicd-buffer-name)
  (wicd-mode)
  (tabulated-list-print)
;  (run-with-timer 5 nil 'wicd-wireless-scan)
  )

;; Global Minor Mode for the mode-line

(define-minor-mode wicd-global-mode
  "Toggle Wicd global mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Wicd global mode is enabled, the mode-line shows a menu to
manage network connections. See also the command `wicd'."
  :lighter " Wicd" :global t :group 'wicd-global-mode
;  (add-hook 'wicd-wireless-scan-hook 'wicd-global-refresh-network-menu)
  )

;; TODO
;; Minor mode menu

(defvar wicd-global-mode-keymap
  (make-sparse-keymap "Wicd")
  "Keymap for Wicd global minor mode.")

(setq wicd-global-mode-keymap (make-sparse-keymap "Wicd")) ;; debug


;; Minor mode menu

(defun wicd-menu-refresh ()
  "Regenarate the menu for wicd global minor mode."
  (interactive)
  (let (l
        (i (- (length wicd-wireless-list) 1)))
    (while (>= i 0)
      (let* ((essid (wicd-wireless-prop i "essid"))
             (displayed-essid (if (= (wicd-wireless-connected) i)
                                  (concat "*" essid "*") ; (propertize essid :face 'bold)
                                essid)))
        (add-to-list 'l `[,displayed-essid (lambda () (interactive) (wicd-wireless-connect ,i)) t])
        (setq i (- i 1))
        )
      )
    (easy-menu-define
      nil
      wicd-global-mode-keymap
      "Some doc"
      `("Wicd"
        ["Scan"
         wicd-wireless-scan
         (not wicd-wireless-scanning)]
        ["Disconnect"
         wicd-wireless-disconnect
         (> (wicd-wireless-connected) (- 1))]
        ,(if l
             (cons "Networks" l)
           ["Networks" nil nil]))))
  (add-to-list 'minor-mode-map-alist
               `(wicd-global-mode . ,wicd-global-mode-keymap)) ;; minor mode keymap is active when minor mode is
  )

;; Run a hook when scan is finished

(dbus-register-signal
 :system
 wicd-dbus-name
 (wicd-dbus-path :wireless)
 (wicd-dbus-name :wireless)
 "SendEndScanSignal"
 (lambda ()
   (setq wicd-wireless-scanning nil)
   (run-hooks 'wicd-wireless-scan-hook)))

(dbus-register-signal
 :system
 wicd-dbus-name
 (wicd-dbus-path :wireless)
 (wicd-dbus-name :wireless)
 "SendStartScanSignal"
 (lambda ()
   (setq wicd-wireless-scanning t)
   (with-current-buffer (wicd-buffer)
     (let (buffer-read-only)
       (erase-buffer)
       (insert "Scanning…\n")))))


(provide 'wicd-mode)
;;; wicd-mode.el ends here
