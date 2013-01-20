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
;; Add these lines to your emacs init file :
; (add-to-list 'load-path "<path-to-wicd-mode>/")
; (require 'wicd-mode)

;; run this programme by the command
; M-x wicd

;; Description:
;; Wicd (https://launchpad.net/wicd), is a popular network
;; communication manager for linux. It is composed of a daemon
;; running as root and of clients running as unprivileged users
;; daemon and clients communicate by the D-Bus 
;; (http://www.freedesktop.org/wiki/Software/dbus) message bus
;; system. This file implements a Wicd client for Emacs using
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
  '(("essid" "%-24s ")
    ("bssid" "%17s ")
    ("channel" "%-7s ")
    ("quality" "%-3s%%"))
  "List of displayed fields with their corresponding format strings."
  :group 'wicd-wireless
  )

(defcustom wicd-wireless-filter
  ".*"
  "Regexp used as a filter to select meaningfull ESSID."
  :group 'wicd-wireless
  :type 'string
  )

(defcustom wicd-wireless-scan-hook
  'wicd-wireless-display
  "Hook run when the daemon signals that the scan is finished."
  )

(defgroup wicd-global-mode nil
  "Global minor mode for managing network connections on the mode-line using wicd."
  :group 'wicd
  :group 'mode-line
  )

(defcustom wicd-global-mode-string
  (concat " " (propertize "Wicd" 'help-echo "Help") " ")
  "Text shown in the mode line when `wicd-global-mode' is active."
  :group 'wicd-global-mode
  :risky t
  )

;; Communication with wicd-daemon via D-BUS

(defun wicd-dbus-name (obj)
  "return name of dbus object OBJ
OBJ is one of :deamon, :wired or :wireless"
  (case obj
    (:daemon wicd-dbus-name)
    (:wired (concat wicd-dbus-name ".wired"))
    (:wireless (concat wicd-dbus-name ".wireless"))
    ))
(defun wicd-dbus-path (obj)
  "return path of dbus object OBJ
OBJ is one of :deamon, :wired or :wireless"
  (case obj
    (:daemon wicd-dbus-path)
    (:wired (concat wicd-dbus-path "/wired"))
    (:wireless (concat wicd-dbus-path "/wireless"))
    ))

(defun wicd-method (obj method)
  "Call METHOD of object OBJ without arguments
OBJ is one of :deamon, :wired or :wireless"
  (dbus-call-method :system
                    wicd-dbus-name
                    (wicd-dbus-path obj)
                    (wicd-dbus-name obj)
                    method)
  )

(defun wicd-method-arg (obj method &rest l)
  "Call METHOD of object OBJ with argument list L
OBJ is one of :deamon, :wired or :wireless"
  (apply 'dbus-call-method
         :system
         wicd-dbus-name
         (wicd-dbus-path obj)
         (wicd-dbus-name obj)
         method
         l)
  )

(defun wicd-wireless-nb ()
  "Return the number of available connections."
  (wicd-method :wireless "GetNumberOfNetworks")
  )

(defun wicd-dbus-wireless-prop (id prop)
  "Ask the WICD daemon for the value of the property PROP of wireless network ID."
  (wicd-method-arg :wireless "GetWirelessProperty" id prop)
  )

(defun wicd-wireless-connected ()
  "Id of the current connection."
  (wicd-method-arg :wireless "GetCurrentNetworkID")
  )

(defun wicd-wireless-connect (n)
  "Try to connect to wireless network number N"
  (interactive "nWireless network id: ")
  (wicd-method-arg :wireless "ConnectWireless" n)
  )

(defun wicd-wireless-disconnect ()
  "Disconnect from the current wireless network"
  (interactive)
  (wicd-method :wireless "DisconnectWireless")
  )

(defun wicd-wireless-scan ()
  "Ask the daemon to rescan available wireless networks."
  (wicd-method :wireless "Scan")
  )

;; Managing the list of available wireless connections

(defvar wicd-wireless-list nil
  "The list of available wireless networks.
Each element is an alist"
  )

(defun wicd-wireless-list ()
  "Fill the variable `wicd-wireless-list'."
  (setq wicd-wireless-list nil)
  (let ((i 0)
        (n (wicd-wireless-nb))
        network)
    (while (< i n)
      (setq network nil)
      (dolist (prop '("essid" "bssid" "channel" "quality"))
        (setq network (plist-put network prop (wicd-dbus-wireless-prop i prop))))
      (add-to-list 'wicd-wireless-list (cons i network))
      (setq i (+ 1 i))
      )
    wicd-wireless-list))


(defun wicd-wireless-prop (id prop)
  "Return property PROP of wireless network ID."
  (lax-plist-get (cdr (assoc id wicd-wireless-list)) prop)
  )




;;; Major Mode

(defun wicd-wireless-format ()
  (apply 'concat (mapcar 'cadr wicd-wireless-prop-list))
  )

(defun wicd-wireless-header-string ()
  "String put in the header-line."
  (concat (propertize " " 'display '((space :align-to 0)))
          "#  "
          (apply 'format (wicd-wireless-format)
                 (mapcar 'car wicd-wireless-prop-list))
          )
  )

(defun wicd-wireless-header ()
  "Set the header-line to wicd-wireless-header-string"
  (setq header-line-format (wicd-wireless-header-string))
  )


(defun wicd-wireless-display ()
  "Redisplay wireless network list"
  (interactive)
  (wicd-wireless-list)
  (with-current-buffer wicd-buffer-name
    (save-excursion
      (let ((buffer-read-only))
        (erase-buffer)
        (wicd-wireless-header)
        (let ((i 0)
              (n (length wicd-wireless-list))
              start)
          (while (< i n)
            (let ((essid (wicd-wireless-prop i "essid")))
              (when (string-match wicd-wireless-filter essid)
                (setq start (point))
                (insert (format "%-2d " i))
                (insert (apply 'format (wicd-wireless-format)
                               (mapcar (lambda (a)
                                         (wicd-wireless-prop i (car a)))
                                       wicd-wireless-prop-list
                                       )))
                (when (= (wicd-wireless-connected) i)
                  (overlay-put (make-overlay start (point)) 'face 'bold)
                  )
                (insert "\n")))
            (setq i (+ 1 i))
            )
          )
        )
      )
    )
  )

(defun wicd-wireless-refresh ()
  "Refresh and redisplay available wireless networks"
  (interactive)
  (wicd-wireless-scan)
  (pop-to-buffer wicd-buffer-name)
  (wicd-wireless-display)
  )




(defun wicd-wireless-connect-current-line ()
  "Try to connect to wireless network displayed on the current line of the wicd buffer"
  (interactive)
  (let* ((id (- (line-number-at-pos) 1))
         (network (wicd-wireless-prop id "essid")))
    (message "Connecting to wireless network %s with id %d." network id)
    (wicd-wireless-connect (with-current-buffer wicd-buffer-name id))
    )
  )

;; Mode definition

(defcustom wicd-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "r" 'wicd-wireless-refresh)
    (define-key map "g" 'wicd-wireless-display)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "c" 'wicd-wireless-connect-current-line)
    (define-key map "d" 'wicd-wireless-disconnect)
    (define-key map (kbd "RET") 'wicd-wireless-connect-current-line)
    map)
  "Keymap for `wicd-mode'."
  :group 'wicd
  )

(define-derived-mode
  wicd-mode
  special-mode
  "Wicd"
  :group 'wicd
  :keymap 'wicd-mode-map
  )

(add-hook 'wicd-mode-hook 'wicd-wireless-refresh)

(defun wicd ()
  "Run a wicd client in a buffer."
  (interactive)
  (pop-to-buffer wicd-buffer-name)
  (wicd-mode)
  (run-with-timer 5 nil 'wicd-wireless-refresh)
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
  nil
  " Wicd"
  nil
  :global t
  :group 'wicd-global-mode
  (or global-mode-string (setq global-mode-string '("")))
  (when wicd-global-mode
    (or (memq 'wicd-global-mode-string global-mode-string)
        (setq global-mode-string
              (append global-mode-string '(wicd-global-mode-string))))
    )
  )

;; TODO
;; Minor mode menu
(defun wicd-wireless-menu-refresh
  (wicd-wireless-scan))

(defvar wicd-global-mode-keymap
  (make-sparse-keymap "Wicd")
  "Keymap for Wicd global minor mode.")

(setq wicd-global-mode-keymap (make-sparse-keymap "Wicd")) ;; debug

(add-to-list 'minor-mode-map-alist `(wicd-global-mode . ,wicd-global-mode-keymap)) ;; minor mode keymap is active when minor mode is

;; Minor mode menu
(define-key wicd-global-mode-keymap [menu-bar wicd]
  `(menu-item "Wicd" ,(make-sparse-keymap) :key-sequence nil))

(define-key wicd-global-mode-keymap [menu-bar wicd networks]
  `(menu-item "Networks" ,(make-sparse-keymap) :key-sequence nil))

(define-key wicd-global-mode-keymap [menu-bar wicd networks refresh]
  `(menu-item "Refresh" wicd-wireless-menu-refresh))


(define-key wicd-global-mode-keymap [menu-bar wicd networks 0]
  `(menu-item "Id0" nil :key-sequence nil))
(define-key wicd-global-mode-keymap [menu-bar wicd networks 1]
  `(menu-item "Id1" nil :key-sequence nil))
(define-key wicd-global-mode-keymap [menu-bar wicd networks 2]
  `(menu-item "Id2" nil :key-sequence nil))



;; Run a hook when scan is finished

(dbus-register-signal
 :system
 wicd-dbus-name
 (wicd-dbus-path :wireless)
 (wicd-dbus-name :wireless)
 "SendEndScanSignal"
 wicd-wireless-scan-hook)


(provide 'wicd-mode)
;;; wicd-mode.el ends here.
