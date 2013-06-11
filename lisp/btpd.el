;;; btpd.el --- Emacs frontend for the Btpd BitTorrent client
;;; Author: Igor B. Poretsky <poretsky@mlbox.ru>
;;; Keywords: Btpd, BitTorrent client

;;{{{  Copyright

;;; Copyright (C) 2013  Igor B. Poretsky

;;; This file is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This file is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ Introduction

;;; Commentary:

;;; This module provides more or less intuitive and simple Emacs interface
;;; for well-known BitTorrent client named Btpd. It actually
;;; communicates with running instance of the btpd daemon program
;;; via btcli executable in order to do the work.

;;; Usage:

;;; To make this module to be loaded automatically when Emacs starts
;;; place it in a directory mentioned in the load-path variable
;;; and include the next form into your Emacs startup file:
;;;
;;; (require 'btpd)
;;;
;;; Of course, you may byte-compile this file previously if you like.
;;;
;;; Then you can activate Btpd either via menu or by typing
;;; "M-x btpd <RET>". The control panel with active torrents
;;; list and control buttons will be popped up.

;;; Code:

;;}}}
;;{{{ Requirements

(eval-when-compile
  (require 'cl)
  (require 'wid-edit))
(require 'custom)
(require 'widget)
(require 'dired)

(autoload 'btpd-view "btpd-view" "View torrent content in virtual dired buffer.")
(autoload 'btpd-info-extract "btpd-info" "Extract info from specified torrent file")

;;}}}
;;{{{ Customizations

(defgroup btpd nil
  "Bittorrent client btpd interface options."
  :prefix "btpd-"
  :group 'applications)

(defcustom btpd-default-save-folder "."
  "Default folder where downloaded content should be placed.
You should specify a relative name here. The folder
will be actually created in the Btpd storage place.
Usually it is `files' subdirectory of the btpd home directory.

On a download start user is asked for the save folder name.
This option holds a default value that is used when the user
supplies nothing."
  :type 'string
  :group 'btpd)

(defcustom btpd-home-directory "/var/lib/btpd/"
  "The base directory where Btpd stores it's data."
  :type 'directory
  :group 'btpd)

(defcustom btpd-torrent-deletion-control '(ask . t)
  "Whether torrents should be deleted along with their content.
This option enables to clean torrents content out of the
Btpd repository, so it requires appropriate permissions
that can be acquired via sudo if enabled here."
  :type '(cons (choice :tag "Try to erase torrents content on deletion"
                       (const :tag "Never" nil)
                       (const :tag "Ask" ask)
                       (const :tag "Always" t))
               (boolean :tag "Acquire privileges via sudo when necessary"))
  :group 'btpd)

(defun btpd-set-display-option (symbol value)
  "Intended for use as the `set' function for the customization
options that affect Btpd control panel appearance."
  (custom-set-default symbol value)
  (when (fboundp 'btpd-update-control-panel)
    (btpd-update-control-panel)))

(defcustom btpd-display-info
  '(size available uploaded state leeching seeding ratio peers)
  "Choose what the information to display for torrents."
  :type '(set (const :tag "Total size" size)
              (const :tag "Amount available" available)
              (const :tag "Amount uploaded" uploaded)
              (const :tag "State" state)
              (const :tag "Leeching rate" leeching)
              (const :tag "Seeding rate" seeding)
              (const :tag "Ratio" ratio)
              (const :tag "Number of peers" peers))
  :set 'btpd-set-display-option
  :group 'btpd)

(defcustom btpd-hide-inactive-torrents nil
  "Whether inactive (stopped) torrents should be hidden. Be careful:
The `Delete all' operation will erase hidden torrents as well."
  :type 'boolean
  :set 'btpd-set-display-option
  :group 'btpd)

;;}}}
;;{{{ Utility functions

(defconst btpd-control-program "btcli"
  "Control program executable.")

(defvar btpd-torrent-name-history nil
  "History for torrent name input.")

(defvar btpd-save-folder-history nil
  "History for save folder input.")

(defun btpd-command (cmd &rest args)
  "Perform a btcli command."
  (with-temp-buffer
    (unless (zerop (apply 'call-process btpd-control-program nil t nil cmd args))
      (error (buffer-string)))))

(defun btpd-do-add (file)
  "Add torrent file to btpd."
  (let ((name (read-string "Alternative torrent name: " nil
                           'btpd-torrent-name-history))
        (args (list "-d"
                    (read-string "Folder to save in: " nil
                                 'btpd-save-folder-history
                                 btpd-default-save-folder)
                    (expand-file-name file))))
    (when (and name (not (string= "" name)))
      (push name args)
      (push "-n" args))
    (apply 'btpd-command "add" args)))

(defun btpd-get-info ()
  "Get list of torrents under control. Each list item is represented
by a vector of 14 strings filled with following information:

0. Torrent id number for reference in btcli commands.

1. Torrent hash.

2. Torrent name.

3. Download directory.

4. Total size in a convenient form.

5. Total size in bytes.

6. Status sign.

7. Available amount in percents.

8. Ratio.

9. Number of peers.

10. Bytes downloaded.

11. Download rate.

12. Bytes uploaded.

13. Upload rate."
  (let ((info nil))
    (with-temp-buffer
      (unless (zerop (call-process btpd-control-program nil t nil "list" "-f"
                                   "%# %h %S %s\n%n\n%d\n%t %p %r %P %g %v %u %^\n\n"))
        (error (buffer-string)))
      (goto-char (point-min))
      (while (looking-at "^\\([0-9]+\\) \\(\\w+\\) +\\([0-9]+\\) +\\([0-9.]+[GKM]\\) *$")
        (let ((item (make-vector 14 nil)))
          (aset item 0 (match-string-no-properties 1))
          (aset item 1 (match-string-no-properties 2))
          (aset item 4 (match-string-no-properties 4))
          (aset item 5 (match-string-no-properties 3))
          (forward-line 1)
          (looking-at "^.*$")
          (aset item 2 (match-string-no-properties 0))
          (forward-line 1)
          (looking-at "^.*$")
          (aset item 3 (expand-file-name (match-string-no-properties 0)))
          (forward-line 1)
          (looking-at "^\\(.\\) +\\([0-9.%]+\\) +\\([0-9.]+\\) +\\([0-9]+\\) +\\([0-9]+\\) +\\([0-9]+\\) +\\([0-9]+\\) +\\([0-9]+\\) *$")
          (dotimes (i 8)
            (aset item (+ i 6) (match-string-no-properties (1+ i))))
          (add-to-list 'info item 'append)
          (forward-line 2))))
    info))

(defun btpd-search (key torrent-list)
  "Search torrent by it's hash in a list provided by
`btpd-get-info'. Return the item found or nil."
  (let ((torrents torrent-list))
    (while (and torrents
                (not (string-equal (aref (car torrents) 1) key)))
      (setq torrents (cdr torrents)))
    (car torrents)))

(defun btpd-torrent-file (hash)
  "Construct torrent file path from a hash value."
  (expand-file-name "torrent"
                    (expand-file-name hash
                                      (expand-file-name "torrents"
                                                        btpd-home-directory))))

(defun btpd-delete-content (tree base-directory)
  "Delete directory tree content recursively."
  (let ((fspec
         (if (or (not (cdr btpd-torrent-deletion-control))
                 (file-writable-p base-directory))
             "%s"
           "/sudo::%s")))
    (dolist (node tree)
      (unless (string-equal (car node) "..")
        (let ((item-path (expand-file-name (car node) base-directory)))
          (if (not (listp (cdr node)))
              (condition-case nil
                  (delete-file (format fspec item-path))
                (error nil))
            (btpd-delete-content (cdr node) item-path)
            (condition-case nil
                (delete-directory (format fspec item-path))
              (error nil))))))))

(defun btpd-delete (torrents)
  "Delete torrents from the Btpd control."
  (if (or (eq (car btpd-torrent-deletion-control) t)
          (and (car btpd-torrent-deletion-control)
               (y-or-n-p "Erase content as well? ")))
      (dolist (item torrents)
        (let ((tree (aref (btpd-info-extract (btpd-torrent-file (cadr item))) 3))
              (base-directory (directory-file-name (cddr item))))
          (btpd-command "del" (car item))
          (btpd-delete-content tree base-directory)
          (unless (string-equal "files"
                                (file-relative-name base-directory
                                                    btpd-home-directory))
            (condition-case nil
                (delete-directory
                 (format
                  (if (or (not (cdr btpd-torrent-deletion-control))
                          (file-writable-p (file-name-directory base-directory)))
                      "%s"
                    "/sudo::%s")
                  base-directory))
              (error nil)))))
    (apply 'btpd-command "del" (mapcar 'car torrents))))

;;}}}
;;{{{ Major mode definition

(defvar btpd-control-mode-map (make-sparse-keymap)
  "Keymap for Btpd control panel.")

(set-keymap-parent btpd-control-mode-map widget-keymap)

(define-derived-mode btpd-control-mode fundamental-mode
  "Control panel"
  "This is a Btpd control panel.
Navigate around and press buttons.

\\{btpd-control-mode-map}")

;;}}}
;;{{{ Common widgets

(defconst btpd-control-panel "*Btpd*"
  "Name of buffer for Btpd control widgets.")

(defconst btpd-new-torrent-confirmation-dialog "*Btpd new torrent*"
  "Buffer name for new torrent confirmation dialog.")

(defun btpd-action (button &rest ignore)
  "General Btpd button click reaction."
  (when (or (not (widget-get button ':unsafe))
            (yes-or-no-p (format "%s? " (widget-get button ':help-echo))))
    (if (functionp (widget-get button ':command))
        (funcall (widget-get button ':command) (widget-get button ':command-args))
      (apply 'btpd-command (widget-get button ':command) (widget-get button ':command-args)))
    (funcall (widget-get button ':panel) (widget-get button ':panel-arg))))

(defun btpd-close-panel (&rest ignore)
  "Close button reaction."
  (quit-window t))

(defun btpd-create-button (label action target panel panel-arg &optional unsafe)
  "General button creation helper."
  (widget-create 'push-button
                 :tag (if (or panel-arg
                              (not (eq panel 'btpd-refresh-panel)))
                          label
                        (format "%s all" label))
                 :help-echo (format "%s %s" label
                                    (if (or panel-arg
                                            (not (eq panel 'btpd-refresh-panel)))
                                        (car target)
                                      (format "all %storrents" (car target))))
                 :command action
                 :command-args (cdr target)
                 :panel panel
                 :panel-arg panel-arg
                 :unsafe unsafe
                 :notify 'btpd-action
                 (if (or panel-arg
                         (not (eq panel 'btpd-refresh-panel)))
                     label
                   (format "%s all" label))))

(defun btpd-create-stop-button (panel &optional target panel-arg)
  "Create stop button."
  (btpd-create-button "Stop" "stop"
                      (if target
                          (list (aref target 2) (aref target 0))
                        (list "active " "-a"))
                      panel panel-arg))

(defun btpd-create-resume-button (panel &optional target panel-arg)
  "Create resume button."
  (btpd-create-button "Resume" "start"
                      (if target
                          (list (aref target 2) (aref target 0))
                        (list "inactive " "-a"))
                      panel panel-arg))

(defun btpd-create-delete-button (panel target &optional panel-arg)
  "Create delete button."
  (btpd-create-button "Delete" 'btpd-delete
                      (if (vectorp target)
                          (list (aref target 2)
                                (cons (aref target 0)
                                      (cons (aref target 1)
                                            (aref target 3))))
                        (list "" target))
                      panel panel-arg t))

(defun btpd-create-close-button (label help)
  "Create close button with specified label and help prompt."
  (widget-create 'push-button
                 :tag label
                 :help-echo help
                 :notify 'btpd-close-panel
                 label))

(defun btpd-create-refresh-button (action)
  "Make a refresh button with specified action."
  (widget-create 'push-button
                 :tag "Refresh"
                 :help-echo "Refresh panel"
                 :panel-action action
                 :notify (lambda (button &rest ignore)
                           (funcall (widget-get button ':panel-action)))
                 "Refresh"))

(defun btpd-create-torrent-handle (name file &optional content-path)
  "Create a torrent handle widget."
  (let ((widget-push-button-prefix "")
        (widget-push-button-suffix ""))
    (widget-create 'push-button
                   :tag name
                   :help-echo "Push me to view the content"
                   :torrent-file file
                   :content-path content-path
                   :notify (lambda (handle &rest ignore)
                             (btpd-view (widget-get handle ':torrent-file)
                                        (widget-get handle ':content-path)
                                        (widget-get handle ':tag)))
                   name)))

(defconst btpd-value-format-units
  (list (cons (* 1024 1024 1024) "G")
        (cons (* 1024 1024) "M")
        (cons 1024 "k"))
  "Associated list of unit factors and associated signs.")

(defun btpd-format-value (value)
  "Transform a numeric value into convenient string representation.
Accepts string representation of a source value as well."
  (let ((src (or (and (stringp value) (string-to-number value)) value))
        (units btpd-value-format-units))
    (while (and units (< src (caar units)))
      (setq units (cdr units)))
    (if units
        (format (concat "%.2f" (cdar units)) (/ (float src) (caar units)))
      (format "%d" src))))

(defun btpd-format-size (value)
  "Generate conventional size representation string.
Accepts number of bytes in the numeric or string representation."
  (let ((bytes (or (and (stringp value) (string-to-number value)) value)))
    (if (< bytes 1024)
        (if (= bytes 1)
            "1 byte"
          (format "%d bytes" bytes))
      (if (stringp value)
          (format "%s (%s bytes)" (btpd-format-value bytes) value)
        (format "%s (%d bytes)" (btpd-format-value bytes) value)))))

(defun btpd-display-torrent (item panel &optional panel-arg)
  "Arrange a torrent item control section on a control panel."
  (widget-insert "\n")
  (btpd-create-torrent-handle (aref item 2)
                              (btpd-torrent-file (aref item 1))
                              (aref item 3))
  (widget-insert "\n")
  (when (memq 'size btpd-display-info)
    (widget-insert "Size: " (btpd-format-size (aref item 5)) "\n"))
  (when (memq 'available btpd-display-info)
    (widget-insert "Available " (aref item 7) ": " (btpd-format-size (aref item 10)) "\n"))
  (when (memq 'uploaded btpd-display-info)
    (widget-insert "Uploaded: " (btpd-format-size (aref item 12)) "\n"))
  (when (memq 'ratio btpd-display-info)
    (widget-insert "Ratio: " (aref item 8) "\n"))
  (cond
   ((string-equal "+" (aref item 6))
    (when (memq 'state btpd-display-info)
      (widget-insert "About to start\n"))
    (btpd-create-stop-button panel item panel-arg)
    (widget-insert "  "))
   ((and (string-equal "-" (aref item 6))
         (memq 'state btpd-display-info))
    (widget-insert "About to stop\n"))
   ((string-equal "I" (aref item 6))
    (when (memq 'state btpd-display-info)
      (widget-insert "Inactive\n"))
    (btpd-create-resume-button panel item panel-arg)
    (widget-insert "  "))
   ((and (string-equal "L" (aref item 6))
         (memq 'leeching btpd-display-info))
    (widget-insert "Leeching at " (btpd-format-value (aref item 11)) "B/s\n"))
   ((and (not (string-equal "S" (aref item 6)))
         (memq 'state btpd-display-info))
    (widget-insert "Undetermined state\n")))
  (when (string-match "[LS]" (aref item 6))
    (when (memq 'seeding btpd-display-info)
      (widget-insert "Seeding at " (btpd-format-value (aref item 13)) "B/s\n"))
    (when (memq 'peers btpd-display-info)
      (widget-insert "Number of peers: " (aref item 9) "\n"))
    (btpd-create-stop-button panel item panel-arg)
    (widget-insert "  "))
  (btpd-create-delete-button panel item panel-arg)
  (widget-insert "\n"))

;;}}}
;;{{{ New torrent adding dialog

(defvar btpd-new-torrent nil
  "Holds name, file path and hash for the torrent about to add.")

(defun btpd-create-add-ok-button ()
  "Create add confirmation button."
  (widget-create 'push-button
                 :tag "Ok"
                 :help-echo "Really add this torrent"
                 :notify (lambda (button &rest ignore)
                           (btpd-do-add (aref btpd-new-torrent 1))
                           (btpd-update-control-panel (aref btpd-new-torrent 2))
                           (btpd-close-panel))
                 "Ok"))

(defun btpd-initialize-new-torrent-confirmation (name file hash)
  "Fill Btpd new torrent confirmation panel with the actual content."
  (let ((duplicate (btpd-search hash (btpd-get-info))))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (when (fboundp 'remove-overlays)
      (remove-overlays))
    (btpd-control-mode)
    (set (make-local-variable 'btpd-new-torrent) (vector name file hash))
    (widget-insert "You are about to add new torrent\n")
    (btpd-create-torrent-handle (aref btpd-new-torrent 0) (aref btpd-new-torrent 1))
    (widget-insert "\n")
    (if duplicate
        (widget-insert "But it seems you have it already. See below.\n"
                       "Explore the situation and delete the duplicate if you wish to proceed.\n")
      (widget-insert "Choose an appropriate action below. To preview torrent content click on it's name.\n")
      (btpd-create-add-ok-button)
      (widget-insert "  "))
    (btpd-create-close-button "Cancel" "Cancel the operation and close dialog panel")
    (widget-insert "\n")
    (when duplicate
      (btpd-display-torrent duplicate
                            (lambda (&rest ignore)
                              (when (buffer-live-p (get-buffer btpd-control-panel))
                                (with-current-buffer btpd-control-panel
                                  (btpd-refresh-panel)))
                              (btpd-refresh-new-torrent-confirmation)))
      (widget-insert "\n")
      (btpd-create-refresh-button 'btpd-refresh-new-torrent-confirmation)
      (widget-insert "\n")))
  (widget-setup)
  (goto-char (point-min))
  (widget-forward 2))

(defun btpd-refresh-new-torrent-confirmation ()
  "Refresh new torrent confirmation dialog."
  (btpd-initialize-new-torrent-confirmation
   (aref btpd-new-torrent 0)
   (aref btpd-new-torrent 1)
   (aref btpd-new-torrent 2)))

;;}}}
;;{{{ Main control panel

(defun btpd-create-add-new-button ()
  "Create add new button."
  (widget-create 'push-button
                 :tag "Add new"
                 :help-echo "Add new torrent"
                 :notify (lambda (&rest ignore)
                           (call-interactively 'btpd-add))
                 "Add new"))

(defun btpd-create-customize-button ()
  "Create button to enter customization mode."
  (widget-create 'push-button
                 :tag "Customize"
                 :help-echo "Set up some options"
                 :notify (lambda (&rest ignore)
                           (customize-group 'btpd))
                 "Customize"))

(defun btpd-refresh-panel (&optional current-item)
  "Refresh Btpd control panel and go to specified current item if any."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (when (fboundp 'remove-overlays)
    (remove-overlays))
  (btpd-control-mode)
  (widget-insert "Btpd control panel\n\n")
  (btpd-create-add-new-button)
  (widget-insert "  ")
  (btpd-create-close-button "Close" "Close control panel")
  (widget-insert "  ")
  (btpd-create-customize-button)
  (widget-insert "\n")
  (let ((torrents (btpd-get-info))
        (active nil)
        (inactive nil)
        (all nil)
        (position nil)
        (item-count 0))
    (when torrents
      (widget-insert "\nTorrents under control:\n")
      (dolist (item torrents)
        (unless (and btpd-hide-inactive-torrents
                     (string-match "[-I]" (aref item 6)))
          (when (or (and (integerp current-item)
                         (= item-count current-item))
                    (and (stringp current-item)
                         (string-equal (aref item 1) current-item)))
            (setq position (point)))
          (btpd-display-torrent item 'btpd-refresh-panel item-count)
          (setq item-count (1+ item-count)))
        (push (cons (aref item 0) (cons (aref item 1) (aref item 3))) all)
        (cond
         ((string-match "[LS]" (aref item 6))
          (setq active t))
         ((string-match "I" (aref item 6))
          (setq inactive t))))
      (when btpd-hide-inactive-torrents
        (widget-insert (format "\nShown %d/%d\n" item-count (length all))
                       "Inactive torrents are hidden\n"))
      (widget-insert "\n")
      (when active
        (btpd-create-stop-button 'btpd-refresh-panel)
        (widget-insert "  "))
      (when inactive
        (btpd-create-resume-button 'btpd-refresh-panel)
        (widget-insert "  "))
      (btpd-create-delete-button 'btpd-refresh-panel all)
      (widget-insert "\n"))
    (widget-insert "\n")
    (btpd-create-refresh-button 'btpd-refresh-panel)
    (widget-insert "\n")
    (widget-setup)
    (goto-char (or position (point-min)))
    (widget-forward 1)))

(defun btpd-update-control-panel (&optional current-item)
  "Update control panel if it exists somewhere."
  (when (buffer-live-p (get-buffer btpd-control-panel))
    (with-current-buffer btpd-control-panel
      (btpd-refresh-panel current-item)
      (dolist (window (get-buffer-window-list))
        (set-window-point window (point))))))

;;}}}
;;{{{ Interactive commands

(defun btpd ()
  "Pop up Btpd control panel."
  (interactive)
  (with-current-buffer (get-buffer-create btpd-control-panel)
    (kill-all-local-variables)
    (btpd-refresh-panel))
  (switch-to-buffer btpd-control-panel))

(defun btpd-add (torrent-file &optional cleanup-function)
  "Interactively add new torrent from specified file.
The second argument is optional. If not `nil' it specifies
a hook function to use at the buffer killing."
  (interactive "fTorrent file: ")
  (let* ((torrent-info (btpd-info-extract (expand-file-name torrent-file)))
         (panel (generate-new-buffer btpd-new-torrent-confirmation-dialog)))
    (with-current-buffer panel
      (kill-all-local-variables)
      (when cleanup-function
        (add-hook 'kill-buffer-hook cleanup-function nil t))
      (btpd-initialize-new-torrent-confirmation (aref torrent-info 0)
                                                (aref torrent-info 5)
                                                (aref torrent-info 7)))
    (switch-to-buffer panel)))

(defun btpd-add-from-dired ()
  "Add torrent from a file in dired."
  (interactive)
  (let ((file (dired-get-filename 'verbatim t)))
    (if file
        (if (file-regular-p file)
            (btpd-add file)
          (error "Not a regular file"))
      (error "No file on this line")))
  (when (and (interactive-p)
             (featurep 'emacspeak))
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defun btpd-quit ()
  "Close Btpd control panel."
  (interactive)
  (unless (eq major-mode 'btpd-control-mode)
    (error "Not in Btpd control panel"))
  (btpd-close-panel)
  (when (and (interactive-p)
             (featurep 'emacspeak))
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ Key definitions

(define-key btpd-control-mode-map (kbd "q") 'btpd-quit)

;;}}}

(provide 'btpd)

;;; btpd.el ends here
