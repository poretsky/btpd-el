;;; btpd-view.el --- Torrent content viewer for Btpd
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

;;; This module is part of the Emacs frontend for the Btpd
;;; BitTorrent client. It provides a convenient way to browse
;;; torrent contents just like a virtual directory tree.

;;; Code:

;;}}}
;;{{{ Requirements

(eval-when-compile
  (require 'cl))
(require 'dired)
(require 'dired-x)
(require 'btpd-utils)
(require 'btpd-bindings)

;;}}}
;;{{{ Displaying torrent content as virtual directory tree

(defconst btpd-view-time-format "%Y-%m-%d %R"
  "Time format for directory listings.")

(defvar btpd-view-torrent-info nil
  "Stores info about currently viewed torrent as a vector of 8 elements
initially provided by `btpd-info-extract' and then consulted
and modified by other functions. It's elements keep the following data:

0. Torrent name.

1. Total size in bytes represented as string.

2. Total number of files represented as string.

3. Hierarchical file list.

4. Source torrent file attributes with modification time in string format.

5. Displayed directory path (real or fake).

6. Content availability flag.

7. Torrent hash.

8. Tracker URLs list.")
(make-variable-buffer-local 'btpd-view-torrent-info)

(defvar btpd-view-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map dired-mode-map)
    (define-key map (kbd "z") 'btpd-view-size)
    (loop for op in
          '(dired-advertised-find-file
            dired-find-file
            dired-view-file)
          do
          (eval
           `(define-key map [remap ,op] 'btpd-view-visit-item)))
    (loop for op in
          '(dired-flag-file-deletion
            dired-do-flagged-delete
            dired-do-delete
            dired-do-chgrp
            dired-do-chmod
            dired-do-chown
            dired-do-rename
            dired-do-rename-regexp
            dired-do-touch
            dired-do-compress
            dired-do-byte-compile
            dired-do-query-replace-regexp
            dired-sort-toggle-or-edit
            wdired-change-to-wdired-mode)
          do
          (eval
           `(define-key map [remap ,op] 'btpd-view-disabled)))
    map)
  "Keymap for torrent content virtual tree navigation.")

(defun btpd-view-buffer-setup (torrent-info)
  "Being provided by the torrent info, fill current buffer with the
viewed torrent directory content and setup it for browsing.
Torrent data must conform to the format detailed
in the `btpd-view-torrent-info' description."
  (let ((inhibit-read-only t)
        (content nil)
        (attributes nil)
        (links-size 0)
        (uid-size 0)
        (gid-size 0)
        (flen-size 0))
    (dolist (node (aref torrent-info 3))
      (push (make-vector 7 nil) content)
      (setq attributes
            (or (and (aref torrent-info 6)
                     (file-attributes (expand-file-name (car node)
                                                        (aref torrent-info 5))
                                      'string))
                (aref torrent-info 4)))
      (if (not (stringp (nth 5 attributes)))
          (setcar (nthcdr 5 attributes) (format-time-string btpd-view-time-format (nth 5 attributes)))
        (setcar (cdr attributes)
                (let ((count 1))
                  (when (listp (cdr node))
                    (dolist (child (cdr node))
                      (when (and (listp (cdr child))
                                 (not (string-equal (car child) "..")))
                        (incf count))))
                  count))
        (setcar (nthcdr 8 attributes)
                (if (listp (cdr node))
                    "dr-xr-xr-x"
                  "----------")))
      (aset (car content) 0 (nth 8 attributes))
      (aset (car content) 1 (number-to-string (nth 1 attributes)))
      (when (> (length (aref (car content) 1)) links-size)
        (setq links-size (length (aref (car content) 1))))
      (aset (car content) 2
            (if (numberp (nth 2 attributes))
                (number-to-string (nth 2 attributes))
              (nth 2 attributes)))
      (when (> (length (aref (car content) 2)) uid-size)
        (setq uid-size (length (aref (car content) 2))))
      (aset (car content) 3
            (if (numberp (nth 3 attributes))
                (number-to-string (nth 3 attributes))
              (nth 3 attributes)))
      (when (> (length (aref (car content) 3)) gid-size)
        (setq gid-size (length (aref (car content) 3))))
      (aset (car content) 4
            (if (stringp (cdr node))
                (cdr node)
              "0"))
      (when (> (length (aref (car content) 4)) flen-size)
        (setq flen-size (length (aref (car content) 4))))
      (aset (car content) 5 (nth 5 attributes))
      (aset (car content) 6 (car node)))
    (erase-buffer)
    (when (fboundp 'remove-overlays)
      (remove-overlays))
    (insert "  " (aref torrent-info 5) ":\n")
    (dolist (url (aref torrent-info 8))
      (insert "  " url "\n"))
    (insert "  Total " (btpd-format-size (aref torrent-info 1))
            " in " (aref torrent-info 2) " file")
    (unless (= (string-to-number (aref torrent-info 2)) 1)
      (insert "s"))
    (insert "\n")
    (dolist (item content)
      (insert "  " (aref item 0)
              " "
              (format (concat "%" (number-to-string links-size) "s")
                      (aref item 1))
              " "
              (format (concat "%-" (number-to-string uid-size) "s")
                      (aref item 2))
              " "
              (format (concat "%-" (number-to-string gid-size) "s")
                      (aref item 3))
              " "
              (format (concat "%" (number-to-string flen-size) "s")
                      (aref item 4))
              " "
              (aref item 5)
              " "
              (aref item 6)
              "\n"))
    (set (make-local-variable 'dired-actual-switches) "-al")
    (dired-build-subdir-alist)
    (dired-virtual (aref torrent-info 5))
    (use-local-map btpd-view-keymap))
  (setq btpd-view-torrent-info torrent-info)
  (set (make-local-variable 'revert-buffer-function) 'btpd-view-revert))

;;}}}
;;{{{ Utility functions

(defun btpd-view-revert (&rest ignore)
  "The revert buffer function for the torrent view buffers."
  (let ((opoint (point))
        (ofile (dired-get-filename nil t))
        (mark-alist (dired-remember-marks (point-min) (point-max))))
    (btpd-view-buffer-setup btpd-view-torrent-info)
    (let ((inhibit-read-only t))
      (dired-mark-remembered mark-alist)
      (or (and ofile (dired-goto-file ofile))
          (goto-char opoint))
      (dired-move-to-filename))))

(defun btpd-view-get-size (nodes)
  "Calculate total size of the listed nodes.
Returns cons cell with total size in car and number of files in cdr."
  (let ((size 0)
        (files 0))
    (dolist (item nodes)
      (unless (string-equal (car item) "..")
        (if (listp (cdr item))
            (let ((augment (btpd-view-get-size (cdr item))))
              (incf size (car augment))
              (incf files (cdr augment)))
          (incf size (string-to-number (cdr item)))
          (incf files))))
    (cons size files)))

(defun btpd-view-validate-buffer ()
  "Ensure that we are in an appropriate buffer."
  (unless btpd-view-torrent-info
    (error "Not in a torrent view buffer")))

(defun btpd-view-get-filename ()
  "Get file name at point."
  (let ((filename (dired-get-filename 'verbatim t)))
    (unless filename
      (error "No file on this line"))
    filename))

(defun btpd-view-get-node ()
  "Get node at point or `nil' if none."
  (assoc (btpd-view-get-filename) (aref btpd-view-torrent-info 3)))

;;}}}
;;{{{ Interactive commands

(defun btpd-view (torrent-file &optional base-dir name)
  "View torrent content in virtual dired buffer.
Other arguments are optional. The second one provides
the base directory where actual content resides if it is available.
The third argument allows to specify an alternative torrent name
that will be inherited by the buffer displaying the content."
  (interactive "fTorrent file: ")
  (let ((torrent-info (btpd-info-extract torrent-file)))
    (when base-dir
      (aset torrent-info 5 (expand-file-name base-dir))
      (aset torrent-info 6 t))
    (setcar (nthcdr 5 (aref torrent-info 4))
            (format-time-string btpd-view-time-format
                                (nth 5 (aref torrent-info 4))))
    (with-current-buffer (get-buffer-create (or name (aref torrent-info 0)))
      (btpd-view-buffer-setup torrent-info)
      (switch-to-buffer (current-buffer)))))

(defun btpd-view-visit-item ()
  "Visit file or directory at point."
  (interactive)
  (btpd-view-validate-buffer)
  (let ((node (btpd-view-get-node)))
    (if (and node (listp (cdr node)))
        (let ((position (and (string-equal (car node) "..")
                             (aref btpd-view-torrent-info 5))))
          (aset btpd-view-torrent-info 3 (cdr node))
          (aset btpd-view-torrent-info 5
                (expand-file-name (car node)
                                  (aref btpd-view-torrent-info 5)))
          (btpd-view-buffer-setup btpd-view-torrent-info)
          (when position
            (dired-goto-file position)))
      (call-interactively 'dired-view-file))))

(defun btpd-view-from-dired ()
  "Preview torrent content from a file in dired."
  (interactive)
  (let ((file (btpd-view-get-filename)))
    (if (file-regular-p file)
        (btpd-view file)
      (error "Not a regular file"))))

(defun btpd-view-size (&optional arg)
  "Calculate total size of all marked (or next ARG) files
or directories in a torrent view buffer. Parent directory link `..'
is always ignored even if it is selected."
  (interactive "P")
  (btpd-view-validate-buffer)
  (let ((emacspeak-speak-messages t)
        (amount (btpd-view-get-size (dired-map-over-marks (btpd-view-get-node) arg))))
    (message "%s in %d file%s"
             (btpd-format-size (car amount))
             (cdr amount)
             (if (= (cdr amount) 1)
                 ""
               "s"))))

(defun btpd-view-disabled ()
  "Signal a disabled operation."
  (interactive)
  (error "The operation is not available here"))

;;}}}

(provide 'btpd-view)

;;; btpd-view.el ends here
