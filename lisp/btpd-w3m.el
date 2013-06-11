;;; btpd-w3m.el --- Btpd bindings for Emacs-w3m
;;; Author: Igor B. Poretsky <poretsky@mlbox.ru>
;;; Keywords: W3m, Btpd, BitTorrent client

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

;;; This module is part of the Emacs frontend for the Btpd BitTorrent
;;; client. It provides seamless torrents previewing and downloading
;;; facilities for the Emacs-w3m web-browser.

;;; Code:

;;}}}
;;{{{ Requirements

(eval-when-compile
  (require 'cl)
  (require 'w3m nil t))
(require 'custom)
(eval-when (load)
  (require 'w3m))

(autoload 'btpd-add "btpd" "Interactively add new torrent from specified file." t)

;;}}}
;;{{{ Customizations

(defcustom btpd-w3m-auto-setup t
  "If not `nil' setup w3m automatically to pass torrents to btpd.
This is done by adding items to the end of `w3m-content-type-alist'
if there is no item for the respective content type already."
  :type 'boolean
  :group 'btpd)

;;}}}
;;{{{ Bittorrent content type viewer function

(defconst btpd-w3m-torrent-content-type "application/x-bittorrent"
  "BitTorrent content type.")

(defconst btpd-w3m-torrent-file-name-pattern "\\.torrent\\'"
  "Regexp matching BitTorrent file name.")

(defun btpd-w3m-cleanup ()
  "Remove downloaded torrent file after work."
  (when (and (boundp 'btpd-new-torrent)
             (vectorp btpd-new-torrent)
             (= (length btpd-new-torrent) 3)
             (stringp (aref btpd-new-torrent 1)))
    (cd "~")
    (delete-directory (file-name-directory (aref btpd-new-torrent 1)) t)))

(defun btpd-w3m-add-url (url &rest ignore)
  "Add URL to btpd."
  (let ((w3m-current-buffer (current-buffer))
        (working-directory (make-temp-file "btpd-w3m-" t)))
    (set-file-modes working-directory
                    (file-modes-symbolic-to-number "go+rx" (file-modes working-directory)))
    (lexical-let ((file (expand-file-name "torrent" working-directory))
                  (pos (point-marker))
                  (curl w3m-current-url))
      (w3m-process-with-null-handler
        (w3m-process-do
            (success (w3m-download url file nil handler))
          (if (not success)
              (delete-directory (file-name-directory file) t)
            (when (and (equal curl w3m-current-url)
                       (buffer-name (marker-buffer pos)))
              (with-current-buffer (marker-buffer pos)
                (goto-char pos)
                (w3m-refontify-anchor)))
            (when (zerop (call-process "gzip" nil nil nil "-t" file))
              (rename-file file (format "%s.gz" file))
              (call-process "gzip" nil nil nil (format "%s.gz" file)))
            (btpd-add file 'btpd-w3m-cleanup)))))))

;;}}}
;;{{{ W3m setup for cooperation with btpd daemon

(defun btpd-w3m-setup ()
  "Setup emacs-w3m to pass BitTorrent URLs to Btpd."
  (when (and btpd-w3m-auto-setup
             (boundp 'w3m-content-type-alist)
             (not (assoc btpd-w3m-torrent-content-type w3m-content-type-alist)))
    (add-to-list 'w3m-content-type-alist
                 (list btpd-w3m-torrent-content-type
                       btpd-w3m-torrent-file-name-pattern
                       'btpd-w3m-add-url
                       nil)
                 'append)
    (add-to-list 'w3m-content-type-alist
                 (list "" "" nil nil)
                 'append)
    (custom-note-var-changed 'w3m-content-type-alist)))

(eval-after-load "w3m" '(btpd-w3m-setup))

;;}}}

(provide 'btpd-w3m)

;;; btpd-w3m.el ends here
