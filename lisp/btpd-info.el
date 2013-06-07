;;; btpd-info.el --- Torrent info extraction utility
;;; Author: Igor B. Poretsky <poretsky@mlbox.ru>
;;; Keywords: Btpd, bittorrent client

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

;;; This module is part of the Emacs frontend for the Btpd bittorrent
;;; client. It provides torrent info extraction facility. It utilizes
;;; btinfo executable for retrieving data from a torrent file.

;;; Code:

;;}}}
;;{{{ Requirements

(eval-when-compile
  (require 'cl))

;;}}}
;;{{{ Torrent info extraction

(defconst btpd-info-command "btinfo"
  "Torrent info retrieving command.")

(defconst btpd-info-name-extractor "^Name: \\(.*\\)$"
  "Regexp for torrent name extraction.")

(defconst btpd-info-size-extractor "^Total size: \\([0-9]+\\)$"
  "Regexp for total torrent size extraction.")

(defconst btpd-info-nfiles-extractor "^Number of files: \\([0-9]+\\)$"
  "Regexp for torrent content files number extraction.")

(defconst btpd-info-content-list-header "^Files:$"
  "Regexp matching file list header in the torrent info.")

(defconst btpd-info-file-info-extractor "^\\(.*\\) (\\([0-9]+\\))$"
  "Regexp for file name and size extraction from the torrent info.")

(defconst btpd-info-hash-extractor "^Info hash: \\(.*\\)$"
  "Regexp for hash value extractor.")

(defun btpd-info-extract (file)
  "Extract info from specified torrent file
and return it as a vector of 8 elements:

0. Torrent name.

1. Total size in bytes represented as string.

2. Total number of files represented as string.

3. File list.

4. Source torrent file attributes.

5. Path to the source torrent file.

6. Reserved. It is initialized by `nil'.

7. Torrent hash.

File list is organized hierarchically. Directories are represented by
cons cells containing the directory name in car and it's content in cdr.
Files are represented by cons cells with the name in car and size in cdr.
File size is expressed in bytes and represented by string.
All these lists are maintained in the reverse order."
  (let ((content (make-vector 8 nil)))
    (with-temp-buffer
      (unless (zerop (call-process btpd-info-command nil t nil file))
        (error (buffer-string)))
      (goto-char (point-min))
      (when (re-search-forward btpd-info-name-extractor nil t)
        (aset content 0 (match-string-no-properties 1)))
      (goto-char (point-min))
      (when (re-search-forward btpd-info-size-extractor nil t)
        (aset content 1 (match-string-no-properties 1)))
      (goto-char (point-min))
      (when (re-search-forward btpd-info-nfiles-extractor nil t)
        (aset content 2 (match-string-no-properties 1)))
      (goto-char (point-min))
      (when (re-search-forward btpd-info-hash-extractor nil t)
        (aset content 7 (match-string-no-properties 1)))
      (goto-char (point-min))
      (when (re-search-forward btpd-info-content-list-header nil t)
        (while (re-search-forward btpd-info-file-info-extractor nil t)
          (let ((path nil)
                (tree nil)
                (fn (match-string-no-properties 1)))
            (while fn
              (push (file-name-nondirectory (directory-file-name fn)) path)
              (setq fn (file-name-directory (directory-file-name fn))))
            (setq fn (cons nil (aref content 3)))
            (dolist (component path)
              (setq tree (cdr fn))
              (setcdr fn
                      (add-to-list 'tree (cons component nil) nil
                                   (lambda (x y)
                                     (string-equal (car x) (car y)))))
              (setq fn (assoc component tree))
              (unless (cdr fn)
                (setcdr fn (list (cons ".." tree)))))
            (setcdr fn (match-string-no-properties 2))
            (while (setq fn (assoc ".." tree))
              (setq tree (cdr fn)))
            (aset content 3 tree)))))
    (aset content 4 (file-attributes file 'string))
    (aset content 5 (expand-file-name file))
    content))

;;}}}

(provide 'btpd-info)

;;; btpd-info.el ends here
