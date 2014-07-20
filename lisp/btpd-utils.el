;;; btpd-utils.el --- Common utility functions for btpd-el package
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

;;; This module is part of the Emacs frontend for the Btpd BitTorrent
;;; client. It is required by other modules to provide some commonly
;;; used functionality. The btinfo executable is utilized
;;; for retrieving data from a torrent file.

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

(defconst btpd-info-urls-detector "^Tracker URLs: \\[ \\(.*\\) ]$"
  "Regexp matching Tracker URLs info item.")

(defconst btpd-info-url-extractor "\\[ \\([^ ]+\\) ]"
  "Regexp for Tracker URL extraction.")

(defun btpd-info-fix-links (tree)
  "Fix links in the directory tree."
  (dolist (node tree)
    (when (and (listp (cdr node))
               (not (string-equal (car node) "..")))
      (let ((backlink (assoc ".." (cdr node))))
        (when backlink
          (setcdr backlink tree)))
      (btpd-info-fix-links (cdr node)))))

(defun btpd-info-extract (file)
  "Extract info from specified torrent file
and return it as a vector of 9 elements:

0. Torrent name.

1. Total size in bytes represented as string.

2. Total number of files represented as string.

3. File list.

4. Source torrent file attributes.

5. Path to the source torrent file.

6. Reserved. It is initialized by `nil'.

7. Torrent hash.

8. Tracker URLs list.

File list is organized hierarchically. Directories are represented by
cons cells containing the directory name in car and it's content in cdr.
Files are represented by cons cells with the name in car and size in cdr.
File size is expressed in bytes and represented by string.
All these lists are maintained in the reverse order."
  (let ((content (make-vector 9 nil)))
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
      (when (re-search-forward btpd-info-urls-detector nil t)
        (goto-char (match-beginning 1))
        (let ((bound (match-end 1))
              (urls nil))
          (while (re-search-forward btpd-info-url-extractor bound t)
            (add-to-list 'urls (match-string-no-properties 1) 'append))
          (aset content 8 urls)))
      (goto-char (point-min))
      (when (re-search-forward btpd-info-content-list-header nil t)
        (let ((files nil))
          (while (re-search-forward btpd-info-file-info-extractor nil t)
            (push (cons (match-string-no-properties 1) (match-string-no-properties 2)) files))
          (setq files
                (sort files
                      (lambda (x y)
                        (string-lessp (car x) (car y)))))
          (dolist (item files)
            (let ((path nil)
                  (tree nil)
                  (fn (car item)))
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
              (setcdr fn (cdr item))
              (while (setq fn (assoc ".." tree))
                (setq tree (cdr fn)))
              (aset content 3 tree))))))
    (btpd-info-fix-links (aref content 3))
    (aset content 4 (file-attributes file 'string))
    (aset content 5 (expand-file-name file))
    content))

;;}}}
;;{{{ Formatting values for display

(defconst btpd-value-format-units
  (list (cons (* 1024 1024 1024) "G")
        (cons (* 1024 1024) "M")
        (cons 1024 "k"))
  "Associated list of unit factors and respective signs.")

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

;;}}}

(provide 'btpd-utils)

;;; btpd-utils.el ends here
