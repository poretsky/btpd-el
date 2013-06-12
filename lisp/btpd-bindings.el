;;; btpd-bindings.el --- General bindings for btpd-el package
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

;;; This module contains all necessary autoloads and other initial actions
;;; that should be done to communicate comfortably with Btpd
;;; in Emacs. It should be required from an Emacs startup file.

;;; Code:

;;}}}
;;{{{ Autoload interactive commands

(autoload 'btpd "btpd" "Btpd BitTorrent client" t)
(autoload 'btpd-add "btpd" "Add torrent file to btpd interactively." t)
(autoload 'btpd-add-from-dired "btpd" "Add torrent from a file in dired." t)
(autoload 'btpd-view "btpd-view" "View torrent content in virtual dired buffer." t)
(autoload 'btpd-view-from-dired "btpd-view" "Preview torrent content from a file in dired." t)

;;}}}
;;{{{ Add item to the main menu

(define-key global-map [menu-bar btpd] '("Btpd" . btpd))

;;}}}
;;{{{ Bindings for dired

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "M-t") 'btpd-add-from-dired)
     (define-key dired-mode-map (kbd "M-RET") 'btpd-view-from-dired)))

;;}}}
;;{{{ Bindings for w3m

(eval-after-load 'w3m '(require 'btpd-w3m nil t))

;;}}}

(provide 'btpd-bindings)

;;; btpd-bindings.el ends here
