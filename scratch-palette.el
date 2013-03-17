;;; scratch-palette --- add scratch notes for each file

;; Copyright (C) 2012 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Version: 1.0.1
;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/

;;; Commentary:

;; To install, put code like
;;
;;   (require 'scratch-palette)
;;
;; in your .emacs file. You can display and edit scratch note for the
;; currently-editing file, with "M-x scratch-palette-popup". If there's
;; no scratch notes yet, a new scratch note is created. Scratch note is
;; is hidden when pressed "C-g", "C-x C-s" or "C-x C-k", saving contents.
;; All scratch notes are stored in scratch-palette-directory. You can also
;; change the directory with code like
;;
;;   (setq scratch-palette-directory "~/.emacs.d/palette/")

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 minor fixes and refactorings

;;; Code:

;; * constants

(defconst scratch-palette-version "1.0.1")

;; * configures

(defvar scratch-palette-directory "~/.emacs.d/palette/"
  "directory used to store palette files in")

(defconst scratch-palette-popwin-available (require 'popwin nil t)
  "if popwin is avaiable to popup palettes")

;; * minor mode for scratch-palette buffer

(defvar scratch-palette-minor-mode nil
  "minor mode for palette files")

(defvar scratch-palette-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-k") 'scratch-palette-kill)
    (define-key map (kbd "C-x C-s") 'scratch-palette-kill)
    (define-key map (kbd "C-g") 'scratch-palette-kill)
    map)
  "keymap for palette files")

(make-variable-buffer-local 'scratch-palette-minor-mode)

(when (not (assq 'scratch-palette-minor-mode minor-mode-alist))
  (add-to-list
   'minor-mode-alist
   '(scratch-palette-minor-mode " Palette")))

(add-to-list 'minor-mode-map-alist
             (cons 'scratch-palette-minor-mode scratch-palette-minor-mode-map))

(defun scratch-palette-kill ()
  "save and kill palette buffer"
  (interactive) (save-buffer) (kill-buffer)
  (if scratch-palette-popwin-available
      (popwin:close-popup-window)
    (delete-window)))

;; * commands and function

(defun scratch-palette-file ()
  "get the palette filename for this buffer"
  (let ((s (buffer-file-name)))
    (if s
        (concat scratch-palette-directory
                (replace-regexp-in-string "[/:.]" "!" s)
                ".org")
      nil)))

(defun scratch-palette-popup ()
  "find the palette file and display it"
  (interactive)
  (let ((file (scratch-palette-file)))
    (if file
        (progn
          (if scratch-palette-popwin-available
              (popwin:find-file file)
            (find-file file))
          (rename-buffer "*Palette*")
          (setq scratch-palette-minor-mode t))
      (message "no visited file found for this buffer"))))

;; * provide

(provide 'scratch-palette)

;;; scratch-palette.el ends here
