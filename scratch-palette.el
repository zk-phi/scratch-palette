;;; scratch-palette.el --- save scratch notes on each file

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
;; 1.0.2 yank region automatically
;;       palette detection on find-file

;;; Code:

;; * constants

(defconst scratch-palette-version "1.0.1")

;; * configures

(defgroup scratch-palette nil
  "add scratch notes for each file"
  :group 'emacs)

(defcustom scratch-palette-directory "~/.emacs.d/palette/"
  "directory used to store palette files in"
  :group 'scratch-palette)

;; * popwin integration

(defconst scratch-palette-popwin-available (require 'popwin nil t)
  "if popwin is avaiable to popup palettes")

;; * minor mode for scratch-palette buffer

(defvar scratch-palette-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-k") 'scratch-palette-kill)
    (define-key map (kbd "C-x C-s") 'scratch-palette-kill)
    (define-key map (kbd "C-g") 'scratch-palette-kill)
    map)
  "keymap for palette files")

(define-minor-mode scratch-palette-minor-mode
  "minor mode for palette files"
  :init-value nil
  :global nil
  :map scratch-palette-minor-mode-map
  :lighter " Palette")

(defun scratch-palette-kill ()
  "save and kill palette buffer"
  (interactive)
  (save-buffer)
  (kill-buffer)
  (if scratch-palette-popwin-available
      (popwin:close-popup-window)
    (delete-window)))

;; * commands and functions

(defun scratch-palette-file ()
  "get the palette filename for this buffer"
  (let ((bfn (buffer-file-name)))
    (when bfn
      (concat scratch-palette-directory
              (replace-regexp-in-string "[/:]" "!" bfn)))))

;;;###autoload
(defun scratch-palette-popup ()
  "find the palette file and display it"
  (interactive)
  (let ((file (scratch-palette-file))
        str)
    (when (use-region-p)
      (setq str (buffer-substring (region-beginning) (region-end)))
      (delete-region (region-beginning) (region-end))
      (deactivate-mark))
    (if (null file)
        (error "not file buffer")
      (if scratch-palette-popwin-available
          (popwin:find-file file)
        (find-file file))
      (rename-buffer "*Palette*")
      (scratch-palette-minor-mode 1)
      (goto-char (point-max))
      (when str
        (insert (concat "\n" str "\n"))))))

;; * palette detection

(defun scratch-palette-detect-scratch ()
  (when (file-exists-p (scratch-palette-file))
    (message "note: scratch-palette detected.")
    (sit-for 0.5)))

(add-hook 'find-file-hook 'scratch-palette-detect-scratch)

;; * provide

(provide 'scratch-palette)

;;; scratch-palette.el ends here
