;;; god-leader.el --- Leader key configuration for god-mode

;; Copyright (C) 2022 Łukasz Pawelczyk

;; Author: Łukasz Pawelczyk <havner@gmail.com>
;; Maintainer: Łukasz Pawelczyk <havner@gmail.com
;; URL: https://github.com/havner/god-leader
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: keys keybinding config leader god god-mode

;; This file is not part of GNU Emacs.

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


;;; Commentary:

;; Simple helper for god-mode to add leader key functionality.


;;; Code:

(require 'god-mode)

(defgroup god-leader nil
  "Leader key configuration for god-mode."
  :group 'god)

(defvar god-leader-map (make-sparse-keymap))

(defcustom god-leader-root-key
  "SPC"
  "A shortcut for the leader map, has to be `kbd' compatible string"
  :group 'god-leader
  :type 'string)

(defcustom god-leader-root-description
  "leader root"
  "A leader root description visible when using `which-key'"
  :group 'god-leader
  :type 'string)

(defcustom god-leader-maps
  '((special-mode-map . simple)
    (dired-mode-map . dired)
    (custom-mode-map . cus-edit)
    (finder-mode-map . finder)
    (grep-mode-map . grep))
  "List of cons of file/map or maps to add the leader key."
  :group 'god-leader
  :type '(alist))

;;;###autoload
(defun god-leader-do-map (map)
  "Add leader key to specific map."
  (if (car-safe map)
      (eval-after-load (cdr map)
        `(define-key ,(car map) (kbd god-leader-root-key) god-leader-map))
    (define-key (eval map) (kbd god-leader-root-key) god-leader-map)))

;;;###autoload
(defun god-leader-initialize ()
  "Setup leader key shortcut."
  (interactive)
  (with-eval-after-load 'god-mode
    (define-key god-local-mode-map (kbd god-leader-root-key) god-leader-map)
    (when (fboundp 'which-key-add-key-based-replacements)
      (which-key-add-key-based-replacements god-leader-root-key god-leader-root-description))
    (dolist (map god-leader-maps)
      (god-leader-do-map map))))

;;;###autoload
(defun god-leader-define-key (key fun &optional description)
  "Add a new key to the leader map. KEY is passed to `kbd'."
  (let ((k (kbd key))
        (s (concat god-leader-root-key " " key)))
    (define-key god-leader-map k fun)
    (when (and description (fboundp 'which-key-add-key-based-replacements))
      (which-key-add-key-based-replacements s description))))

;;;###autoload
(defun god-leader-describe-key (key description)
  "Add description to specific KEY."
  (let ((s (concat god-leader-root-key " " key)))
    (when (fboundp 'which-key-add-key-based-replacements)
      (which-key-add-key-based-replacements s description))))

(provide 'god-leader)

;;; god-leader.el ends here
