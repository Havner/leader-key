;;; leader-key.el --- Leader key configuration for god-mode

;; Copyright (C) 2022 Łukasz Pawelczyk

;; Author: Łukasz Pawelczyk <havner@gmail.com>
;; Maintainer: Łukasz Pawelczyk <havner@gmail.com
;; URL: https://github.com/havner/leader-key
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

;; Simple helper to add leader key functionality (e.g. for god-mode)

;; The idea is to replicate a vim/evil leader key concept, but be independant of
;; any other mode. This has been developed and tested together with god-mode but
;; should work without it or even without any modal mode. It works together with
;; which-key, but the dependency is optional.

;; With the configuration below we can do shortcuts like:

;; SPC x q       ;; quits emacs
;; SPC p p       ;; opens projectile project list
;; SPC <left>    ;; moves to the left window
;; SPC m         ;; opens magit
;; SPC SPC       ;; starts selection
;; SPC x SPC     ;; starts rectangle selection
;; SPC q c       ;; (vdiff)-close-fold, only when major vdiff-mode active
;; SPC q c       ;; (rust-mode)-rust-compile, only when major rust-mode active

;; To configure (an example with god-mode):

;; (require 'which-key)                     ;; optional
;; (require 'leader-key)                    ;; only in case of no auto-loads

;; Define the main/root leader key that will trigger the leader map (make sure
;; the key is not bound to anything in situations we want it to trigger, in this
;; case when god-mode is active):

;; (setq leader-key-root-key "SPC")         ;; redundant, "SPC" is default
;; (with-eval-after-load 'god-mode
;;   (define-key god-local-mode-map (kbd leader-key-root-key) nil))

;; Configure the predicament that must be true for the leader-key to be
;; triggered. Otherwise the key should fall back to its default function. In
;; case of god-mode the below is a usable choice (activate when god-mode is
;; active or the buffer is read only):

;; (defun +leader-key-pred ()
;;   (or buffer-read-only
;;       god-local-mode))
;; (setq leader-key-pred #'+leader-key-pred)

;; Turn on the global minor mode:

;; (leader-key-mode t)

;; There are some major-modes that don't play well with leader, e.g. modes that
;; are marked as read only yet we need the leader key to do its original
;; job. One of such modes is vterm-mode. To handle such cases add this mode to
;; exceptions:

;; (add-to-list 'leader-key-exempt-major-modes 'vterm-mode t)

;; There might also be some major-modes that can have our leader key already
;; mapped to something else and it conflicts with the leader-key in leader minor
;; map. In such a case there is a helper function that will add our leader key
;; directly to such a map. The second parameter for (leader-key-do-map) is
;; optional, it will basically do (eval-after-load 2ND-PARAM) for that
;; expression so we don't need to handle the loading order.

;; (leader-key-do-map 'custom-mode-map 'cus-edit)
;; (leader-key-do-map 'magit-blame-read-only-mode-map 'magit-blame)

;; There is no default for the map that will be triggered with leader-key, we
;; need to build it ourselves. We can add whole submaps or just specific
;; functions. Examples below:

;; (with-eval-after-load 'projectile
;;   (leader-key-define-key "p" projectile-command-map "projectile"))
;; (with-eval-after-load 'flycheck
;;   (leader-key-define-key "f" flycheck-command-map "flycheck"))

;; (leader-key-define-key "m" #'magit-status)
;; (leader-key-define-key "g" #'magit-file-dispatch)

;; (leader-key-define-key "<left>" #'window-jump-left)
;; (leader-key-define-key "<right>" #'window-jump-right)
;; (leader-key-define-key "<up>" #'window-jump-up)
;; (leader-key-define-key "<down>" #'window-jump-down)
;; (leader-key-define-key "0" #'+switch-window-then-delete-window-and-balance "delete-window")
;; (leader-key-define-key "1" #'switch-window-then-maximize "maximize")
;; (leader-key-define-key "2" #'+switch-window-then-split-below-switch-and-balance "split-below")
;; (leader-key-define-key "3" #'+switch-window-then-split-right-switch-and-balance "split-right")

;; (leader-key-define-key "SPC" #'set-mark-command)

;; (leader-key-describe-key "x" "emacs")       ;; optional, for which-key
;; (leader-key-define-key "x SPC" #'rectangle-mark-mode)
;; (leader-key-define-key "x ;" #'eval-expression)
;; (leader-key-define-key "x e" #'eval-last-sexp)
;; (leader-key-define-key "x p" #'paradox-list-packages)
;; (leader-key-define-key "x q" #'save-buffers-kill-terminal)
;; (leader-key-define-key "x s" #'save-some-buffers)
;; (leader-key-define-key "x w" #'write-file)
;; (leader-key-define-key "x x" #'execute-extended-command)

;; We can also have a sub map assigned that will be different dependent on the
;; currently active major mode. This requires key and specific map assignment.

;; (setq leader-key-major-mode-key "q")        ;; redundant, "q" is default

;; The first parameter for (leader-key-major-mode-map) defines the major mode
;; map where our leader will be active (basically the major mode map assigned to
;; major mode), the second parameter is the map we want to have assigned under
;; leader key (might be provided externally, some packages provide usable maps,
;; might be built manually), the third parameter is optional, it basically does
;; (eval-after-load 3RD-PARAM) so we don't need to bother with loading order.

;; (leader-key-major-mode-map 'vdiff-mode-map 'vdiff-mode-prefix-map 'vdiff)

;; (setq rust-mode-map (make-sparse-keymap))   ;; optional, to clean up unusable default
;; (defvar rust-command-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "d") #'rust-dbg-wrap-or-unwrap)
;;     (define-key map (kbd "c") #'rust-compile)
;;     (define-key map (kbd "k") #'rust-check)
;;     (define-key map (kbd "t") #'rust-test)
;;     (define-key map (kbd "r") #'rust-run)
;;     (define-key map (kbd "l") #'rust-run-clippy)
;;     (define-key map (kbd "f") #'rust-format-buffer)
;;     (define-key map (kbd "n") #'rust-goto-format-problem)
;;     map))
;; (leader-key-major-mode-map 'rust-mode-map 'rust-command-map 'rust-mode)



;;; Code:

(defgroup leader-key nil
  "Leader key configuration.")

(defcustom leader-key-root-key
  "SPC"
  "A shortcut for the `leader-key-map'.

Has to be `kbd' compatible string."
  :group 'leader-key
  :type 'string)

(defcustom leader-key-major-mode-key
  "q"
  "A sub shortcut for the major mode map within `leader-key-map'.

Has to be `kbd' compatible string."
  :group 'leader-key
  :type 'string)

(defcustom leader-key-root-description
  "<leader>"
  "A leader root description visible when using `which-key'"
  :group 'leader-key
  :type 'string)

(defcustom leader-key-major-mode-description
  "<major>"
  "A major mode description visible when using `which-key'"
  :group 'leader-key
  :type 'string)

(defcustom leader-key-exempt-major-modes
  '(term-mode)
  "List of major modes that should have `leader-key-root-key' disabled.

This is for some strange modes that are e.g. marked as read-only,
while in reality they are not, e.g. terminal modes."
  :group 'leader-key
  :type '(repeat symbol))

(defcustom leader-key-pred
  'leader-key--default-pred
  "A function that defines when to activate `leader-key-map'
after pressing `leader-key-root-key'.

Most likely this will have to be redefined by the user as the default
is not very usable. Please refer to the package readme."
  :group 'leader-key
  :type 'function)

(defvar leader-key-map (make-sparse-keymap)
  "Map containing all leader bindings. Conditionally bound under
`leader-key-root-key' within `leader-key-mode-map'.")

(defvar leader-key-mode-map (make-sparse-keymap)
  "Map for the `leader-key-mode'. Its only purpose is to conditionally
enable `leader-key-map' after pressing `leader-key-root-key'.")

;;;###autoload
(define-minor-mode leader-key-mode
  "Leader key mode.

Enables `leader-key-mode-map' that has one binding: `leader-key-root-key'
that conditionally triggers `leader-key-map'. The condition is configurable
through `leader-key-pred' and `leader-key-exempt-major-modes'."
  :global t
  :group 'leader-key
  (define-key leader-key-mode-map (kbd leader-key-root-key)
    `(menu-item "" ,leader-key-map :filter leader-key--check-pred))
  (when (fboundp 'which-key-add-key-based-replacements)
    (which-key-add-key-based-replacements
      leader-key-root-key
      leader-key-root-description)
    (which-key-add-key-based-replacements
      (concat leader-key-root-key " " leader-key-major-mode-key)
      leader-key-major-mode-description)))

;;; private functions

(defun leader-key--default-pred ()
  "Default predicate that controls whether `leader-key-root-key' is triggered."
  buffer-read-only)

(defun leader-key--exempt-mode-pred ()
  "Return non-nil if `major-mode' is exempt from using leader-key."
  (memq major-mode leader-key-exempt-major-modes))

(defun leader-key--check-pred (cmd)
  "Final filtering function for the `leader-key-map'.

See `leader-key-pred' and `leader-key-exempt-major-modes'."
  (if (leader-key--exempt-mode-pred) nil
    (if (funcall leader-key-pred) cmd)))

;;; public functions

;;;###autoload
(defun leader-key-do-map (map &optional module)
  "Setup `leader-key-map' in specific major-mode map.

Use for some hybrid major-modes that are partially writeable and
overload `leader-key-root-key'. E.g. `Custom-mode'."
  (if module
      (eval-after-load module
        `(define-key ,map (kbd leader-key-root-key) leader-key-map))
    (define-key (eval map) (kbd leader-key-root-key) leader-key-map)))

;;;###autoload
(defun leader-key-define-key (key fun &optional description)
  "Add a new key to the `leader-key-map'. KEY is passed to `kbd'.

This is the main function to build your own `leader-key-map'."
  (let ((k (kbd key))
        (s (concat leader-key-root-key " " key)))
    (define-key leader-key-map k fun)
    (when (and description (fboundp 'which-key-add-key-based-replacements))
      (which-key-add-key-based-replacements s description))))

;;;###autoload
(defun leader-key-describe-key (key description)
  "Add DESCRIPTION to specific KEY for `which-key'.

This is useful to give names to key prefixes within `leader-key-map'."
  (let ((s (concat leader-key-root-key " " key)))
    (when (fboundp 'which-key-add-key-based-replacements)
      (which-key-add-key-based-replacements s description))))

;;;###autoload
(defun leader-key-major-mode-map (map commands &optional module)
  "Add major mode command map under usable with `leader-key-major-mode-key'.

This function allows to have a specific leader prefix (by default 'SPC q')
that shows a different command map depending on active major-mode.

MAP is major-mode map where the prefix will be active.
COMMANDS is command map that will show under the prefix."
  (let ((prefix-map-name (intern (concat "leader-key--" (symbol-name commands)))))
    (eval `(defvar ,prefix-map-name (make-sparse-keymap)))
    (if module
        (eval-after-load module
          `(progn
             (define-key ,prefix-map-name (kbd leader-key-major-mode-key) ,commands)
             (define-key ,map (kbd leader-key-root-key)
               '(menu-item "" ,(eval prefix-map-name) :filter leader-key--check-pred))))
      (progn
        (define-key (eval prefix-map-name) (kbd leader-key-major-mode-key) (eval commands))
        (define-key (eval map) (kbd leader-key-root-key)
          `(menu-item "" ,(eval prefix-map-name) :filter leader-key--check-pred))))))
