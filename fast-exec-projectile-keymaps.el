;;; fast-exec-projectile-keymaps --- Support of projectile for "fast-exec"

;; Copyright (C) 2021 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package define some useful "keymaps" for `fast-exec.el`.
;; Main function of this package is `fast-exec-define/projectile-keys`.
;; For using this package, use folowed code of Emacs-Lisp:
;; ```
;; (fast-exec/register-keymap-func 'fast-exec/define-projectile-keys)
;; ```
;;; Code:

;; (eval-and-compile (require 'fast-exec))

(defun fast-exec-projectile-toggle-caching ()
  "Set `projectile-enable-caching' to opposite value."
  (interactive)
  (setq projectile-enable-caching (not projectile-enable-caching)))

(defun *projectile-discover-projects-in-current-directory* ()
  "Discover projectile's projects in directory of current open file."
  (interactive)
  (projectile-discover-projects-in-directory default-directory))

(defun fast-exec-projectile-command (symb)
  "Use on of the consult-, helm- or non prefix for command at SYMB."
  (or
   (fast-exec-projectile--bound-with-prefix-p 'helm- symb)
   (fast-exec-projectile--bound-with-prefix-p 'consult- symb)
   symb))

(defun fast-exec-projectile--bound-with-prefix-p (prefix symb)
  "Get command symbol if SYMB with the PREFIX is not void function definition."
  (let ((command (intern
                  (concat
                   (symbol-name prefix)
                   (symbol-name symb)))))
    (and
     (fboundp command)
     command)))

(fast-exec-bind 'projectile
  (list
   (fast-exec-make-command
    "Projectile Switch Project"
    (fast-exec-projectile-command 'projectile-switch-project))
   (fast-exec-make-command
    "Projectile Find File in Known Project"
    (fast-exec-projectile-command 'projectile-find-file-in-known-projects))
   (fast-exec-make-command
    "Projectile Test Current Project"
    (fast-exec-projectile-command 'projectile-test-project))
   (fast-exec-make-command
    "Projectile Find File in Current Directory"
    (fast-exec-projectile-command 'projectile-find-file))
   (fast-exec-make-command
    "Projectile Search String"
    (fast-exec-projectile-command 'projectile-grep))
   (fast-exec-make-command
    "Projectile Open Version Controle"
    (fast-exec-projectile-command 'projectile-vc))
   (fast-exec-make-command
    "Projectile Open Dired"
    (fast-exec-projectile-command 'projectile-dired))
   (fast-exec-make-command
    "Projectile Open Eshell"
    (fast-exec-projectile-command 'projectile-run-eshell))
   (fast-exec-make-command
    "Projectile Search and Replace String"
    (fast-exec-projectile-command 'projectile-replace))
   (fast-exec-make-command
    "Projectile Search and Replace Regular Expression"
    (fast-exec-projectile-command 'projectile-replace-regexp))
   (fast-exec-make-command
    "Projectile Complie Project"
    (fast-exec-projectile-command 'projectile-compile-project))
   (fast-exec-make-command
    "Projectile Discover Projects in Search Path"
    'projectile-discover-projects-in-search-path)
   (fast-exec-make-command
    "Projectile Discover Projects in Current Directory"
    '*projectile-discover-projects-in-current-directory*)
   (fast-exec-make-command
    "Projectile Edit Dir Locale" 'projectile-edit-dir-locals)
   (fast-exec-make-command
    "Projectile Add Known Project" 'projectile-add-known-project)
   (fast-exec-make-command
    "Projectile To Implementation or Test"
    'projectile-toggle-between-implementation-and-test)
   (fast-exec-make-command
    "Projectile Run Install Commnad" 'projectile-install-project)
   (fast-exec-make-command
    "Projectile Close All Files of Project" 'projectile-kill-buffers)
   (fast-exec-make-command
    "Projectile Run Command in Root" 'projectile-run-command-in-root)
   (fast-exec-make-command
    "Projectile Run Project" 'projectile-run-project)
   (fast-exec-make-command
    "Projectile Toggle Cache" 'fast-exec-projectile-toggle-caching)))

(provide 'fast-exec-projectile-keymaps)
;;; fast-exec-projectile-keymaps.el ends here
