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

(defun fast-exec-projectile-toggle-caching ()
  "Set `projectile-enable-caching' to opposite value."
  (interactive)
  (setq projectile-enable-caching (not projectile-enable-caching)))

(defun *projectile-discover-projects-in-current-directory* ()
  "Discover projectile's projects in directory of current open file."
  (interactive)
  (projectile-discover-projects-in-directory default-directory))

(defun *projectile-or-helm-projectile-fun* (symb)
  "If has `helm-projectile-`+SYMB, then get its, else get `projectile-`+SYMB."
  (let* ((symb-name (symbol-name symb))
         (projectile-symb (intern (s-prepend "projectile-" symb-name)))
         (helm-projectile-symb
          (intern (s-prepend "helm-projectile-" symb-name))))
    (if (fboundp helm-projectile-symb)
        helm-projectile-symb
      projectile-symb)))

(fast-exec-bind projectile
  (cons
   (with-eval-after-load 'helm-projectile
     (fast-exec-make-command
      "Projectile Find File in Known Project"
      'helm-projectile-find-file-in-known-projects))
   (list
    (fast-exec-make-command
     "Projectile Switch Project"
     (*projectile-or-helm-projectile-fun* 'switch-project))
    (fast-exec-make-command
     "Projectile Test Current Project"
     (*projectile-or-helm-projectile-fun* 'test-project))
    (fast-exec-make-command
     "Projectile Find File in Current Directory"
     (*projectile-or-helm-projectile-fun* 'find-file))
    (fast-exec-make-command
     "Projectile Search String"
     (*projectile-or-helm-projectile-fun* 'grep))
    (fast-exec-make-command
     "Projectile Open Version Controle"
     (*projectile-or-helm-projectile-fun* 'vc))
    (fast-exec-make-command
     "Projectile Open Dired"
     (*projectile-or-helm-projectile-fun* 'dired))
    (fast-exec-make-command
     "Projectile Open Eshell"
     (*projectile-or-helm-projectile-fun* 'run-eshell))
    (fast-exec-make-command
     "Projectile Search and Replace String"
     (*projectile-or-helm-projectile-fun* 'replace))
    (fast-exec-make-command
     "Projectile Search and Replace Regular Expression"
     (*projectile-or-helm-projectile-fun* 'replace-regexp))
    (fast-exec-make-command
     "Projectile Complie Project"
     (*projectile-or-helm-projectile-fun* 'compile-project))
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
     "Projectile Toggle Cache" 'fast-exec-projectile-toggle-caching))))

(provide 'fast-exec-projectile-keymaps)
;;; fast-exec-projectile-keymaps.el ends here
