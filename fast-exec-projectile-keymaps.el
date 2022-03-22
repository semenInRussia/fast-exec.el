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

(defun fast-exec/define-projectile-keys ()
    "Define some useful \"keymaps\" for projectile and `fast-exec.el`."
    (-concat
     (fast-exec/some-commands
      ("Projectile Switch Project"
       (*projectile-or-helm-projectile-fun* 'switch-project))
      ("Projectile Test Current Project"
       (*projectile-or-helm-projectile-fun* 'test-project))
      ("Projectile Find File in Current Directory"
       (*projectile-or-helm-projectile-fun* 'find-file))
      ("Projectile Search String"
       (*projectile-or-helm-projectile-fun* 'grep))
      ("Projectile Open Version Controle"
       (*projectile-or-helm-projectile-fun* 'vc))
      ("Projectile Open Dired"
       (*projectile-or-helm-projectile-fun* 'dired))
      ("Projectile Open Eshell"
       (*projectile-or-helm-projectile-fun* 'run-eshell))
      ("Projectile Search and Replace String"
       (*projectile-or-helm-projectile-fun* 'replace))
      ("Projectile Search and Replace Regular Expression"
       (*projectile-or-helm-projectile-fun* 'replace-regexp))
      ("Projectile Complie Project"
       (*projectile-or-helm-projectile-fun* 'compile-project))
      ("Projectile Discover Projects in Search Path"
       'projectile-discover-projects-in-search-path)
      ("Projectile Discover Projects in Current Directory"
       '*projectile-discover-projects-in-current-directory*)
      ("Projectile Edit Dir Locale" 'projectile-edit-dir-locals)
      ("Projectile Add Known Project" 'projectile-add-known-project)
      ("Projectile To Implementation or Test"
       'projectile-toggle-between-implementation-and-test)
      ("Projectile Run Install Commnad" 'projectile-install-project)
      ("Projectile Close All Files of Project" 'projectile-kill-buffers)
      ("Projectile Run Command in Root" 'projectile-run-command-in-root)
      ("Projectile Run Project" 'projectile-run-project))
     (with-eval-after-load 'helm-projectile
         (fast-exec/some-commands
          ("Projectile Find File in Known Project"
           'helm-projectile-find-file-in-known-projects)))))


(defun *projectile-discover-projects-in-current-directory* ()
    "Discover projectile's projects in directory of current open file."
    (interactive)
    (projectile-discover-projects-in-directory default-directory))


(defun *projectile-or-helm-projectile-fun* (symb)
    "If has `helm-projectile-`+SYMB, then get its, else get `projectile-`+SYMB."
    (let* ((symb-name (symbol-name symb))
           (projectile-symb
            (intern (s-prepend "projectile-" symb-name)))
           (helm-projectile-symb
            (intern (s-prepend "helm-projectile-" symb-name))))
        (if (fboundp helm-projectile-symb)
            helm-projectile-symb
            projectile-symb)))


(provide 'fast-exec-projectile-keymaps)
;;; fast-exec-projectile-keymaps.el ends here
