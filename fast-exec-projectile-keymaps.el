;;; fast-exec-projectile-keymaps --- Support of projectile for "fast-exec"
;; Copyright (C) 2021 Free Software Foundation, Inc.

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
    (interactive)

    (fast-exec/some-commands
     ("Projectile Switch Project" projectile-switch-project)
     ("Projectile Test Current Project" projectile-test-project)
     ("Projectile Find File" projectile-find-file)
     ("Projectile Search String" projectile-grep)
     ("Projectile Open Version Controle" projectile-vc)
     ("Projectile Open Dired" projectile-dired)
     ("Projectile Search and Replace String" projectile-replace)
     ("Projectile Search and Replace Regular Expression"
      projectile-replace-regexp)
     ("Projectile Complie Project"
      project-compile-project)
     ("Projectile Discover Projects in Search Path"
      projectile-discover-projects-in-search-path)
     ("Projectile Discover Projects in Current Directory"
      *projectile-discover-projects-in-current-directory*)
     ("Projectile Edit Dir Locale" projectile-edit-dir-locals)
     ("Projectile To Implementation or Test"
      projectile-toggle-between-implementation-and-test)
     ("Projectile Run Install Commnad" projectile-install-project)
     ("Projectile Close All Files of Project" projectile-kill-buffers)
     ("Projectile Run Command in Root" projectile-run-command-in-root)
     ("Projectile Run Project" projectile-run-project)
     )
    )


(defun *projectile-discover-projects-in-current-directory* ()
    "Discover projectile's projects in directory of current open file."
    (interactive)
    (projectile-discover-projects-in-directory default-directory)
    )


(provide 'fast-exec-projectile-keymaps)
;;; fast-exec-projectile-keymaps.el ends here
