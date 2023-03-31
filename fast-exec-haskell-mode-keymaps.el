;;; fast-exec-haskell-mode-keymaps --- Builtin fast-exec, "keymaps" for haskell-mode
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
;; Main function of this package is `fast-exec/define-haskell-mode-keys`.
;; For using this package, use folowed code of Emacs-Lisp:
;; ```
;; (fast-exec/register-keymap-func 'fast-exec/define-projectile-keys)
;; ```
;; or
;; ```
;; (fast-exec/enable-builtin-support haskell-mode)
;; ```
;;; Code:

(fast-exec-bind 'haskell-mode
  (fast-exec-make-some-commands
   ("Haskell Align Imports" 'haskell-align-imports)
   ("Haskell Sort Imports" 'haskell-sort-imports)
   ("Haskell Sort and Align Imports" '*haskell-align-and-sort-imports*)
   ("Haskell Complie Program" 'haskell-compile)
   ("Enable Haskell Documentation Mode" 'haskell-doc-mode)
   ("Haskell Hoogle Lookup from Local" 'haskell-hoogle-lookup-from-local)
   ("Haskell Hoogle Lookup from Web Site" 'haskell-hoogle-lookup-from-website)
   ("Open Haskell REPL" 'run-haskell)
   ("Open Haskell Menu" 'haskell-menu)
   ("Haskell Change Session" 'haskell-session-change)
   ("Haskell Kill Session" 'haskell-session-kill)
   ("Haskell Show Type" 'haskell-mode-show-type-at)
   ("Haskell Go To Imports" 'haskell-navigate-imports)))

(defun *haskell-align-and-sort-imports* ()
  "Composition of `haskell-sort-imports' and `haskell-align-imports'."
  (interactive)
  (haskell-align-imports)
  (haskell-sort-imports))

(provide 'fast-exec-haskell-mode-keymaps)
;;; fast-exec-haskell-mode-keymaps.el ends here
