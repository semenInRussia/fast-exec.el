;;; fast-exec-yasnippet-keymaps --- Additional to fast-exec, define "keymaps" for flycheck
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
;; Main function of this package is `fast-exec-/define-flycheck-keys`.
;; For using this package, use folowed code of Emacs-Lisp:
;; ```
;; (fast-exec/register-keymap-func 'fast-exec/define-flycheck-keys)
;; ```
;;; Code:

(fast-exec-bind flycheck
  (fast-exec-make-some-commands
   ("Enable Flycheck" '*turn-on-flycheck*)
   ("Disable Flycheck" '*turn-off-flycheck*)
   ("Flycheck View List of Errors" 'flycheck-list-errors)
   ("Flycheck Disable Checker" 'flycheck-disable-checker)
   ("Flycheck Select Checker" 'flycheck-select-checker)
   ("Flycheck Select Indication Mode" 'flycheck-set-indication-mode)
   ("Flycheck Verify Checker" 'flycheck-verify-checker)
   ("Flycheck Verify Setup" 'flycheck-verify-setup)))

(defun *turn-on-flycheck* ()
  "Turn On `flycheck` mode."
  (interactive)
  (flycheck-mode 38))

(defun *turn-off-flycheck* ()
  "Turn off `flycheck` mode."
  (interactive)
  (flycheck-mode 0))

(provide 'fast-exec-flycheck-keymaps)
;;; fast-exec-flycheck-keymaps.el ends here
