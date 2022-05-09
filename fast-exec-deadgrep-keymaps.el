;;; fast-exec-deadgrep-keymaps.el --- Additional to fast-exec, define "keymaps" for Deadgrep
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
;; Main function of this package is `fast-exec/define-deadgrep-keys`.
;; For using this package, use folowed code of Emacs-Lisp:
;; ```
;; (fast-exec/register-keymap-func 'fast-exec/define-deadgrep-keys)
;; ```
;;; Code:


(defun fast-exec/define-deadgrep-keys ()
    "Define some useful \"keymaps\" for `deadgrep' and `fast-exec'."
    (fast-exec/some-commands
     ("Search in Some Files" 'deadgrep)))


(provide 'fast-exec-deadgrep-keymaps)
;;; fast-exec-deadgrep-keymaps.el ends here
