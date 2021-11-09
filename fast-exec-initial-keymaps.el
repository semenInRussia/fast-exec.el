;;; fast-exec-initial-keymaps --- Additional to fast-exec, define basic "keymaps"
;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Packages-Requires: ((fast-exec "0.0.1"))

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
;; Main function of this package is `fast-exec-define-standard-keys`.
;; For using this package, use folowed code of Emacs-Lisp:
;; ```
;; (fast-exec-define-standard-keys)
;; ```
;;; Code:

(require 'fast-exec)



(defun *delete-whole-file-content* ()
    "Delete whole content of current open file/buffer."
    (interactive)
    (kill-region (point-min) (point-max))
    )


(defun *delete-current-file* (delete-p)
    "If `DELETE-P`, Delete current file from FS and close its."
    (interactive (let ((bname (or (buffer-file-name) "unitled")))
                     (list
                      (y-or-n-p (s-lex-format "Delete file ${bname} ? " )))))

    (when delete-p
        (delete-file (buffer-name))
        (kill-buffer (current-buffer)))
    )


(defun *enable-whitespace-mode* ()
    "Enable `whitespace` mode."
    (interactive)
    (whitespace-mode 38)
    )


(defun *disable-whitespace-mode* ()
    "Disable `whitespace` mode."
    (interactive)
    (whitespace-mode 0)
    )


(defun *rename-current-file* (new-name)
    "Rename current open file to `NEW-NAME`."
    (interactive "F")
    (rename-file (buffer-file-name)
                 new-name)
    )


(defun *indent-current-file* ()
    "Indent all content of current file."
    (interactive)
    (indent-region-line-by-line (point-min) (point-max))
    )


(defun *delete-blank-lines* ()
    "Delete all newline around cursor.
AUTHOR: XahLee http://xahlee.info"
    (let (p1 p2)
        (skip-chars-backward "\n")
        (setq p1 (point))
        (skip-chars-forward "\n")
        (setq p2 (point))
        (delete-region p1 p2)))


(defun fast-exec/define-standard-keys ()
    "Define some useful \"keymaps\" for `fast-exec.el`."
    (interactive)
    "Define some useful \"keys\" for `fast-exec`."
    
    (setq fast-exec/full-commands nil)
    (setq fast-exec/commands-and-names nil)

    (fast-exec/add-some-commands
     ("Revert Buffer" revert-buffer)
     ("Enable Auto Revert Mode" auto-revert-mode)
     ("Save All Files" save-some-buffers)
     ("Delete the Whole File's Content" *delete-whole-file-content*)
     ("Delete Current File" *delete-current-file*)
     ("Rename Current File" *rename-current-file*)
     ("Indent Current File" *indent-current-file*)
     ("Indent Selected Region" indent-region)
     ("Find and Replace String" replace-string)
     ("Replace Regexp" replace-regexp)
     ("Enable Whitespace Mode" *enable-whitespace-mode*)
     ("Disable Whitespace Mode" *disable-whitespace-mode*)
     ("Run Eshell" eshell)
     ("Delete Blank Lines" *delete-blank-lines*)
     ("Delete Duplicated Lines" delete-duplicate-lines)
     ("Delete Lines Contains" delete-matching-lines)
     ("Delete Lines Not Contains" delete-non-matching-lines)))


(provide 'fast-exec-initial-keymaps)
;;; fast-exec-initial-keymaps.el ends here