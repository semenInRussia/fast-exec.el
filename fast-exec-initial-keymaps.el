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

;;; Code:

(require 'fast-exec)

(fast-exec-bind initial
  (fast-exec-make-some-commands
   ("Revert Buffer"              'revert-buffer)
   ("Enable Auto Revert Mode"    'auto-revert-mode)
   ("Save All Files"             'save-some-buffers)
   ("Delete the Whole File's Content" '*delete-whole-file-content*)
   ("Delete Current File"        '*delete-current-file*)
   ("Rename Current File"        '*rename-current-file*)
   ("Move Current File"          '*move-current-file*)
   ("Indent Current File"        '*indent-current-file*)
   ("Indent Selected Region"     'indent-region)
   ("Search and Replace Regexp"  '*map-query-replace-regexp-in-whole-buffer*)
   ("Enable Whitespace Mode"     '*enable-whitespace-mode*)
   ("Disable Whitespace Mode"    '*disable-whitespace-mode*)
   ("Delete Blank Lines"         '*delete-blank-lines*)
   ("Delete Something"           'fast-exec/delete-something)
   ("Delete Duplicated Lines"    'delete-duplicate-lines)
   ("Delete Lines Contains"      'delete-matching-lines)
   ("Delete Lines Not Contains"  'delete-non-matching-lines)
   ("Kill Current Buffer"        '*kill-current-buffer*)
   ("Transpose Words"            'transpose-words)
   ("Transpose Regions"          'transpose-regions)
   ("Transpose Lines"            'transpose-lines)
   ("Transpose Sexps"            'transpose-sexps)
   ("Convert Tabs to Spaces"     'untabify)
   ("Check Parens"               'check-parens)
   ("Align by Regular Expresion" 'align-regexp)
   ("Open Eshell"                'eshell)
   ("Open Ielm"                  'ielm)
   ("Open Calculator"            'calc)
   ("Unload Feature"             'unload-feature)
   ("Load Theme"                 'load-theme)
   ("Rename Current Buffer"      'rename-buffer)
   ("Toggle Truncate Lines"      'toggle-truncate-lines)
   ("Visual Line"                'visual-line-mode)
   ("Fast Exec Reload"       'fast-exec-reload)
   ("Open Regexp Builder"        'regexp-builder)
   ("Move Something"             'fast-exec/move-something)))

(defun *delete-whole-file-content* ()
  "Delete whole content of current open file/buffer."
  (interactive)
  (kill-region (point-min) (point-max)))

(defun *delete-current-file* (delete-p)
  "If `DELETE-P`, Delete current file from FS and close its."
  (interactive
   (let ((bname (buffer-file-name)))
     (list (y-or-n-p (s-lex-format "Delete file \"${bname}\" ? ")))))
  (when delete-p
    (delete-file (buffer-file-name))
    (kill-buffer (current-buffer))))

(defun *kill-current-buffer* ()
  "Kill current open buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun *enable-whitespace-mode* ()
  "Enable `whitespace` mode."
  (interactive)
  (whitespace-mode 38))

(defun *disable-whitespace-mode* ()
  "Disable `whitespace` mode."
  (interactive)
  (whitespace-mode 0))

(defun *rename-current-file* (new-name)
  "Rename current open file to NEW-NAME."
  (interactive
   (list
    (read-string "New name, please: "
                 (f-filename (buffer-file-name)))))
  (let ((pos (point))
        (dest (f-join default-directory new-name))
        (source (buffer-file-name)))
    (save-buffer)
    (rename-file source dest)
    (kill-buffer)
    (find-file dest)
    (goto-char pos)))

(defun *move-current-file* (destination-dir)
  "Move current open file to DESTINATION-DIR."
  (interactive "D")
  (let ((pos (point))
        (source-file (buffer-file-name))
        (destination-file (f-join destination-dir (buffer-name))))
    (save-buffer)
    (rename-file source-file destination-file)
    (kill-buffer)
    (find-file destination-file)
    (goto-char pos)))

(defun fast-exec/move-something (source destination)
  "Move SOURCE to DESTINATION, source may be wildcard regexp.
Examples of SOURCE: *.docx, cool.tex"
  (interactive
   (list
    (read-string "Enter source: " (buffer-file-name))
    (read-directory-name "Enter destination: ")))
  (let* ((sources (file-expand-wildcards source)))
    (--each sources
      (rename-file it destination)
      (message "File %s moved!" (f-filename it)))))

(defun fast-exec/delete-something (files-regexp)
  "Move files which match with FILES-REGEXP - wildcard regexp.
Examples of FILES-REGEXP: *.docx, cool.tex"
  (interactive "sWhat delete?: ")
  (let* ((files (file-expand-wildcards files-regexp)))
    (--each files
      (f-delete it)
      (message "File %s deleted!" (f-filename it)))))

(defun *indent-current-file* ()
  "Indent all content of current file."
  (interactive)
  (indent-region-line-by-line (point-min) (point-max)))

(defun *delete-blank-lines* ()
  "Delete all newline around cursor.
AUTHOR: XahLee http://xahlee.info"
  (let (p1 p2)
    (skip-chars-backward "\n")
    (setq p1 (point))
    (skip-chars-forward "\n")
    (setq p2 (point))
    (delete-region p1 p2)))

(defun *map-query-replace-regexp-in-whole-buffer* ()
  "Replace some match for `regexp` with various strings, in current buffer.
This command works like `query-replace-regexp' except that
each successive replacement uses the next successive replacement string,
wrapping around from the last such string to the first."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (call-interactively 'map-query-replace-regexp)))

(provide 'fast-exec-initial-keymaps)
;;; fast-exec-initial-keymaps.el ends here
