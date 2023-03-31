;;; fast-exec.el --- Fast execution of the commands -*- lexical-binding: t; -*-

;; Copyright (C) 2022 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.18.0") (s "1.12.0"))
;; Homepage: https://github.com/semeninrussia/fast-exec.el

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

;; Fast execution of the commands.

;;; Code:

(require 'dash)
(require 's)

(require 'cl-lib)

(defgroup fast-exec nil
  "Fast execution of the commands."
  :group 'tools)

(defvar fast-exec--commands-bindings
  (make-hash-table :test 'eq)
  "List of the functions which return list of `fast-exec-command' objects.")

(defvar fast-exec-commands nil
  "Instances of the `fast-exec-command' which will be handled at start.")

(defcustom fast-exec-buffer-parent-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "0" 'fast-exec-hide-commands-buffer)
    map)
  "Keymap for buffer of `fast-exec-command' search."
  :type 'keymap
  :group 'fast-exec)

(defface fast-exec-char-face
  '((t (:foreground "#6495ed")))        ; CornflowerBlue
  "Face for char indicates a key for call command in `fast-exec' buffer.")

(defclass fast-exec-command ()
  ((words :initarg :words :accessor fast-exec-command-words)
   (chars :initarg :chars :accessor fast-exec-command-chars)
   (command :initarg :command :accessor fast-exec-command-command))
  "A command for the `fast-exec'.")

(defun fast-exec-make-command (name command)
  "Make a instance of the `fast-exec-command'.

NAME will be splitted into words and chars and passed to the `fast-exec-command'
function.  Pass COMMAND to the `fast-exec-command' function"
  (let* ((words
          (->>                           ;nofmt
           name
           (s-split-words)
           (-partition-before-pred 'fast-exec--first-upcase-char-p)
           (--map (s-join " " it))))
         (chars (-map 'string-to-char words)))
    (fast-exec-command :words words :chars chars :command command)))

(defmacro fast-exec-make-some-commands (&rest bindings)
  "Make some instances of the `fast-exec-command'.

Each of BINDINGS is list from the command description as `car', and command
symbol as an second item"
  (cons
   'list
   (--map
    `(fast-exec-make-command ,(car it) ,(-second-item it))
    bindings)))

(defun fast-exec--first-upcase-char-p (s)
  "Return non-nil, when first char of the S is upcased."
  (let ((first-char (string-to-char s)))
    (= (upcase first-char) first-char)))

(cl-defmethod fast-exec-command-char ((self fast-exec-command)
                                      &optional n)
  "Get N -th char of an `fast-exec-command' instance SELF."
  (or n (setq n 0))
  (downcase (nth n (fast-exec-command-chars self))))

(cl-defmethod fast-exec-command-key ((self fast-exec-command)
                                     &optional n)
  "Get N -th keybinding of an `fast-exec-command' instance SELF."
  (or n (setq n 0))
  (char-to-string (fast-exec-command-char self n)))

(cl-defmethod fast-exec-command-word ((self fast-exec-command)
                                      &optional n)
  "Get N -th word of an `fast-exec-command' instance SELF."
  (or n (setq n 0))
  (nth n (fast-exec-command-words self)))

(cl-defmethod fast-exec-command-rest-words ((self fast-exec-command)
                                            &optional n)
  "Get rest words of SELF `fast-exec-command' name for step N."
  (or n (setq n 0))
  (-slice (fast-exec-command-words self) n))

(cl-defmethod fast-exec-command-call ((self fast-exec-command))
  "Call command of the SELF (a `fast-exec-command' instance)."
  (call-interactively (fast-exec-command-command self)))

(defun fast-exec-exec ()
  "Main command of the `fast-exec'."
  (interactive)
  (fast-exec-reload)
  (fast-exec--exec-from-step))

(defun fast-exec--exec-from-step (&optional commands step)
  "Find `fast-exec' command in COMMANDS, start search from STEP.

STEP defaults to zero, COMMANDS defaults to value of function
`fast-exec-commands' call"
  (or commands (setq commands fast-exec-commands))
  (setq commands (-non-nil commands))
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap fast-exec-buffer-parent-keymap)
    (--each commands                    ;nofmt
      (define-key keymap
        (fast-exec-command-key it step)
        (lambda ()
          (interactive)
          (fast-exec-handle-nth-char commands step))))
    (define-key keymap [t]
      (lambda ()
        (interactive)
        (fast-exec--message-key-is-undefined)
        (fast-exec--exec-from-step commands step)))
    (fast-exec-view-commands-in-buffer commands step)
    (set-transient-map keymap)))

(defun fast-exec-handle-nth-char (commands &optional n char)
  "Limit COMMANDS by N -th CHAR, if has 1 command run, else run main command.

CHAR defaults to last pressed character, N defaults to zero."
  (or n (setq n 0))
  (let ((commands
         (fast-exec-filter-commands-by-nth-char commands n char)))
    (if (not (= (length commands) 1))
        (fast-exec--exec-from-step commands (1+ n))
      (fast-exec-hide-commands-buffer)
      (message "Evaluate `%s'"
               (fast-exec-command-command (car commands)))
      (fast-exec-command-call (car commands)))))

(defun fast-exec-filter-commands-by-nth-char (commands &optional n char)
  "Filter the `fast-exec' COMMANDS by having N -th character equal to CHAR.

N defaults to 0, CHAR defaults to last pressed character."
  (or char (setq char (event-basic-type last-input-event)))
  (--filter (= (fast-exec-command-char it n) char) commands))

(defun fast-exec--message-key-is-undefined ()
  "Message in the minibuffer, that function at last pressed key is undefined."
  (message "Key `%c' is undefined."
           (event-basic-type last-input-event)))

(defun fast-exec-hide-commands-buffer ()
  "Hide hints for `fast-exec-command' instances with in special buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*fast-exec*")))
    (kill-buffer buffer)))

(defun fast-exec-view-commands-in-buffer (commands step)
  "View hints for `fast-exec-command' COMMANDS with STEP in special buffer."
  (let ((buffer (get-buffer-create "*fast-exec*")))
    (with-current-buffer buffer
      (erase-buffer)
      (toggle-truncate-lines t)
      (--each
          (--group-by (fast-exec-command-char it step) commands)
        (fast-exec-view-commands-char (car it) (cdr it) step)
        (newline))
      (align-regexp
       (point-min)
       (point-max)
       "\\(.\\)(" 1))
    (pop-to-buffer buffer '((display-buffer-same-window)))))

(defun fast-exec-view-commands-char (char commands &optional step)
  "View a group of `fast-exec-command' COMMANDS grouped by N -th CHAR.

STEP defaults to zero"
  (or step (setq step 1))
  (let ((prev-words
         (delete-dups
          (--map (fast-exec-command-word it (1- step)) commands)))
        (next-words
         (-non-nil
          (delete-dups
           (--map (or (fast-exec-command-word it step) "") commands)))))
    (insert
     (s-join " | " prev-words)
     "  ("
     (propertize (format "%c" char) 'face 'fast-exec-char-face)
     ")  "
     (s-join " | " next-words))))

(defmacro fast-exec-bind (id &rest body)
  "Bind `fast-exec-commands' evalueted from BODY with a symbol ID with quote."
  (declare (indent 1))
  (interactive)
  `(progn (puthash ,id (progn ,@body) fast-exec--commands-bindings)))

(defmacro fast-exec-unbind (id)
  "Remove binding of `fast-exec-command' at a symbol (unquoted) ID."
  (interactive)
  `(remhash ',id fast-exec--commands-bindings))

(defun fast-exec-reload ()
  "Reload `fast-exec-commands' depends on `fast-exec--keymap-functions'.

See `fast-exec-register-function' for change `fast-exec--keymap-functions'"
  (interactive)
  (setq fast-exec-commands nil)
  (maphash
   (lambda (_ v)
     (setq fast-exec-commands (append fast-exec-commands v)))
   fast-exec--commands-bindings))

(defun fast-exec-use-builtin-support (module)
  "Use built-in support of MODULE for `fast-exec'."
  (require
   (intern (s-concat "fast-exec-" (symbol-name module) "-keymaps"))))

(defmacro fast-exec-use (&rest modules)
  "Use built-in support each of MODULES for `fast-exec'."
  (cons 'progn (--map `(fast-exec-use-builtin-support ',it) modules)))

(provide 'fast-exec)
;;; fast-exec.el ends here
