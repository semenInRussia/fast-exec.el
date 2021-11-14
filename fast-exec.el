;;; fast-exec --- Very Fast Executing Emacs Commands

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Packages-Requires: ((dash "2.18.0")
;;                     (s     "1.12.0"))

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

;;; `fast-exec` is package for very fast executing,
;;; for example, if you want call save-all-buffers, here your steps:
;;;    1. press prefix, for example M-x
;;;    2. press `s`         | This is Save
;;;    3. press `a`         | This is All
;;;    4. press `f`         | This is files
;;; 
;;; Code:

(require 'dash)
(require 's)

(require 'fast-exec-full-commands)


(defgroup fast-exec nil
    "Very Fast Execute Commands!."
    :group 'tools
    :link '(url-link :tag "GitHub" "https://github.com/semenInRussia/fast-M-x.el")
    )


(defcustom fast-exec/buffer-name "*fast-exec view candidates to run command*"
  "Name of buffer that created for asking user char of word of command."
  :group 'fast-exec
  :type 'string)


(defcustom fast-exec/keymap-function-chain nil
  "This is list of function, which take nothing, and return some `full-commands`."
  :group 'fast-exec
  :type '(repeat function))


(defcustom fast-exec/full-commands nil
  "This is list of FULL COMMANDS.
FULL COMMAND is command function and list of char for type and words for view."
  :group 'fast-exec
  :type '(repeat (command '(repeat char string))))


(defcustom fast-exec/keymap-prefix "M-a"
  "Fast-Exec keymap prefix."
  :group 'fast-exec
  :type 'string)


(defun fast-exec/*to-string-nth-char-and-full-commands* (char-and-full-commands n)
    "To string grouped by character of `N`-th word of name `full-commands`."

    (let* ((char-as-str (char-to-string (-first-item char-and-full-commands)))
           (full-commands (cdr char-and-full-commands))
           (nth-words-of-commands (fast-exec/nth-words-of-full-commands full-commands n))
           (unique-nth-words (delete-dups nth-words-of-commands))
           (joined-nth-words (fast-exec-str/join-strings " | " unique-nth-words)))
        (s-lex-format "${char-as-str}                                 ${joined-nth-words}"))
    )


(defun fast-exec/*to-string-groups-by-nth-char-full-commands* (groups n)
    "To string groups grouped by character of `N`-th word of name `full-commands`."
    
    (--map (fast-exec/*to-string-nth-char-and-full-commands* it n)
           groups)
    )


(defun fast-exec/*to-string-full-commands-as-candidates-with-nth-chars* (full-commands n)
    "To str `FULL-COMMANDS` as candidates for typing `N`-th char of commands' names.."
    (s-join "\n"
            (fast-exec/*to-string-groups-by-nth-char-full-commands*
             (--group-by (fast-exec/full-command-nth-char it n) full-commands)
             n))
    )


(defun fast-exec/*insert-full-commands-as-candidates-with-nth-chars* (full-commands n)
    "Insert `FULL-COMMANDS` as candidates for typing `N`-th char of commands' names.."
    (insert (fast-exec/*to-string-full-commands-as-candidates-with-nth-chars* full-commands n))
    )


(defun fast-exec/*view-full-commands-as-candidates-with-nth-chars-in-new-buffer*
    (full-commands n)
    "View `FULL-COMMANDS` as candidates with `N`-th character.
For executing in `fast-exec/exec` command."
    (let ((new-buffer (generate-new-buffer fast-exec/buffer-name))
          (inhibit-read-only t))
        (progn
            (switch-to-buffer new-buffer)
            (funcall 'fundamental-mode)
            (fast-exec/*insert-full-commands-as-candidates-with-nth-chars* full-commands n)))
    )


(defun fast-exec/*completing-read-full-command-nth-part* (prompt candidates n)
    "Read `full-command`'s `N`-th chars from user's minibufer from `CANDIDATES` with `PROMPT`."
    (setq fast-exec/*char-of-user* nil)
    (fast-exec/*view-full-commands-as-candidates-with-nth-chars-in-new-buffer* candidates n)
    
    (let ((candidates-chars (fast-exec/nth-chars-of-full-commands candidates n)))
        (while (not (-contains? candidates-chars fast-exec/*char-of-user*))
            (setq fast-exec/*char-of-user* (read-char))
            (kill-buffer fast-exec/buffer-name)))
    fast-exec/*char-of-user*)


(defun fast-exec/exec (&optional full-commands typed-chars-num char-of-command-word)
    "Execute command, command searching from `CHAR-OF-COMMAND-WORD`."
    (interactive)
    
    (setq full-commands (or full-commands fast-exec/full-commands))
    (setq typed-chars-num (or typed-chars-num 0))
    (setq char-of-command-word
          (fast-exec/*completing-read-full-command-nth-part*
           "Enter Character, Please (: " full-commands typed-chars-num))
    (let ((suitable-full-commands
           (fast-exec/full-commands-with-excepted-nth-char
            full-commands
            char-of-command-word
            typed-chars-num)))
        (pcase (length suitable-full-commands)
            ;; TODO: Add timeout
            (0 (message "Your command not found, we are back you to previous char!")) ;; TODO: Go back...
            (1 (message "Your command found! :) ... Executing")
               (fast-exec/call-first-full-command suitable-full-commands))
            (_ (fast-exec/exec suitable-full-commands (+ typed-chars-num 1)))
            )))


(defun fast-exec/register-keymap-func (func)
    "Add `FUNC` to `fast-exec`' func chain for defining keymaps to `fast-exec`.
`FUNC` is function, which taked nothing, and gives collection of
`full-commands`.  I am not use for this situation basic list, because
if user change mine list of keymaps, for valid updating
\"fast-exec/full-commands\", user must delete your old keymaps' version,
but if user use funcs and `fast-exec` use chain of functions, then after
updating any function `fast-exec/full-commands` set to nil, and all functions
 call again. Example:
`(fast-exec/register-keymap-func 'foo)`."
    (unless (-contains? fast-exec/keymap-function-chain func)
        (add-to-list 'fast-exec/keymap-function-chain func))
    )


(defmacro fast-exec/register-some-keymap-funcs (&rest funcs)
"Add all `FUNCS` to `fast-exec`' func chain for defining keymaps to `fast-exec`.
`FUNC` is function, which taked nothing, and gives collection of
`full-commands`.  I am not use for this situation basic list, because
if user change mine list of keymaps, for valid updating
\"fast-exec/full-commands\", user must delete your old keymaps' version,
but if user use funcs and `fast-exec` use chain of functions, then after
updating any function `fast-exec/full-commands` set to nil, and all functions
 call again. Example:
`(fast-exec/register-some-keymaps-funcs
    foo
    bar).`"
    `(--map
      (fast-exec/register-keymap-func it)
      (quote ,funcs)))


(defun fast-exec/unregister-keymap-func (func)
"Remove `FUNC` from `fast-exec`' func chain for undefine keymaps to `fast-exec`.
`FUNC` is function, which taked nothing, and gives collection of
`full-commands`.  I am not use for this situation basic list, because
if user change mine list of keymaps, for valid updating
\"fast-exec/full-commands\", user must delete your old keymaps' version,
but if user use funcs and `fast-exec` use chain of functions, then after
updating any function `fast-exec/full-commands` set to nil, and all functions
 call again."
    (when (-contains? fast-exec/keymap-function-chain func)
        (setq fast-exec/keymap-function-chain
              (remove 'fast-exec/keymap-function-chain func)))
    )


(defun fast-exec/reload-functions-chain ()
    "Recall all functions from `fast-exec/keymap-function-chain`."
    (setq fast-exec/full-commands
          (-flatten-n 1 (-map 'funcall fast-exec/keymap-function-chain)))
    )


(defun fast-exec/clean-function-chain (&optional functions-add-after)
    "Clean `keymap-function-chain`, and add `FUNCTIONS-ADD-AFTER` after cleaning."
    (interactive (list nil)) ; Ignore `functions-add-after` in interactive call
    (setq fast-exec/keymap-function-chain functions-add-after)
    )


(defun fast-exec/initialize ()
    "Initialize for `fast-exec`."
    (interactive)
    (require 'fast-exec-initial-keymaps)
    (fast-exec/register-keymap-func 'fast-exec/define-standard-keys)
    (fast-exec/reload-functions-chain)
    (global-set-key (kbd fast-exec/keymap-prefix) 'fast-exec/exec)
    )


(provide 'fast-exec)
;;; fast-exec.el ends here
