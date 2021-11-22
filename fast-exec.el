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
(require 'fast-exec-str)
(require 'fast-exec-functools)
(require 'fast-exec-hashsets)

(defgroup fast-exec nil
    "Very Fast Execute Commands!."
    :group 'tools
    :link
    '(url-link :tag "GitHub" "https://github.com/semenInRussia/fast-M-x.el")
    )


(defcustom fast-exec/hints-buffer-name
  "*fast-exec view candidates to run command*"
  "Name of buffer that created for asking user char of word of command."
  :group 'fast-exec
  :type 'string)


(defcustom fast-exec/keymap-function-chain nil
  "List of function, which take nothing, and return some `full-commands`."
  :group 'fast-exec
  :type '(repeat function))


(defcustom fast-exec/full-commands nil
  "This is list of FULL COMMANDS.  DON'T TOUCH, PLEASE!
FULL COMMAND is command function and list of char for type and words for view."
  :group 'fast-exec
  :type '(repeat (command '(repeat char string))))


(defcustom fast-exec/keymap-prefix "M-a"
  "Fast-Exec keymap prefix."
  :group 'fast-exec
  :type 'string)


(defcustom fast-exec/backward-step-key 127
  "Key (1 char) for go to backward step of fast-exec/exec.
Defaults to: DEL (backspace)."
  :group 'fast-exec
  :type 'char)


(defcustom fast-exec/exit-key 48
  "Key (1 char) for exit fast-exec/exec.
Defaults to: 0 (zero)."
  :group 'fast-exec
  :type 'char)


(defun fast-exec/*to-string-nth-char-and-full-commands*
    (char-and-full-commands n)
    "To str grouped by char of `N`-th word of name `CHAR-AND-FULL-COMMANDS`."

    (let* ((char-as-str (char-to-string (-first-item char-and-full-commands)))
           (full-commands (cdr char-and-full-commands))
           (nth-words-of-commands (fast-exec/nth-words-of-full-commands
                                   full-commands n))
           (unique-nth-words (delete-dups nth-words-of-commands))
           (previous-n (max (- n 1) 0))
           previous-words
           joined-previous-words
           (joined-nth-words
            (fast-exec-str/join-strings " | " unique-nth-words)))

        (if (= n 0)
            (setq joined-previous-words "")
            (progn (setq previous-words
                         (delete-dups
                          (fast-exec/nth-words-of-full-commands full-commands
                                                                previous-n)))
                   (setq joined-previous-words (fast-exec-str/join-strings
                                                " | "
                                                previous-words)))
            )

        (s-lex-format
         "${joined-previous-words} | ${char-as-str} | ${joined-nth-words}")
        ))


(defun fast-exec/*to-string-groups-by-nth-char-full-commands* (groups n)
    "To str grouped by char of `N`-th word of name `full-commands` `GROUPS`."
    (--map (fast-exec/*to-string-nth-char-and-full-commands* it n)
           groups)
    )


(defun fast-exec/*to-string-full-commands-as-hints-with-nth-chars*
    (full-commands n)
    "To str `FULL-COMMANDS` as hints for typing `N`-th char of commands' names."
    (s-join "\n"
            (fast-exec/*to-string-groups-by-nth-char-full-commands*
             (--group-by (fast-exec/full-command-nth-char it n) full-commands)
             n)))


(defun fast-exec/*insert-full-commands-as-hints-with-nth-chars*
    (full-commands n)
    "Insert `FULL-COMMANDS` as hints for typing `N`-th char in commands' names."
    (insert
     (fast-exec/*to-string-full-commands-as-hints-with-nth-chars*
      full-commands n))
    )


(defun fast-exec/*dev-align-regexp* (start end regexp)
    "Alignment with respect to the given regular expression `REGEXP`.
Call to region begin from `START` and end to `END`.
Part of Code got from: https://www.emacswiki.org/emacs/AlignCommands#h5o-2"
    (interactive "r\nsAlign regexp: ")
    (align-regexp start end
                  (concat "\\(\\s-*\\)" regexp) 1 1 nil))


(defun fast-exec/*view-full-commands-as-hints-with-nth-chars-in-new-buffer*
    (full-commands n)
    "View `FULL-COMMANDS` as candidates with `N`-th character.
For executing in `fast-exec/exec` command."
    (let ((new-buffer (generate-new-buffer fast-exec/hints-buffer-name)))
        (switch-to-buffer new-buffer)
        (funcall 'fundamental-mode)
        (fast-exec/*insert-full-commands-as-hints-with-nth-chars*
         full-commands
         n)
        (highlight-regexp "| . | ." 'hi-black-b)
        (highlight-regexp "| . |" 'outline-1)
        (highlight-regexp "| . |" 'hi-red-b)
        (fast-exec/*dev-align-regexp* (point-min) (point-max) "| . |")
        ))


(defun fast-exec/*completing-read-full-command-nth-part* (prompt candidates n)
    "Ask full command's `N`-th chars of `CANDIDATES` from user  with `PROMPT`."
    (fast-exec/*view-full-commands-as-hints-with-nth-chars-in-new-buffer*
     candidates
     n)

    (prog1
        (read-char)
        (kill-buffer fast-exec/hints-buffer-name))
    )


(defun fast-exec/exec
    (&optional full-commands typed-chars-num char)
    "Execute command and return `limited-full-commands`.
Main comand of `fast-exec.el`.  Command searching in `FULL-COMMANDS`.  Commands
limit depends on `TYPED-CHARS-NUM` and `CHAR`."
    (interactive)
    (setq typed-chars-num (or typed-chars-num 0))
    (setq full-commands (or full-commands fast-exec/full-commands))
    (setq char
          (or char (fast-exec/*completing-read-full-command-nth-part*
                    "Enter Character, Please (: "
                    full-commands typed-chars-num)))

    (cond
      ((= char fast-exec/backward-step-key)
       (fast-exec/*backward-step* full-commands typed-chars-num)
       (message "Go to backward STEP!!!!!"))
      ((= char fast-exec/exit-key)
       (message "Exit from fast-exec ... Good By :0"))
      (t
       (fast-exec/*handle-nth-char-of-commands*
        full-commands
        char
        typed-chars-num))))


(defun fast-exec/*handle-nth-char-of-commands* (commands char n)
    "Handle `CHAR` of `N`-th word of command from `COMMANDS`."
    (interactive)
    (let ((suitable-full-commands
           (fast-exec/full-commands-with-excepted-nth-char
            commands
            char
            n)))
        (pcase (length suitable-full-commands)
            (0 (message
                "Your command not found, we are back you to previous char!")
               (fast-exec/exec full-commands n))
            (1 (message "Your command found! :) ... Executing")
               (fast-exec/call-first-full-command suitable-full-commands))
            (_ (fast-exec/exec suitable-full-commands (+ n 1)))
            )))


(defun fast-exec/*backward-step* (full-commands n)
    "Go to backward step in `fast-exec`.
This is mean: Extend `FULL-COMMANDS` and decrease typed char of command: `N`."
    (setq n (- n 1))

    (let* ((first-command (-first-item full-commands))
           (command-all-initials (string-to-list
                                  (fast-exec/full-command-name-initials
                                   first-command)))
           (command-initials (-slice command-all-initials 0 n))
           (enumerated-command-initials (fast-exec-functools/enumerate
                                         command-initials))
           (suitable-commands (--reduce-from
                               (fast-exec/full-commands-with-excepted-nth-char
                                acc
                                (cdr it)
                                (-first-item it))
                               fast-exec/full-commands
                               enumerated-command-initials)))
        (fast-exec/exec suitable-commands n))
    )


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
    "Add `FUNCS` to `fast-exec`' func chain for defining keymaps to `fast-exec`.
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


(defun fast-exec/*enable-builtin-support-function* (pkg-name)
    "As `fast-exec/enable-builtin-support`, but without quoting `PKG-NAME`."
    (let ((built-in-func
           (intern
            (s-concat "fast-exec/define-" (symbol-name pkg-name) "-keys"))))
        (fast-exec/register-keymap-func built-in-func)
        ))


(defmacro fast-exec/enable-builtin-support (pkg-name)
    "Enable package support with name `PKG-NAME` builtin in `fast-exec.el`.
Examples of `PKG-NAME`:
* yasnippet
* projectile."
    `(fast-exec/*enable-builtin-support-function* ',pkg-name)
    )


(defmacro fast-exec/enable-some-builtin-supports (&rest pkg-names)
    "Enable some package's supports called `PKG-NAMES` builtin in fast-exec.el.
Examples of `PKG-NAMES`:
* yasnippet
* projectile."
    `(--map (fast-exec/*enable-builtin-support-function* it) (quote ,pkg-names))
    )


(defun fast-exec/unregister-keymap-func (func)
    "Undefine `FUNC`'s keymaps.
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
    (interactive)
    (setq fast-exec/full-commands
          (-flatten-n 1 (-map 'funcall fast-exec/keymap-function-chain)))
    )


(defun fast-exec/clean-function-chain (&optional functions-add-after)
    "Clean `keymap-function-chain`, and add `FUNCTIONS-ADD-AFTER` after."
    (interactive (list nil)) ; Ignore `functions-add-after` in interactive call
    (setq fast-exec/keymap-function-chain functions-add-after)
    (setq fast-exec/full-commands nil)
    (fast-exec/reload-functions-chain)
    )


(defun fast-exec/initialize ()
    "Initialize for `fast-exec`."
    (interactive)
    (require 'fast-exec-initial-keymaps)
    (fast-exec/register-keymap-func 'fast-exec/define-standard-keys)
    (fast-exec/reload-functions-chain)
    (global-set-key (kbd fast-exec/keymap-prefix) 'fast-exec/exec)
    )

(require 'fast-exec-yasnippet-keymaps)
(require 'fast-exec-projectile-keymaps)
(require 'fast-exec-magit-keymaps)
(require 'fast-exec-flycheck-keymaps)

(provide 'fast-exec)
;;; fast-exec.el ends here
