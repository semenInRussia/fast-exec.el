;;; fast-exec --- Very Fast Executing Emacs Commands

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Packages-Requires: ((dash "2.18.0"))

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



(defgroup fast-exec nil
    "Very Fast Execute Commands!."
    :group 'tools
    :link '(url-link :tag "GitHub" "https://github.com/semenInRussia/fast-M-x.el")
    )


(defcustom fast-exec/buffer-name "*fast-exec view candidates to run command*"
  "Name of buffer that created for asking user char of word of command."
  :group 'fast-exec
  :type 'string)


(defcustom fast-exec/commands-and-names nil
  "List of pair from command and name of this command."
  :group 'fast-exec
  :type '(repeat (command . string))
  )


(defcustom fast-exec/full-commands nil
  "This is list of FULL COMMANDS.
FULL COMMAND is command function and list of char for type and words for view."
  :group 'fast-exec
  :type '(repeat (command '(repeat char string))))


(defcustom fast-exec/keymap-prefix "M-a"
  "Fast-Exec keymap prefix."
  :group 'fast-exec
  :type 'string)


(defun fast-exec/*first-letter* (s)
    "Return first letter of `S`."
    (s-left 1 s)
    )


(defun fast-exec/*first-letter-upper-p* (s)
    "Is first letter of `S` in upper case?"
    (s-capitalized? (fast-exec/*first-letter* (s-trim s)))
    )


(defun fast-exec/*concat-strings* (strings)
    "Concatenate `STRINGS`."
    (--reduce (s-concat acc it) strings))


(defun fast-exec/*join-strings* (sep strings)
    "Join `STRINGS`, by `SEP`."
    (fast-exec/*concat-strings* (-interpose sep strings))
    )


(defun fast-exec/add-command (command-name command)
"Add command `COMMAND` with name `COMMAND-NAME` to `fast-exec` commands lists.
WARNING! be caruful with case of `COMMAND-NAME` words of name in first lower
character will ignored as unnecassary."
    (add-to-list 'fast-exec/commands-and-names '(command command-name))
    (add-to-list 'fast-exec/full-commands
                 (let* ((command-words
                         (s-split-words command-name))
                        (command-important-parts
                         (--map
                          (fast-exec/*join-strings* " " it)
                          (-partition-before-pred 'fast-exec/*first-letter-upper-p* command-words)))
                        (chars-of-command-important-parts
                         (--map (string-to-char (s-downcase it)) command-important-parts)))
                     (list
                      command
                      (-zip chars-of-command-important-parts command-important-parts)))))


(defmacro fast-exec/add-some-commands (&rest names-and-commands)
    "Add some commands to fast-exec commands lists `NAMES-AND-COMMANDS` is pair from name & command."
    `(--map
     (fast-exec/add-command (-first-item it) (-second-item it))
     (quote ,names-and-commands)))


(defun fast-exec/*full-command-nth-char-and-word* (command n)
    "Return `N`-th char and word of `COMMAND`'s name."
    (nth n (-second-item command)))


(defun fast-exec/*full-command-nth-word* (command n)
    "Return `N`-th word of `COMMAND`'s name."
    (cdr (fast-exec/*full-command-nth-char-and-word* command n))
    )


(defun fast-exec/*full-command-nth-char* (command n)
    "Return first character of `N`-th word of `COMMAND`'s name."
    (-first-item (fast-exec/*full-command-nth-char-and-word* command n))
    )


(defun fast-exec/*full-commands-with-excepted-nth-char* (commands expected-char n)
    "Return full commands from `COMMANDS` what has `N`-th char `EXPECTED-CHAR`."
    (--filter
     (= (fast-exec/*full-command-nth-char* it typed-chars-num) char-of-command-word)
     full-commands)
    )


(defun fast-exec/*nth-chars-of-full-commands* (commands n)
    "Return first characters of `N`-th words of `COMMANDS`' names."
    (--map
     (fast-exec/*full-command-nth-char* it n)
     commands)
    )


(defun fast-exec/*nth-words-of-full-commands* (commands n)
    "Return first characters of `N`-th words of `COMMANDS`' names."
    (--map
     (fast-exec/*full-command-nth-word* it n)
     commands)
    )


(defun fast-exec/*to-string-nth-char-and-full-commands* (char-and-full-commands n)
    "To string grouped by character of `N`-th word of name `full-commands`."

    (let* ((char-as-str (char-to-string (-first-item char-and-full-commands)))
           (full-commands (cdr char-and-full-commands))
           (nth-words-of-commands (fast-exec/*nth-words-of-full-commands* full-commands n))
           (joined-nth-words (fast-exec/*join-strings* " | " nth-words-of-commands)))
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
             (--group-by (fast-exec/*full-command-nth-char* it n) full-commands)
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
            (funcall 'org-mode)
            (fast-exec/*insert-full-commands-as-candidates-with-nth-chars* full-commands n)))
    )


(defun fast-exec/*completing-read-full-command-nth-part* (prompt candidates n)
    "Read `full-command`'s `N`-th chars from user's minibufer from `CANDIDATES` with `PROMPT`."
    (setq fast-exec/*char-of-user* nil)
    (fast-exec/*view-full-commands-as-candidates-with-nth-chars-in-new-buffer* candidates n)
    (let ((candidates-chars (fast-exec/*nth-chars-of-full-commands* candidates n)))
        (while (not (-contains? candidates-chars fast-exec/*char-of-user*))
            (setq fast-exec/*char-of-user* (read-char))
            (kill-buffer fast-exec/buffer-name)))
    fast-exec/*char-of-user*)


(defun fast-exec/*full-command-only-command* (full-command)
    "Return `command-function` of `FULL-COMMAND`."
    (-first-item full-command))


(defun fast-exec/*full-command-call* (full-command)
    "Exceute only command of `FULL-COMMAND`."
    (call-interactively (fast-exec/*full-command-only-command* full-command))
    )


(defun fast-exec/*call-first-full-command* (full-commands)
    "Execute only command of first item of `FULL-COMMANDS`."
    (fast-exec/*full-command-call* (-first-item full-commands))
    )


(defun fast-exec/exec (&optional full-commands typed-chars-num char-of-command-word)
    "Execute command, command searching from `CHAR-OF-COMMAND-WORD`."
    (interactive)
    
    (setq full-commands (or full-commands fast-exec/full-commands))
    (setq typed-chars-num (or typed-chars-num 0))
    (setq char-of-command-word
          (fast-exec/*completing-read-full-command-nth-part*
           "Enter Character, Please (: " full-commands typed-chars-num))
    (let ((suitable-full-commands
           (fast-exec/*full-commands-with-excepted-nth-char*
            full-commands
            char-of-command-word
            typed-chars-num)))
        (pcase (length suitable-full-commands)
            ;; TODO: Add timeout
            (0 (message "Your command not found, we are back you to previous char!")) ;; TODO: Go back...
            (1 (message "Your command found! :) ... Executing")
               (fast-exec/*call-first-full-command* suitable-full-commands))
            (_ (fast-exec/exec suitable-full-commands (+ typed-chars-num 1)))
            )))


(defun fast-exec/initialize ()
    "Initialize for `fast-exec`."
    (interactive)
    (global-set-key (kbd fast-exec/keymap-prefix) 'fast-exec/exec)
    )


(provide 'fast-exec)
;;; fast-exec.el ends here
