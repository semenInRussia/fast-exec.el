;;; fast-exec-full-commands --- This subpackage of fast-exec.el for manipulations with full-commands

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1
;; Packages-Requires: ((dash "2.19.1"))
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
;; This package has functions for manipulations with full-commands: create, get info

;;; Code:
(require 'dash)
(require 'fast-exec-str)


(defun fast-exec/full-command (command-name command)
    "Return full with func `COMMAND`, named `COMMAND-NAME`.
WARNING! be caruful with case of `COMMAND-NAME` words of name in first lower
character will ignored as unnecassary."
    (let* ((command-words
            (s-split-words command-name))
           (command-important-parts
            (--map
             (fast-exec-str/join-strings " " it)
             (-partition-before-pred 'fast-exec-str/first-letter-upper-p command-words)))
           (chars-of-command-important-parts
            (--map (string-to-char (s-downcase it)) command-important-parts)))
        (list
         command
         (-zip chars-of-command-important-parts command-important-parts))))


(defmacro fast-exec/some-commands (&rest names-and-commands)
    "Return some commands with names with names % commands from `NAMES-AND-COMMANDS`."
    `(--map
      (fast-exec/full-command (-first-item it) (-second-item it))
      (quote ,names-and-commands)))


(defun fast-exec/full-command-nth-char-and-word (command n)
    "Return `N`-th char and word of `COMMAND`'s name."
    (nth n (-second-item command)))


(defun fast-exec/full-command-nth-word (command n)
    "Return `N`-th word of `COMMAND`'s name."
    (cdr (fast-exec/full-command-nth-char-and-word command n))
    )


(defun fast-exec/full-command-nth-char (command n)
    "Return first character of `N`-th word of `COMMAND`'s name."
    (-first-item (fast-exec/full-command-nth-char-and-word command n))
    )


(defun fast-exec/full-commands-with-excepted-nth-char (commands expected-char n)
    "Return full commands from `COMMANDS` what has `N`-th char `EXPECTED-CHAR`."
    (--filter
     (= (fast-exec/full-command-nth-char it n) expected-char)
     full-commands)
    )


(defun fast-exec/nth-chars-of-full-commands (commands n)
    "Return first characters of `N`-th words of `COMMANDS`' names."
    (--map
     (fast-exec/full-command-nth-char it n)
     commands)
    )


(defun fast-exec/nth-words-of-full-commands (commands n)
    "Return first characters of `N`-th words of `COMMANDS`' names."
    (--map
     (fast-exec/full-command-nth-word it n)
     commands)
    )


(defun fast-exec/full-command-only-command (full-command)
    "Return `command-function` of `FULL-COMMAND`."
    (-first-item full-command))


(defun fast-exec/full-command-call (full-command)
    "Exceute only command of `FULL-COMMAND`."
    (call-interactively (fast-exec/full-command-only-command full-command))
    )


(defun fast-exec/call-first-full-command (full-commands)
    "Execute only command of first item of `FULL-COMMANDS`."
    (fast-exec/full-command-call (-first-item full-commands))
    )


(provide 'fast-exec-full-commands)
;;; fast-exec-full-commands.el ends here
