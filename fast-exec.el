;;; package --- fast-exec

;; Copyright (C) 2013-2021 Free Software Foundation, Inc.

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


(defcustom fast-exec/completion-system 'auto
  "The completion system to be used by executing command.
Code taked from projectile (https://github.com/bbatsov/projectile)."
  :group 'projectile
  :type '(radio
          (const :tag "Auto-detect" auto)
          (const :tag "Ido" ido)
          (const :tag "Helm" helm)
          (const :tag "Ivy" ivy)
          (const :tag "Default" default)
          (function :tag "Custom function")))


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
    "Add command `COMMAND` with name `COMMAND-NAME` to `fast-exec` commands list.
WARNING! be caruful with case of `COMMAND-NAME` words of name in first lower
character will ignored as unnecassary."
    (add-to-list 'fast-exec/commands-and-names '(command command-name))
    (add-to-list 'fast-exec/full-commands
                 (let* ((command-words
                         (s-split-words command-name))
                        (command-important-parts
                         (--map
                          (fast-exec/*join-strings* " " it)
                          (-partition-after-pred 'fast-exec/*first-letter-upper-p* command-words)))
                        (chars-of-command-important-parts
                         (--map (string-to-char (s-downcase it)) command-important-parts)))
                     (list
                      command
                      (-zip chars-of-command-important-parts command-important-parts)))))


(cl-defun fast-exec/*completing-read* (prompt choices &key initial-input action)
    "Present a project tailored `PROMPT` with `CHOICES`.
Code taked from projectile (https://github.com/bbatsov/projectile)."
    (let (res)
        (setq res
              (pcase (if (eq fast-exec/completion-system 'auto)
                         (cond
                           ((bound-and-true-p ido-mode)  'ido)
                           ((bound-and-true-p helm-mode) 'helm)
                           ((bound-and-true-p ivy-mode)  'ivy)
                           (t 'default))
                         fast-exec/completion-system)
                  ('default (completing-read prompt choices nil nil initial-input))
                  ('ido (ido-completing-read prompt choices nil nil initial-input))
                  ('helm
                   (if (and (fboundp 'helm)
                            (fboundp 'helm-make-source))
                       (helm :sources
                             (helm-make-source "Fast Exec" 'helm-source-sync
                                 :candidates choices
                                 :action (if action
                                             (prog1 action
                                                 (setq action nil))
                                             #'identity))
                             :prompt prompt
                             :input initial-input
                             :buffer "*helm-fast-exec*")
                       (user-error "Please install helm")))
                  ('ivy
                   (if (fboundp 'ivy-read)
                       (ivy-read prompt choices
                                 :initial-input initial-input
                                 :action (prog1 action
                                             (setq action nil))
                                 :caller 'fast-exec/completing-read)
                       (user-error "Please install ivy")))
                  (_ (funcall fast-exec/*completing-read* prompt choices))))
        (if action
            (funcall action res)
            res)))


(defun fast-exec/*completing-read-char* (prompt chars)
    "User type character with `fast-exec/completion-system` from `CHARS`, and call `ACTION`."
    (string-to-char (fast-exec/*completing-read* prompt (-map 'char-to-string chars)))
    )


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
          (fast-exec/*completing-read-char*
           "Enter Character, Please (: "
           (fast-exec/*nth-chars-of-full-commands* full-commands typed-chars-num)))

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
    
    (setq fast-exec/commands-and-names nil)
    (setq fast-exec/full-commands nil)
    
    (fast-exec/add-command "Save All Files" 'save-some-buffers)
    (fast-exec/add-command "Revert Buffer" 'revert-buffer)
    (fast-exec/add-command "Set Mark" 'set-mark-command)

    (global-set-key (kbd fast-exec/keymap-prefix) 'fast-exec/exec)
    )

(provide 'fast-exec)
;;; fast-exec.el ends here
