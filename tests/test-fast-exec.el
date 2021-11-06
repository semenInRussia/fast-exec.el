;;; package --- tests for fast-exec.el

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: semenInRussia
;; Package-Requires: ((buttercup "1.24"))
;; Keywords: tools

;;; Commentary:

;;; This is tests for fast-exec.el
;;; I am use framework for unit-testing: `buttercup`

;;; Code:
(require 'buttercup)
(require 'fast-exec)


(defun create-full-command (name command)
    "Create `full-command` with name `NAME` and command `COMMAND`."
    (fast-exec/add-command name command)
    (-last-item fast-exec/full-commands)
    )

(defun full-command ()
    "Return full command."
    (create-full-command "Save All Buffer" 'save-some-buffers)
    )


(describe "Fast Execute"

    (describe "functions with strings"
        (it "join-strings"
            (expect (fast-exec/*join-strings* "," '("a" "b" "c"))
                    :to-equal "a,b,c")))
                    
                    
    (describe "manipulations with commands"
        :var (foo)
        (before-each
            (setf (symbol-function 'foo) (lambda () (interactive) nil))
            (spy-on 'foo))
        
        (after-each
            (fast-exec/initialize))
        
        (it "nth word and char of command's name"
            (expect (fast-exec/*full-command-nth-char-and-word* (full-command) 1)
                    :to-equal '(?a . "All")))

        (it "nth word of command's name"
            (expect (fast-exec/*full-command-nth-word* (full-command) 1)
                    :to-equal "All"))

        (it "only command of full command"
            (expect (fast-exec/*full-command-only-command* (full-command))
                    :to-equal 'save-some-buffers))
        
        (it "nth char of command's name"
            (expect (fast-exec/*full-command-nth-char* (full-command) 1)
                    :to-equal ?a))
        
        (it "call command of full command"
            (expect (progn
                        (fast-exec/*full-command-call* (create-full-command "Foo" 'foo))
                        'foo))
            :to-have-been-called)

        (it "call command of first full command from full commands"
            (expect (progn
                        (fast-exec/*call-first-full-command*
                         (list (create-full-command "Foo" 'foo)))
                        'foo) )
            :to-have-been-called)))

                    
(provide 'test-fast-exec)
;;; test-fast-exec.el ends here
