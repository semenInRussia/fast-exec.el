# Fast Exec (fast-exec.el)
`fast-exec.el` is awesome package for Emacs, it's mark executing command ("M-x") as very fast task.

## Cool Demo

![fast-exec demo](doc/assets/gif/fast-exec-demo.gif)

## Get Start
Install this package, for example with current command, in this example user save all packages in `.emacs.d/lisp`:

```bash
cd .emacs.d/lisp
git clone https://github.com/semenInRussia/fast-exec.el.git
```

For get start paste folowed code to your Emacs init file:

```emacs-lisp

(require 'fast-exec)

```

## Usage

By default, for run `fast-exec-exec` you must hit `M-x` and type
`fast-exec-exec`, but you can bind it with any key binding

`fast-exec-exec` is the main function of `fast-exec.el`, after running
it show buffer of hints, which show previous word of current sentence,
letter, first letter of a current command word and variants of its
word (one letter can demoted some words):

* hit letter, you can see it at buffer of hints, letter will be shown
  with | around.  It will run command with corresponding name or if a
  current word is not the final word of command's name, in its case
  jump to next step, updating buffer of hints
* press `0` (`ZERO`) for exit from buffer of hints


## Support of Very Famous Packages

By default, `fast-exec` contain only keymaps for vanila Emacs commands, but you can use very famous package, for example: projectile, magit. For some famous package `fast-exec` has built in support. Full list of supported packages:

* package (built-in Emacs)
* [Projectile](https://github.com/bbatsov/projectile)
* [Yasnippet](https://github.com/joaotavora/yasnippet)
* `agenda`
* [Magit](https://github.com/magit/magit)
* [Flycheck](https://www.flycheck.org/)
* [Haskell Mode](https://github.com/haskell/haskell-mode) 
* [skeletor](https://github.com/chrisbarrett/skeletor.el "this is cool package!")
* [format-all](https://github.com/lassik/emacs-format-all-the-code "cool Package for Format Code in 50+ languages")
* [wikinforg](https://github.com/progfolio/wikinforg "Package for Load
  Org from Wiki")
* [suggest](https://github.com/Wilfred/suggest.el "Coool Package for
  Suggest Elisp Functions by In/Out")
* [devdocs](https://github.com/astoff/devdocs.el "Read Docs from
DevDocs in Emacs!")
* `helm-wikipedia`
* [deadgrep](https://github.com/Wilfred/deadgrep "Link to Repo of `deadgrep`")

For enable this support, paste this code to your emacs' config.

```emacs-lisp
;            Change projectile to other word from list of supported packages
;                                |
;                                |
;                                |
(fast-exec-use-builtin-support projectile)
;;                               ^
;; This is enable only 1 support |

(fast-exec-use projectile
               yasnippet)
;             ^
;             |
;             |
;             |
;            Change projectile and yasnippet
;            to other words from list of supported packages
```


## Define Your Commands for Run Them with `fast-exec`

For defining commands (not binding) you must use the functions
`fast-exec-make-command` and `fast-exec-make-some-commands`

Function `fast-exec-make-command` take 2 arguments, first is sentence
of command, its letters should be used when you run `fast-exec-exec`,
second is command which will be evaluated after choosing it in buffer
of hints

```emacs-lisp
(fast-exec-make-command "Call Foo" 'foo)
```

Function `fast-exec-make-some-command` take any number of arguments,
and do `fast-exec-make-command`, but for some commands.  It take lists
of lists from sentence of the command and function, they will be
passed to `fast-exec-make-command` and result of each call will be
returned with function `fast-exec-make-some-command` as list.  Example:

```emacs-lisp
(fast-exec-make-some-commands
 ("Pretty Print Buffer" 'pp-buffer)
 ("Pretty Print Last Expression" 'pp-last-sexp))
```

OK, you can create commands, but why?

For using them, you should return the macro `fast-exec-bind`.  It take
2 arguments, first is name of binding (this very useful for unbinding
and updating of bindings without an Emacs Reloading), second argument
is body (like on body of `defun`), this body will be evaluated and
used as list of `fast-exec-command` (you can create them with
functions `fast-exec-make-command` and
`fast-exec-make-some-commands`), so you can bind commands of `pp` with
`fast-exec` using the following code

```elisp
(fast-exec-bind pp
  (fast-exec-make-some-commands
   ("Pretty Print Buffer" 'pp-buffer)
   ("Pretty Print Last Expression" 'pp-last-sexp)))
```

For removing it binding, use the macro `fast-exec-unbind`:

```elisp
(fast-exec-unbind pp)
```
