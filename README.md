# Fast Exec (fast-exec.el)
`fast-exec.el` is awesome package for Emacs, it's mark executing command ("M-x") as very fast task.

## Cool Demo

![fast-exec demo](doc/assets/gif/fast-exec-demo.gif)

## Get Start
Install this package, for example with current command, in this
example the user save all packages in `.emacs.d/lisp`:

```bash
cd .emacs.d/lisp
git clone https://github.com/semenInRussia/fast-exec.el.git
```

For set up paste the folowing code into your Emacs init file:

```emacs-lisp

(require 'fast-exec)

```

## Usage

By default, to run `fast-exec-exec` you must hit `M-x` and type
`fast-exec-exec`, but you can bind it with a key binding

`fast-exec-exec` is the main function of `fast-exec.el`, after running
it shows the hints buffer which view previous words of current
sentence, first letter of a current command word and variants of its
word (one letter can demote some words):

- hit a letter. you can see it in the hint buffer, letter will be
  shown with `|`s around.  It will run the command with the
  corresponding name if the current word is the final word of a
  command's name.  If the letter isn't bound with any command that the
  buffer will be updated to show letters
- press `0` (ZERO) to exit from the buffer

## Support of Very Famous Packages

By default, `fast-exec` contains only keymaps of vanila Emacs
commands, but you can use very famous package, for example:
`projectile` and `magit`.  For some famous package `fast-exec` has a
built-in support. The full list of supported packages:

- `package.el` (built-in Emacs)
- [Projectile](https://github.com/bbatsov/projectile)
- [Yasnippet](https://github.com/joaotavora/yasnippet)
- `agenda`
- [Magit](https://github.com/magit/magit)
- [Flycheck](https://www.flycheck.org/)
- [Haskell Mode](https://github.com/haskell/haskell-mode) 
- [skeletor](https://github.com/chrisbarrett/skeletor.el "this is cool package!")
* [format-all](https://github.com/lassik/emacs-format-all-the-code "cool Package for Format Code in 50+ languages")
* [wikinforg](https://github.com/progfolio/wikinforg "Package for Load
  Org from Wiki")
* [suggest](https://github.com/Wilfred/suggest.el "Coool Package for
  Suggest Elisp Functions by In/Out")
* [devdocs](https://github.com/astoff/devdocs.el "Read Docs from
DevDocs in Emacs!")
* `helm-wikipedia`
* [deadgrep](https://github.com/Wilfred/deadgrep "Link to Repo of `deadgrep`")

To enable the support, paste the following code to your Emacs config.

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

To create commands (not binding) you must use the functions
`fast-exec-make-command` and `fast-exec-make-some-commands`

Function `fast-exec-make-command` accept 2 arguments, the first is a
the name of a command, the first letters of the words should be used
when you run `fast-exec-exec`, the second is an Emacs command which
will be evaluated after choosing it in the hints buffer.  For example

```emacs-lisp
(fast-exec-make-command "Call Foo" 'foo)
```

Function `fast-exec-make-some-command` accept any number of arguments,
and do `fast-exec-make-command`, but for some commands.  It takes
pairs from the name of a command and the emacs function, they will be
passed to `fast-exec-make-command` and the result of each call will be
returned with function `fast-exec-make-some-command` as list.

Example:

```emacs-lisp
(fast-exec-make-some-commands
 ("Pretty Print Buffer" 'pp-buffer)
 ("Pretty Print Last Expression" 'pp-last-sexp))
```

OK, you can create commands, but why?

To using them, you should return the macro `fast-exec-bind`.  It
accepts 2 arguments, the first is the name of a binding (this is very
useful to unbind), the second argument is a body (like on body of
`defun`), the body will be evaluated and used as list of
`fast-exec-command` (you can create them with functions
`fast-exec-make-command` and `fast-exec-make-some-commands`), so you
can bind commands of `pp` with `fast-exec` using the following code

```elisp
(fast-exec-bind 'pp
  (fast-exec-make-some-commands
   ("Pretty Print Buffer" 'pp-buffer)
   ("Pretty Print Last Expression" 'pp-last-sexp)))
```

For removing this binding, use the macro `fast-exec-unbind`:

```elisp
(fast-exec-unbind pp)
```
