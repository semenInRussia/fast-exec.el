;;; fast-exec-str --- Private package of fast-exec, for working with strings

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.0.1

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
;;

;;; Code:
(require 's)


(defun fast-exec-str/first-letter (s)
    "Return first letter of `S`."
    (s-left 1 s)
    )


(defun fast-exec-str/first-letter-upper-p (s)
    "Is first letter of `S` in upper case?"
    (s-capitalized? (fast-exec-str/first-letter (s-trim s)))
    )


(defun fast-exec-str/concat-strings (strings)
    "Concatenate `STRINGS`."
    (--reduce (s-concat acc it) strings))


(defun fast-exec-str/join-strings (sep strings)
    "Join `STRINGS`, by `SEP`."
    (fast-exec-str/concat-strings (-interpose sep strings))
    )


(defun fast-exec-str/chars-to-string (chars)
    "Transform list of `CHARS` to string."
    (fast-exec-str/concat-strings (-map 'char-to-string chars))
    )


(provide 'fast-exec-str)
;;; fast-exec-str.el ends here
