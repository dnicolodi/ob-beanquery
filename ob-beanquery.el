;;; ob-beanquery.el --- Babel Functions for bean-query -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Daniele Nicolodi <daniele@grinta.net>
;; Keywords: literate programming, reproducible research
;; URL: https://orgmode.org

;; This file is not part of GNU Emacs.

;; This package is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this package.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating bean-query source code.

;;; Code:

(require 'ob)
(require 'ob-sql)

(declare-function org-table-convert-region "org-table" (beg end &optional separator))
(declare-function org-table-to-lisp "org-table" (&optional txt))

(defvar org-babel-default-header-args:beanquery '())

(defvar org-babel-header-args:beanquery
  '((db        . :any)
    (errors    . :any)
    (numberify . :any))
  "bean-query specific header args.")

(defun org-babel-expand-body:beanquery (body params)
  "Expand BODY according to the values of PARAMS."
  (let ((prologue (cdr (assq :prologue params)))
        (epilogue (cdr (assq :epilogue params))))
    (mapconcat 'identity
               (list
                prologue
                (org-babel-sql-expand-vars
                 body (org-babel--get-vars params) t)
                epilogue)
               "\n")))

(defvar org-babel-beanquery-command "bean-query")

(defun org-babel-execute:beanquery (body params)
  "Execute bean-query BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'."
  (let ((result-params (split-string (or (cdr (assq :results params)) "")))
        (db (cdr (assq :db params)))
        (no-errors-p (not (equal "no" (cdr (assq :errors params)))))
        (numberify (cdr (assq :numberify params)))
        (colnames-p (not (equal "no" (cdr (assq :colnames params))))))
    (with-temp-buffer
      (insert
       (org-babel-eval
        (string-join (list
                      org-babel-beanquery-command
                      (if numberify "--numberify" "")
                      (if no-errors-p "--no-errors" "")
                      "--format=csv"
                      db)
                     " ")
        (org-babel-expand-body:beanquery body params)))
      ;; bean-query uses \r\n newlines
      (goto-char 0)
      (while (search-forward "\r\n" nil t) (replace-match "\n" nil t))
      (org-babel-result-cond result-params
        (buffer-string)
        (if (equal (point-min) (point-max))
            ""
          (org-table-convert-region (point-min) (point-max))
          (org-babel-beanquery-table-or-scalar
           (org-babel-beanquery-offset-colnames
            (org-table-to-lisp) colnames-p)))))))

(defun org-babel-beanquery-table-or-scalar (result)
  "Cleanup cells in the RESULT table.
If RESULT is a trivial 1x1 table, then unwrap it."
  (if (and (equal 1 (length result))
           (equal 1 (length (car result))))
      (org-babel-read (caar result) t)
    (mapcar (lambda (row)
              (if (eq 'hline row)
                  'hline
                (mapcar (lambda (cell) (org-babel-read cell t)) row)))
            result)))

(defun org-babel-beanquery-offset-colnames (table headers)
  "If HEADERS is non-nil then offset the first row as column names in TABLE."
  (if headers
      (cons (car table) (cons 'hline (cdr table)))
    (cdr table)))

(defun org-babel-prep-session:beanquery(_session _params)
  "Raise an error because support for bean-query sessions isn't implemented.
Prepare SESSION according to the header arguments specified in PARAMS."
  (error "bean-query sessions not yet implemented"))

(provide 'ob-beanquery)

;;; ob-beanquery.el ends here
