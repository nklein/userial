;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :userial)

(defun get-syms-types-places (type-place-list &optional vars types places)
  "Take a list like (:UINT AA :STRING BB :COW CC) and return three lists, the third the list (AA BB CC), the second the list (:UINT :STRING :COW), and the first a list of #'GENSYM symbols as long as the other lists are."
  (cond
    ((null type-place-list)
        (values (nreverse vars)
                (nreverse types)
                (nreverse places)))
    ((= (length type-place-list) 1)
        (error "odd-number of type-value pairs"))
    (t  (get-syms-types-places (rest (rest type-place-list))
                               (cons (gensym "SYMS-") vars)
                               (cons (first type-place-list) types)
                               (cons (second type-place-list) places)))))

(defun quote-2 (aa bb)
  "Take two parameters and return them as a quoted list."
  `(,aa ,bb))

(defun separate-docstring-and-decls (body &optional docstring decls)
  "Take a body form and return two values.  The first value is a list of the docstring and any introductory DECLARE forms in the body and the second value is the rest of the body."
  (let ((first (first body)))
    (cond
      ((and (null decls)
            (null docstring)
            (stringp first))
          (separate-docstring-and-decls (rest body) (cons first docstring)))
      ((and (consp first)
            (eq 'common-lisp:declare (first first)))
          (separate-docstring-and-decls (rest body)
                                        docstring
                                        (cons first decls)))
      (t (values (nconc docstring (nreverse decls)) body)))))
