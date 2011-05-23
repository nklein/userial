;; Copyright (c) 2011 nklein software
;; MIT License. See included LICENSE.txt file for licensing details.

(in-package :userial)

(defun get-syms-types-places (type-place-list &optional vars types places)
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
  `(,aa ,bb))

(defun separate-docstring-and-decls (body &optional docstring decls)
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
