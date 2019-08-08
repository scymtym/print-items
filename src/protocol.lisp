;;;; protocol.lisp --- Protocol functions of the utilities.print-items system.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.print-items)

;;; Print items protocol

(defgeneric print-items (object)
  (:method-combination append)
  (:documentation
   "Return a list of items that should appear in the printed
    representation of OBJECT.

    Each method should return a list of items of the form

      ITEM              ::= (KEY [FORMAT-AND-VALUES [(OPTION*)]])

      KEY               ::= any Lisp object

      FORMAT-AND-VALUES ::= `nil'
                            | VALUE
                            | (FORMAT-CONTROL ARGUMENT*)
      FORMAT-CONTROL    ::= a format control string or a formatter function
      VALUE             ::= any Lisp object not of type (or null (cons string))
      ARGUMENT          ::= any Lisp object

      OPTION            ::= CONSTRAINT
      CONSTRAINT        ::= ((:before | :after) KEY)

    When multiple items have `eql' KEYs, items appearing closer to the
    beginning of the item list take precedence. This mechanism can be
    used by subclasses to replace print items produced by
    superclasses.

    When FORMAT-AND-VALUE is `nil' the whole item is ignored. This
    mechanism can be used by subclasses to disable print items
    produced by superclasses."))

(defgeneric effective-print-items (object)
  (:documentation
   "Return a list of items like `print-items', but filtered and sorted.

    Filtering removes all but the first occurrence of multiple items
    using the same key.

    Sorting arranges the filtered items according to their specified
    constraints."))

;;; Default behavior

(defmethod print-items append ((object t))
  ;; Default behavior is to not return any print items for OBJECT.
  '())

(defmethod effective-print-items ((object t))
  (let* ((raw    (print-items object))
         (unique (remove-duplicates raw :key #'first :from-end t)))
   (sort-with-partial-order unique #'item-<)))

;;;

(defun select-item (items key)
  (find key items :test #'eq :key #'first))

(defun select-items (items &rest keys)
  (map 'list (curry #'select-item items) keys))

(defun expand-select-item (items key)
  `(find ,key ,items :test #'eq :key #'first))

(defun expand-select-items (items keys)
  (loop :for key :in keys
        :collect (expand-select-item items key)))

(define-compiler-macro select-items (items &rest keys)
  (once-only (items)
    `(list ,@(expand-select-items items keys))))

;;; Formatting functions

(defun format-item (stream item &optional colon? at?)
  (declare (ignore colon? at?))
  (destructure-item (nil enabled? format arguments) item
    (when enabled?
      (format stream "~?" format arguments))))

(defun format-print-items (stream items &optional colon? at?)
  "Print ITEMS onto STREAM.

   ITEMS is a list of items of the form

     (KEY VALUE [FORMAT [(CONSTRAINT*)]]

   where

     KEY        ::= any Lisp object
     VALUE      ::= any Lisp object
     FORMAT     ::= `nil' or a format string (Default is \"~A\")

     CONSTRAINT ::= (:before | :after) KEY"
  (declare (ignore colon? at?))
  (mapc (curry #'format-item stream)
        ;; TODO we might remove this later and expect the client to
        ;; pass in the effective list of items
        (sort-with-partial-order
         (remove-duplicates items :key #'first :from-end t)
         #'item-<)))

(defun format-selected-items (stream format-control items &rest keys)

  (let ((fragments (map 'list (lambda (key)
                                (when-let ((item (select-item items key)))
                                  (with-output-to-string (stream)
                                    (format-item stream item))))
                        keys)))
    (apply #'format stream format-control fragments)))

(define-compiler-macro format-selected-items (stream format-control items &rest keys)
  (once-only (stream items)
    `(format ,stream ,format-control
             ,@(loop :for key :in keys
                     :collect `(with-output-to-string (stream)
                                 (format-item
                                  stream ,(expand-select-item items key)))))))

;;; Print items mixin

(defclass print-items-mixin ()
  ()
  (:documentation
   "This mixin class adds printing via `print-items' to classes."))

(defmethod print-object ((object print-items-mixin) stream)
  (cond (*print-readably*
         (call-next-method))
        (t
         (print-unreadable-object (object stream :identity t)
           (format stream "~A~@[ ~/print-items:format-print-items/~]"
                   (class-name (class-of object))
                   (effective-print-items object))))))
