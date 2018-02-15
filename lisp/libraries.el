;;; libraries.el --- my libraries
;;
;; Filename: libraries.el
;; Description: library function for emacs setup
;; Author: WangXiao
;; Copyright 2018 (c) WangXiao
;; Created: Sat Jan 13 11:39:06 2018 (+0800)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; Features:
;;  1. Find root according to current opened buffer file name and a stop filename list
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;  1. init version. Find root.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(defconst athenacle|lib-stop-file
  '(
    "compile_commands.json"
    "package.json"
    "setup.py"
    "__init__.py"
    "pom.xml"
    )
  "Get root stop filename list.")

(defun athenacle|lib-file-exist-in-dir (dir name)
  "Check file NAME exist in DIR. This works even when name is nil."
  (if name (file-exists-p (expand-file-name name dir))
    nil))

(defun athenacle|lib-file-exist (dir stops)
  "Check filenames in STOPS list exist in DIR."
  (if (not dir) nil)
  (when stops
    (if (athenacle|lib-file-exist-in-dir dir (car stops))
        dir
      (athenacle|lib-file-exist dir (cdr stops)))))

(defun athenacle|lib-parent-directory (dir)
  "Get DIR's parent directory."
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun athenacle|lib-search-from-begin (begin)
  "Begin search from BEGIN."
  (when begin
    (if (athenacle|lib-file-exist begin athenacle|lib-stop-file)
        begin
      (athenacle|lib-search-from-begin (athenacle|lib-parent-directory begin)))))

(defun athenacle|lib-search (begin)
  "Begin search from BEGIN. If found stop file, return its path, otherwise return BEGIN path."
  (let ((path (athenacle|lib-search-from-begin begin)))
    (if path
        path
      begin)))

(defun athenacle|lib-search-buffer-file-root ()
  "Get current buffer file name, and get its root."
  (interactive)
  (let ((name (buffer-file-name)))
    (if name
        (athenacle|lib-search (file-name-directory name))
      default-directory)))



(defmacro athenacle||lib-call-if (pred func &optional object others)
  "Test function PRED, if it eval as not nil, apply OBJECT to FUNC. When FUNC is nil, set it to `message'."
  `(when (funcall ,pred)
     (apply (quote ,func) (cons ,object ,others))))

(defmacro athenacle||lib-call-if-debug-on-error (func &optional object others)
  "If `debug-on-error' is not nil, apply OBJECT to FUNC."
  `(athenacle||lib-call-if
    (lambda () (or init-file-debug debug-on-error))
    ,func
    ,object
    ,others))

(defmacro athenacle||lib-message-if-debug-on-error (object &optional others)
  "If `debug-on-error' is not nil, print OBJECT by `message'. If OTHERS is not nil, append it to paraments of `message'."
  `(athenacle||lib-call-if-debug-on-error message ,object ,others))



(defun athenacle|lib-parse-args (keys args)
  "Get KEYS arguments and body from ARGS."
  (let ((ht (make-hash-table :test 'equal :size 5 :rehash-size 5))
        (body '()))
    (while args
      (let* ((first (car args))
             (next (cdr args)))
        (if (and (symbolp first) (string-prefix-p ":" (symbol-name first)))
            (let* ((name (symbol-name first))
                   (short-name (substring name 1)))
              (when (gethash short-name ht)
                (error "Key %s already exist, previous value is %s" name (gethash short-name ht)))
              (unless (member (intern short-name) keys)
                (error "Key %s doesnot exist in keys parameter" name))
              (unless next
                (error "Need a keyword argument of %s" name))
              (puthash short-name (car next) ht)
              (setq next (cdr next)))
          (add-to-list 'body first))
        (setq args next)))
    (cons ht (nreverse body))))


(defun athenacle|lib-build-lambda-from-sexps (args list-of-sexp)
  "Build lambda function from LIST-OF-SEXP, which signature is (lambda ARGS nil nil LIST-OF-SEXP)."
  (unless (listp list-of-sexp)
    (error "Error type %s of list-of-sexp" (type-of list-of-sexp)))
  (unless (listp args)
    (error "Error type %s of args" (type-of args)))
  (let ((tmp-lambda list-of-sexp)
        (func nil))
    (add-to-list 'tmp-lambda nil)       ;; interactive
    (add-to-list 'tmp-lambda nil)       ;; docstring
    (add-to-list 'tmp-lambda args)      ;; args
    (add-to-list 'tmp-lambda 'lambda)   ;; lambda
    (eval tmp-lambda)))

(cl-defmacro athenacle||lib-macro (name keys &rest rest)
  "Build macro named NAME which has KEYS keyword parameter and REST is the body of function."
  `(cl-defmacro ,name (&rest arg-list)
     (let* ((ht-table (athenacle|lib-parse-args (quote ,keys) arg-list))
            (ht (car ht-table))
            (rest-body (cdr ht-table))
            (actual-args '()))
       (dolist (k (quote ,keys))
         (push (gethash (format "%s" k) ht) actual-args))
       (setq actual-args (nreverse actual-args))
       (push (athenacle|lib-build-lambda-from-sexps nil rest-body) actual-args )
         (apply
          (lambda (body ,@keys)
            (progn ,@rest))
          actual-args))))

(defalias 'ath-defmacro 'athenacle||lib-macro)


(provide 'libraries)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; libraries.el ends here
