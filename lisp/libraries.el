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
  (if (athenacle|lib-file-exist begin athenacle|lib-stop-file)
      begin
    (athenacle|lib-search-from-begin (athenacle|lib-parent-directory begin) )))

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

(provide 'libraries)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; libraries.el ends here
