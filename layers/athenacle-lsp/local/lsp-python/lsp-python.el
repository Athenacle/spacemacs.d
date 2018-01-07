;;; lsp-python.el --- Python support for lsp-mode

;; Copyright (C) 2017 Vibhav Pant <vibhavp@gmail.com>

;; Author: Vibhav Pant <vibhavp@gmail.com>
;; Version: 1.0
;; Package-Requires: ((lsp-mode "3.0"))
;; Keywords: python
;; URL: https://github.com/emacs-lsp/lsp-python

;;; Code:
(require 'lsp-mode)
(require 'lsp-common)

(defvar pyls-path "pyls")


(defun lsp-python--get-root ()
  (lsp-make-traverser
   #'(lambda (dir)
       (directory-files dir dir nil "\\(__init__\\|setup\\)\\.py"))))

(defun lsp-python--make-renderer (mode)
  `(lambda (str)
     (with-temp-buffer
       (delay-mode-hooks (,(intern (format "%s-mode" mode))))
       (insert str)
       (font-lock-ensure)
       (buffer-string))))

(defun lsp-python--initialize-client (client)
  (lsp-provide-marked-string-renderer client "python" (lsp-python--make-renderer "python")))

(lsp-define-stdio-client
 lsp-python
 "python"
 (lsp-make-traverser #'(lambda (dir)
						                        (directory-files
						                         dir
						                         nil
						                         "\\(__init__\\|setup\\)\\.py")))
 `(,pyls-path)
 :initialize #'lsp-python--initialize-client)

(provide 'lsp-python)
;;; lsp-python.el ends here
