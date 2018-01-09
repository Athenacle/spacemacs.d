;;; package --- Summary Add Support for lsp-mode

;;; Commentary:
;;; This file should load after mainly libs have been loaded.
;;; For spacemacs, this should be required in user config

;;; Code:

(require 'lsp-mode)

(defvar athenacle|js-ts-server "/home/wangxiao/gits/javascript-typescript-langserver/lib/language-server-stdio.js")
(defconst athenacle|node "node")

(defun athenacle|travser (dir name)
  (directory-file dir nil name))

(defun athenacle|make-renderer (mode)
  `(lambda (str)
     (with-temp-buffer
       (delay-mode-hooks (,(intern (format "%s-mode" mode))))
       (insert str)
       (font-lock-ensure)
       (buffer-string))))

(defun athenacle|get-root (dir stop-name)
  "Get root function.
STOP-NAME Assume as root if is file is found."
  (lsp-make-traverser #'(lambda (dir) (directory-files dir nil stop-name))))

(defun athenacle|js-ts-init (client)
  (lsp-provide-marked-string-renderer client "javascript" (athenacle|make-renderer "javascript")))

(lsp-define-stdio-client
 lsp-athenacle-js-ts
 "javascript"
 (lsp-make-traverser #'(lambda (dir)
                         (directory-files dir nil "package.json")))
 `(,"node" ,athenacle|js-ts-server)
 :initialize #'athenacle|js-ts-init
 :ignore-regexps '("readFile .*? requested by Typescript but content not available"))

(add-hook 'js-mode-hook #'lsp-athenacle-js-ts-enable)
(add-hook 'js2-mode-hook #'lsp-athenac-js-ts-enable)


(provide 'init-lsp)
;;; lsp.el ends here
