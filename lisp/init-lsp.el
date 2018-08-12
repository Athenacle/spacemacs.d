;;; init-lsp.el --- init lsp
;;
;; Filename: init-lsp.el
;; Description: initilize LSP utilites
;; Author: WangXiao
;; Copyright 2018 (c) WangXiao;; Created: Sun Feb 11 13:35:29 2018 (+0800)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; Initilize LSP packages.
;;  Available Language: Go, Javascript, Java, Python
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;  1. Feb 11, 2018: First Revision
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

(require 'use-package)
(require 'cl-lib)
(require 'libraries)
(require 'lsp-mode)
(require 'company)
(require 'lsp-ui)
(require 'lsp-ui-flycheck)
(require 'company-lsp)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(push 'company-lsp company-backends)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst athenacle|js-ts-server "/home/wangxiao/gits/javascript-typescript-langserver/lib/language-server-stdio.js")
(defconst athenacle|go-server "/home/wangxiao/.go/bin/go-langserver" )
(defconst athenacle|node-bin "node")
(defconst athenacle|ccls-path "~/.local/bin/ccls")
(defconst athenacle|pyls-path "~/.local/bin/pyls")
(defconst athenacle|jdt-path "~/gits/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; functions begin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun athenacle|spacemacs-enabled()
  "Check whether spacemacs has enabled."
  (fboundp 'spacemacs/init))

(defun athenacle|make-renderer (mode)
  "Make renderer for MODE."
  `(lambda (str)
     (with-temp-buffer
       (delay-mode-hooks (,(intern (format "%s-mode" mode))))
       (insert str)
       (font-lock-ensure)
       (buffer-string))))

(defun athenacle|get-root (stop-name)
  "Get root function.
STOP-NAME Assume as root if is file is found."
  (lsp-make-traverser #'(lambda (dir) (directory-files dir nil stop-name))))


(cl-defmacro athenacle|start-lsp(mode &key (start nil) (before nil) (after nil))
  "Main start-lsp macro
MODE: enable LSP for major mode
START: LSP starting function. If it is `nil', a default function such as `lsp-go-enable' is called.
BEFORE: function that should be called before `START'
AFTER: function called after `START'
."
  (when debug-on-error
    (message "enable start-lsp: mode: %s, start: %s, before: %s, after: %s" mode start before after))
  (setq before (if before before #'(lambda())))
  (setq after (if after after #'(lambda())))
  (setq mode-hook-name (intern (format "%s-hook" mode)))
  (setq start-func-name (if start start (intern (format "lsp-%s-enable" (car (split-string (symbol-name mode) "-"))))))
  `(add-hook (quote ,mode-hook-name)
             (lambda() (progn
                         (,before)
                         ;;                         (athenacle|start-lsp-mode (quote,mode))
                         (with-eval-after-load 'lsp-mode
                           (require 'lsp-ui-flycheck))
                         (flycheck-mode t)
                         (when (athenacle|spacemacs-enabled)
                           (spacemacs|add-company-backends :modes ,mode :backends company-lsp)
                           (spacemacs|diminish lsp-mode "  Ⓛ " " L ")
                           (spacemacs/declare-prefix-for-mode (quote ,mode) "mn" "lsp tools")
                           (spacemacs/set-leader-keys-for-major-mode  (quote ,mode)
                             "nd" 'lsp-ui-peek-find-definitions
                             "nr" 'lsp-ui-peek-find-references
                             "ns" 'lsp-ui-peek-find-workspace-symbol
                             "nc" 'athenacle-lsp/clear-buffer-delimiters))
                         (,start-func-name)
                         (,after)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; functions end

(use-package company-lsp
  :defer t
  :init
  (setq company-quickhelp-delay 0)
  ;; Language servers have better idea filtering and sorting,
  ;; don't filter results on the client side.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil)
  (spacemacs|add-company-backends :backends company-lsp :modes c-mode-common))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; clients

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go

(lsp-define-stdio-client
 lsp-go
 "go"
 (lsp-make-traverser #'(lambda (dir)
                         (directory-files dir nil "main.go")))
 `(,athenacle|go-server, "-mode=stdio")
 :initialize
 (lambda (client)
   (lsp-provide-marked-string-renderer client "go" (athenacle|make-renderer "go")))
 :ignore-regexps
 '("^langserver-go: reading on stdin, writing on stdout$"))

(athenacle|start-lsp go-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js-ts

(lsp-define-stdio-client
 lsp-javascript-typescript
 "javascript"
 (lsp-make-traverser #'(lambda (dir)
                         (directory-files
                          dir
                          nil
                          "package.json")))
 `(,athenacle|node-bin, athenacle|js-ts-server)
 :initialize
 (lambda (client)
   (lsp-provide-marked-string-renderer client "javascript" (athenacle|make-renderer "javascript")))
 :ignore-messages '("readFile .*? requested by TypeScript but content not available"))

(athenacle|start-lsp js2-mode :start lsp-javascript-typescript-enable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; java
(use-package lsp-java
  :defer t
  :config
  (progn
    (setq lsp-java-server-install-dir athenacle|jdt-path)))

(athenacle|start-lsp java-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cquery

(use-package ccls
  :defer t
  :init
  (progn
    (setq ccls-executable athenacle|ccls-path)
    (setq ccls-extra-init-params '(:enableComments 0 :cacheFormat "msgpack"))))

(defun athenacle|lsp-ccls-toggle-cquery-code-length()
  "Toggle code length."
  (interactive)
  (if (get 'ccls-code-lens-mode 'codelens-state)
      (progn
        (ccls-clear-code-lens)
        (put 'ccls-code-lens-mode 'codelens-state nil))
    (progn
      (ccls-request-code-lens)
      (put 'ccls-code-lens-mode 'codelens-state t))))

(defun athenacle/lsp-ccls-clear-buffer-delimiters()
  "Clear Delitmiters."
  (interactive)
  (rainbow-delimiters-mode-disable)
  (rainbow-delimiters-mode-enable))

(defun athenacle|cquery-post-init (mode)
  "Post-init funtion for MODE."
  (put 'ccls-code-lens-mode 'code-length-state nil)
  (when (athenacle|spacemacs-enabled)
    (progn
      (spacemacs|diminish cquery-code-lens-mode " ☪ " "LEN")
      (spacemacs/set-leader-keys-for-major-mode mode
        "nl" 'athenacle|lsp-ccls-toggle-cquery-code-length))))

(athenacle|start-lsp c-mode :start lsp-ccls-enable :after (lambda () (athenacle|cquery-post-init 'c-mode)))
(athenacle|start-lsp c++-mode :start lsp-ccls-enable :after (lambda () (athenacle|cquery-post-init 'c++-mode)))
(athenacle|start-lsp objc-mode :start lsp-ccls-enable :after (lambda () (athenacle|cquery-post-init 'objc-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python

(lsp-define-stdio-client
 lsp-python
 "python"
 (lsp-make-traverser #'(lambda (dir)
                         (directory-files dir nil "\\(__init__\\|setup\\)\\.py")))
 `(,athenacle|pyls-path))

(athenacle|start-lsp python-mode)

;;;;;; end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-lsp)
;;; init-lsp.el ends here

