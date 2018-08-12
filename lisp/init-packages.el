;;; init-packages.el --- init packages
;;
;; Filename: init-packages.el
;; Description:  init packages for spacemacs
;; Author: WangXiao
;; Copyright 2018 (c) WangXiao;; Created: Sat Jan 13 12:03:07 2018 (+0800)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; Init packages for spacemacs.
;; Spacemacs manage its package by LAYER which cannot be modified easily.
;; So, create this file for setup packages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;        1. Jan 13, 2018:
;;                  Init Version.
;;                       Include package: neotree nameless
;;                       Include layer: git chinese rcirc elfeed
;;        2. Feb 16, 2018
;;                  Refactor with athenacle||lib-macro
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'libraries)
(require 'init-lsp)


(defalias 'athenacle|package-used 'configuration-layer/package-used-p)
(defalias 'athenacle|layer-used 'configuration-layer/layer-used-p)

  "Add directories exist in ~/.spacemacs.d/packages to `load-path'."
  (let ((package-directory (expand-file-name "~/.spacemacs.d/packages")))
    (seq-do
     (lambda(dir)
       (let ((full-path (concat package-directory "/" dir)))
         (when (and (file-directory-p full-path) (not (string-prefix-p "." dir)))
           (add-to-list 'load-path full-path))))
     (directory-files package-directory))))


(ath-defmacro athenacle||add-package (layer package use-package use-package-config)
             (when (and layer package)
               (error "Package %s and layer %s cannot be set together" package layer))
             (unless (or layer package)
               (error "Must set one of `layer' or `package'"))
             (let ((pred (if package 'athenacle|package-used 'athenacle|layer-used))
                   (pkg (or package layer))
                   (type (if package "package" "layer")))
               `(when use-package
                 (use-package ,use-package
                   :defer t
                   :use-package-verbose debug-on-error
                   :config
                   (progn ,use-package-config)))
               (if (funcall pred pkg)
                   (progn
                     (athenacle||lib-message-if-debug-on-error
                      (format "Athenacle: setup %s %s." type pkg))
                     (funcall body))
                 (athenacle||lib-message-if-debug-on-error
                  (format "Athenacle: %s %s not enabled." type pkg)))))


(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(athenacle||add-package
 :package company
 :use-package company-childframe
 :use-package-config
 (progn
   (company-childframe-mode t)))

;; pyim
(athenacle||add-package
 :package pyim
 (use-package pyim
   :ensure nil
   :demand t
   :config
   (use-package pyim-basedict
     :ensure nil
     :config (pyim-basedict-enable))
   (setq default-input-method "pyim")
   (setq pyim-default-scheme 'quanpin)
   (pyim-isearch-mode 1)
   (setq pyim-page-tooltip 'posframe)
   (setq pyim-page-length 9)
   (add-hook 'emacs-startup-hook
             #'(lambda () (pyim-restart-1 t)))))


;; helm-xref package
(athenacle||add-package
 :package helm-xref
 :use-package helm-xref
 :use-package-config
   (setq xref-show-xrefs-function 'helm-xref-show-xrefs))


;;; nameless-mode package
(athenacle||add-package
 :package nameless
 (add-to-list 'nameless-global-aliases '("ATH" . "athenacle"))
 (add-hook 'emacs-lisp-mode-hook #'nameless-mode)
 (add-hook 'nameless-mode-hook
           (lambda()
             (let ((current-buffer-name (buffer-file-name)))
               (when (and current-buffer-name (string-suffix-p ".el" current-buffer-name))
                 (setq-local nameless-current-name (file-name-base current-buffer-name)))))))


;;; neotree package
(defvar athenacle|buffer-root-directory nil "Global Buffer Root directory as nil, after press <f3>, this variable will be buffer-local.")

(athenacle||add-package
 :package neotree
 (global-set-key
  (kbd "<f3>")
  (lambda ()
    "Toggle neotree according to directory root."
    (interactive)
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    ;; `neo-global--window-exists-p' and `delete-window' are stolen from `neotree-hide' function.
    (if (neo-global--window-exists-p)
        (delete-window neo-global--window)
      (progn
        (if (not athenacle|buffer-root-directory)
            (let ((tmp-dir (athenacle|lib-search-buffer-file-root)))
              (if tmp-dir
                  (setq-local athenacle|buffer-root-directory tmp-dir)
                (setq-local athenacle|buffer-root-directory default-directory))))
        (neotree-dir athenacle|buffer-root-directory))))))


;;; evil package
(athenacle||add-package
 :package evil
 (require 'evil-common)
 (require 'evil-commands)
 (defun athenacle|evil-ZQ ()
   "evil-ZQ delete "
   (interactive)
   (evil-delete-buffer (current-buffer))
   (when (athenacle|spacemacs-enabled)
     (spacemacs/home-delete-other-windows)))
 (define-key evil-normal-state-map "ZQ" 'athenacle|evil-ZQ)
 (define-key evil-normal-state-map "ZZ"
   (lambda ()
     (interactive)
     (when (buffer-modified-p)
       (save-buffer)
       (message "Buffer %s saved to disk." (buffer-file-name (current-buffer))))
     (athenacle|evil-ZQ))))


;;; rjsx-mode
(athenacle||add-package
 :package rjsx-mode
 :use-package rjsx-mode
 :use-package-config
 (when (athenacle|package-used 'company)
   (setq company-backends-rjsx-mode '())
   (add-to-list 'company-backends-rjsx-mode 'company-lsp))
 (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
 (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
 (athenacle|start-lsp rjsx-mode :start lsp-javascript-typescript-enable))


;; (use-package npm-mode
;;   :defer t
;;   :config
;;   (when (athenacle|spacemacs-enabled)
;;     (spacemacs/declare-prefix-for-mode rjsx-mode "mp" "npm tools")
;;     (spacemacs/set-leader-keys-for-major-mode rjsx-mode
;;       "pr" 'npm-mode-npm-run
;;       "pl" 'npm-mode-visit-project-file)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup packages end

;;; setup layers begin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; chinese layer
(athenacle||add-package
 :layer chinese
 (spacemacs//set-monospaced-font
  "Source Code Pro for Powerline"
  "Noto Sans Mono CJK SC"
  15
  25))


;;; rcirc layer
(athenacle||add-package
 :layer rcirc
 :use-package rcirc
 :use-package-config
 (add-hook 'rcirc-mode-hook
           (lambda ()
             "This function read rcirc credentials in other file named private_things."
             (require 'privacy_things)
             (setq rcirc-server-alist athenacle|rcirc-server))))


;;; elfeed layer
(athenacle||add-package
 :layer elfeed
 :use-package elfeed-search
 :use-package-config
 (define-key elfeed-search-mode-map (kbd "R")
   (lambda ()
     "Mark all elfeed threads as read."
     (interactive)
     (mark-whole-buffer)
     (elfeed-search-untag-all-unread))))


;;; git layer
(athenacle||add-package
 :layer git
 (spacemacs/declare-prefix "gz" "others")
 (spacemacs/set-leader-keys
   "gzp" 'magit-pull-popup
   "gzf" 'magit-fetch-popup
   "gzP" 'magit-push-popup))


;;; react layer
(athenacle||add-package
 :layer react
 (athenacle|start-lsp react-mode :start lsp-javascript-typescript-enable))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup layers end

(provide 'init-packages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-packages.el ends here
