;;; packages.el --- athenacle-lsp layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: WangXiao <wangxiao@Gentoo-WangXiao>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:


;;; Code:
(defconst athenacle-lsp-packages
  '(
    ;; company
    company
    (company-lsp :requires company)

    ;;; lsp-client
    lsp-mode
    flycheck
    lsp-ui

    ;;; lsp-server
    (cquery :location local :toggle (configuration-layer/layer-usedp 'c-c++))
    (lsp-python :location local :toggle (configuration-layer/layer-usedp 'python))
    (lsp-java :toggle (configuration-layer/layer-usedp 'java))
    ))


(defun athenacle-lsp/init-lsp-java()
  (require 'lsp-java)
  (setq lsp-java-server-install-dir athenacle-lsp/jdt-path))

(defun athenacle-lsp/init-lsp-python()
  (require 'lsp-python)
  (setq pyls-path athenacle-lsp/pyls-path))

(defun athenacle-lsp/init-lsp-mode()
  (use-package lsp-mode
    :defer t
    :init
    (progn (require 'lsp-flycheck))
    :config
    (progn
      (add-hook 'c-mode-hook #'athenacle-lsp/start-lsp-cquery)
      (add-hook 'c++-mode-hook #'athenacle-lsp/start-lsp-cquery)
      (add-hook 'objc-mode-hook #'athenacle-lsp/start-lsp-cquery)
      (add-hook 'python-mode-hook #'lsp-python-enable)
      (add-hook 'java-mode-hook #'lsp-java-enable))))

(defun athenacle-lsp/post-init-lsp-mode ()
  (spacemacs|diminish lsp-mode "  Ⓛ " " L "))

(defun athenacle-lsp/init-company-lsp()
  (use-package company-lsp
    :defer t
    :init
    (progn
      (spacemacs|add-company-backends :backends company-lsp :modes c-mode-common)
      (spacemacs|add-company-backends :backends company-lsp :modes python-mode)
      (spacemacs|add-company-backends :backends company-lsp :modes java-mode))))

(defun athenacle-lsp/init-lsp-ui()
  (use-package lsp-ui
    :defer t
    :init
    (progn
      (require 'lsp-ui)
      (require 'lsp-ui-peek)
      (add-hook 'lsp-mode-hook 'lsp-ui-mode)
      (define-key lsp-ui-peek-mode-map (kbd "k") 'lsp-ui-peek--select-prev)
      (define-key lsp-ui-peek-mode-map (kbd "j") 'lsp-ui-peek--select-next)
      (define-key lsp-ui-peek-mode-map (kbd "K") 'lsp-ui-peek--select-prev-file)
      (define-key lsp-ui-peek-mode-map (kbd "J") 'lsp-ui-peek--select-next-file)
      (define-key lsp-ui-peek-mode-map (kbd "g") 'lsp-ui-peek--select)
      (define-key lsp-ui-peek-mode-map (kbd "c") 'lsp-ui-peek--abort))))

(defun athenacle-lsp/init-cquery()
  (use-package cquery
    :init
    (progn
      (setq cquery-executable athenacle-lsp/cquery-path)
      (when athenacle-lsp/cquery-additional-argunemts
        (setq cquery-additional-arguments athenacle-lsp/cquery-addition-arguments)))))

(defun athenacle-lsp/post-init-cquery ()
  (dolist (mode '(c-mode c++-mode objc-mode))
    (spacemacs/set-leader-keys-for-major-mode mode
      "nl" 'athenacle-lsp/toggle-cquery-code-length)))

(defun athenacle-lsp/post-init-lsp-ui()
  (dolist (mode '(c-mode c++-mode objc-mode python-mode java-mode))
    (spacemacs/declare-prefix-for-mode mode "mn" "lsp tools")
    (spacemacs/set-leader-keys-for-major-mode mode
      "nd" 'lsp-ui-peek-find-definitions
      "nr" 'lsp-ui-peek-find-references
      "ns" 'lsp-ui-peek-find-workspace-symbol
      "nc" 'athenacle-lsp/clear-buffer-delimiters)))

(defun athenacle-lsp/post-init-flycheck()
  )

(defun athenacle-lsp/post-init-company()
  )

;;; packages.el ends here
