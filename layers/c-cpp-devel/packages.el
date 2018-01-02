;;; packages.el --- C/C++ Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq c-cpp-devel-packages
  '(
    cc-mode
    disaster
    clang-format
    cmake-mode
    company
    (company-c-headers :toggle (configuration-layer/package-usedp 'company))
    flycheck
    gdb-mi
    ggtags
    helm-cscope
    helm-gtags
    semantic
    srefactor
    stickyfunc-enhance
    xcscope

    irony
    flycheck-irony
    company-irony
    ))

(defun c-cpp-devel/init-irony()
  (use-package irony
     :defer t
     :init
     (progn
       (add-hook 'c++-mode-hook 'irony-mode)
       (add-hook 'c-mode-hook 'irony-mode)
       (add-hook 'objc-mode-hook 'irony-mode)
       (add-hook 'irony-mode-hook
                 (lambda ()
                   (define-key irony-mode-map [remap completion-at-point]
                     'irony-completion-at-point-async)
                   (define-key irony-mode-map [remap complete-symbol]
                     'irony-completion-at-point-async)))
       (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
       (spacemacs|diminish irony-mode " Ⓘ" " I"))))

(defun c-cpp-devel/init-ggtags()
  (use-package ggtags
    :defer t
    :init
    (progn
      (spacemacs|diminish ggtags-mode " Ⓖ" " G"))))

(defun c-cpp-devel/init-company-irony()
  (use-package company-irony
     :defer t
     :init
     (progn
       (eval-after-load 'company
         '(add-to-list 'company-backends 'company-irony))
       (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
       (add-hook 'irony-mode-hook 'company-mode))))

(defun c-cpp-devel/init-flycheck-irony()
  (use-package flycheck-irony
    ;; :defer t                            ; fix this ???
    :init
    (progn
      (eval-after-load 'flycheck
        '(add-to-list 'flycheck-checkers 'irony))
      (add-hook 'irony-mode-hook 'flycheck-mode))))

(defun c-cpp-devel/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist
                   `("\\.h\\'" . ,c-cpp-devel-default-mode-for-headers)))
    :config
    (progn
      (require 'compile)
      (c-toggle-auto-newline 1)
      (c-toggle-auto-hungry-state 1)
      (setq-default c-basic-offset 4)
      (setq-default tab-width 4)
      (setq-default indent-tabs-mode nil)
      (spacemacs/set-leader-keys-for-major-mode 'c-mode
        "ga" 'projectile-find-other-file
        "gA" 'projectile-find-other-file-other-window)
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "ga" 'projectile-find-other-file
        "gA" 'projectile-find-other-file-other-window))))

(defun c-cpp-devel/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'c-mode
        "D" 'disaster)
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "D" 'disaster))))

(defun c-cpp-devel/init-clang-format ()
  (use-package clang-format
    :if c-cpp-devel-enable-clang-support
    :init
    (progn (c-cpp-devel/clang-format-startup))))

(defun c-cpp-devel/init-cmake-mode ()
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
    :init (push 'company-cmake company-backends-cmake-mode)))


(defun c-cpp-devel/post-init-company ()
  (spacemacs|add-company-hook c-mode-common)
  (spacemacs|add-company-hook cmake-mode)
  (when c-cpp-devel-enable-clang-support
    (push 'company-clang company-backends-c-mode-common)
    (defun company-mode/more-than-prefix-guesser ()
      (c-cpp-devel/load-clang-args)
      (company-clang-guess-prefix))
    (setq company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser)
    (spacemacs/add-to-hooks 'c-cpp-devel/load-clang-args '(c-mode-hook c++-mode-hook))))

(defun c-cpp-devel/init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (push 'company-c-headers company-backends-c-mode-common)))

(defun c-cpp-devel/post-init-flycheck ()
  (dolist (mode '(c-mode c++-mode))
    (spacemacs/add-flycheck-hook mode))
  (when c-cpp-devel-enable-clang-support
    (spacemacs/add-to-hooks 'c-cpp-devel/load-clang-args '(c-mode-hook c++-mode-hook))))

(defun c-cpp-devel/post-init-ggtags ()
  (add-hook 'c-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
  (add-hook 'c++-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun c-cpp-devel/init-gdb-mi ()
  (use-package gdb-mi
    :defer t
    :init
    (setq
     ;; use gdb-many-windows by default when `M-x gdb'
     gdb-many-windows t
     ;; Non-nil means display source file containing the main routine at startup
     gdb-show-main t)))

(defun c-cpp-devel/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'c-mode)
  (spacemacs/helm-gtags-define-keys-for-mode 'c++-mode))

(defun c-cpp-devel/post-init-semantic ()
  (spacemacs/add-to-hooks 'semantic-mode '(c-mode-hook c++-mode-hook)))

(defun c-cpp-devel/post-init-srefactor ()
  (spacemacs/set-leader-keys-for-major-mode 'c-mode "r" 'srefactor-refactor-at-point)
  (spacemacs/set-leader-keys-for-major-mode 'c++-mode "r" 'srefactor-refactor-at-point)
  (spacemacs/add-to-hooks 'spacemacs/lazy-load-srefactor '(c-mode-hook c++-mode-hook)))

(defun c-cpp-devel/post-init-stickyfunc-enhance ()
  (spacemacs/add-to-hooks 'spacemacs/lazy-load-stickyfunc-enhance '(c-mode-hook c++-mode-hook)))

(defun c-cpp-devel/pre-init-xcscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (dolist (mode '(c-mode c++-mode))
      (spacemacs/set-leader-keys-for-major-mode mode "gi" 'cscope-index-files))))

(defun c-cpp-devel/pre-init-helm-cscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (dolist (mode '(c-mode c++-mode))
      (spacemacs/setup-helm-cscope mode))))

