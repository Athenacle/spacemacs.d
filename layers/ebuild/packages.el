;;; packages.el --- ebuild layer packages file for Spacemacs.
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

(defconst ebuild-packages
  '(
    (ebuild-mode :location local)
    ))

(defun ebuild/init-ebuild-mode()
  (use-package ebuild-mode
    :defer t
    :mode ("\\.ebuild\\'" . ebuild-mode)
    :config
    (progn
      (add-hook 'ebuild-mode-hook 'ebuild-eldoc|init)
      (spacemacs/declare-prefix-for-mode 'ebuild-mode "mi" "insert")
      (spacemacs/set-leader-keys-for-major-mode 'ebuild-mode
        "is" 'ebuild/insert-skeleton
        "ia" 'ebuild/insert-all-functions
        "if" 'ebuild/insert-function)
      (spacemacs/declare-prefix-for-mode 'ebuild-mode "mx" "execute ebuild")
      (spacemacs/set-leader-keys-for-major-mode 'ebuild-mode
        "xx" 'ebuild-run-command
        "xd" 'ebuild/execute-digest
        "xf" 'ebuild/execute-fetch
        "xc" 'ebuild/execute-configure
        "xb" 'ebuild/execute-compile))
    )
  (use-package glep-mode
    :defer t
    :mode  ("/glep.*\\.rst\\'" . glep-mode))
  (use-package gentoo-newsitem-mode
    :defer t
    :mode ("/[0-9]\\{4\\}-[01][0-9]-[0-3][0-9]-.+\\.[a-z]\\{2\\}\\.txt\\'" . gentoo-newsitem-mode)
  ))



;;; packages.el ends here
