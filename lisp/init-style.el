

(defun athenacle|cc-mode-settings()
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))

(defun athenacle|c-modes-hooks()
  (add-hook 'c-mode-hook 'athenacle|cc-mode-settings)
  (add-hook 'c++-mode-hook 'athenacle|cc-mode-settings)
  (add-hook 'objc-mode-hook 'athenacle|cc-mode-settings))

(defun athenacle|init-style ()
  (athenacle|c-modes-hooks)
  (setq powerline-default-separator 'arrow)
  (when (configuration-layer/layer-usedp 'chinese)
  (spacemacs//set-monospaced-font   "Source Code Pro for Powerline" "Noto Sans Mono CJK SC" 15 16)))

(provide 'init-style)

