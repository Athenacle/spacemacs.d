

(defun athenacle|cc-mode-settings()
  (set-default c-basic-offset 4)
  (set-default tab-width 4)
  (set indent-tabs-mode nil))

(defun athenacle|c-modes-hooks()
  (athenacle|cc-mode-settings))

(dolist m '(c-mode-hook c++-mode-hook objc-mode-hook)
        (add-hook m #'athenacle|c-modes-hook))
