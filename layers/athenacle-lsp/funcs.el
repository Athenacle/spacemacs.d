

;;;; cquery client
(defun athenacle-lsp/start-lsp-cquery()
  (lsp-cquery-enable)
  (with-eval-after-load 'lsp-mode
    (require 'lsp-flycheck))
  (flycheck-mode t)
  (cquery-code-lens-mode t)
  (lsp-ui-peek-mode t)
  (lsp-ui-sideline-mode t)
  (put 'cquery-code-lens-mode 'codelens-state t)
  (spacemacs|diminish cquery-code-lens-mode " ☪ " "LEN"))

(defun athenacle-lsp/toggle-cquery-code-length()
  (interactive)
  (if (get 'cquery-code-lens-mode 'codelens-state)
      (progn
        (cquery-clear-code-lens)
        (put 'cquery-code-lens-mode 'codelens-state nil))
    (progn
      (cquery-request-code-lens)
      (put 'cquery-code-lens-mode 'codelens-state t))))

(defun athenacle-lsp/clear-buffer-delimiters()
  (interactive)
  (rainbow-delimiters-mode-disable)
  (rainbow-delimiters-mode-enable))

;;; lsp-python.el ends here
