;;; rcirc start

;;;###autoload
(defun athenacle|rcirc-load-credentials()
  "This function read rcirc credentials in other file named private_things."
  (require 'privacy_things)
  (setq rcirc-server-alist athenacle|rcirc-server))

;;;###autoload
(defun athenacle|rcirc-hooks()
  (when (configuration-layer/layer-usedp 'rcirc)
    (add-hook 'rcirc-mode-hook 'athenacle|rcirc-load-credentials)))

;;; rcirc end


;;; elfeed start
;;;###autoload
(defun athenacle|elfeed-mark-all-as-read ()
  "Mark all elfeed threads as read."
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

;;;###autoload
(defun athenacle|elfeed-hooks()
  "This function will be called in dotspacemacs/user-config to initial key-bindings,"
  (when (configuration-layer/layer-used-p 'elfeed)
    (add-hook 'elfeed-new-entry-hook   (lambda() (define-key elfeed-search-mode-map (kbd "R") 'athenacle/elfeed-mark-all-as-read)))))

(defun athenacle|add-hooks ()
  (athenacle|rcirc-hooks)
  (athenacle|elfeed-hooks))


(provide 'init-others)
