

(defun athenacle|keybingding-magit()
  (when (configuration-layer/layer-used-p 'git)
    (spacemacs/declare-prefix "gz" "others")
    (spacemacs/set-leader-keys
      "gzp" 'magit-pull-popup
      "gzf" 'magit-fetch-popup
      "gzP" 'magit-push-popup)
    ))



(athenacle|keybingding-magit)

(provide 'init-keybinds)
