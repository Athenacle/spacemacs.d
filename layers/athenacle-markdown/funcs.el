;;; funcs.el --- athenacle-markdown Layer Functions for Spacemacs

;;; Commentary
;;; Copyright (c) 2018 Wang Xiao
;;; License: GPLv3

;;; Code:

(defvar athenacle-markdown/start-function nil)
(defvar athenacle-markdown/browser nil)

(defun athenacle-markdown/start(url)
  (if (not athenacle-markdown/browser) (message "not browser found. flymd refuse to start.")
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           "flymd" nil
           athenacle-markdown/browser
           (list "--new-window" "--allow-file-access-from-files" url)))))

(defun athenacle-markdown/build-start-function(browser)
  "Searh brower in PATH.
BROSWER: broswer name "
  (let ((browser-exec (executable-find browser)))
    (if browser-exec (setq athenacle-markdown/browser browser-exec)
      (message "no browser named %s, please check" browser))))



;;funcs.el ends here
