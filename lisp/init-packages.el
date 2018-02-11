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

(defalias 'athenacle|package-used 'configuration-layer/package-used-p)
(defalias 'athenacle|layer-used 'configuration-layer/layer-used-p)

;;;###autoload
(defun athenacle|setup-if-enabled (test package-type package func)
  "Apply PACKAGE argument to the TEST function, call FUNC if TEST return t."
  (if (funcall test package)
      (progn
        (message "Athenacle Setting %s: %s." package-type package)
        (funcall func))
    (message "Athenacle %s %s not enabled" package-type package)))

(defun athenacle|setup-if-package-used (package func)
  "Call FUNC if PACKAGE is enabled."
  (athenacle|setup-if-enabled #'athenacle|package-used "package" package func))

(defun athenacle|setup-if-layer-used (layer func)
  "Call FUNC if LAYER is enabled."
  (athenacle|setup-if-enabled #'athenacle|layer-used "layer" layer func))

(defvar athenacle|setup-layer-lists '())
(defvar athenacle|setup-package-lists '())

(defun athenacle|add-packages-to-load-path ()
  (let ((package-directory (expand-file-name "~/.spacemacs.d/packages")))
    (seq-do
     (lambda(dir)
       (let ((full-path (concat package-directory "/" dir)))
         (when (and (file-directory-p full-path) (not (string-prefix-p "." dir)))
           (add-to-list 'load-path full-path))))
     (directory-files package-directory))))

(require 'use-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; helm-xref package
(add-to-list 'athenacle|setup-package-lists
             '(helm-xref . (lambda ()
                             (setq xref-show-xrefs-function 'helm-xref-show-xrefs))))

;;; nameless-mode package
(add-to-list 'athenacle|setup-package-lists
             '(nameless . (lambda ()
                            (add-to-list 'nameless-global-aliases '("ATH" . "athenacle"))
                            (add-hook 'emacs-lisp-mode-hook #'nameless-mode))))


;;; neotree package
(defvar athenacle|buffer-root-directory nil "Global Buffer Root directory as nil, after press <f3>, this variable will be buffer-local.")

(add-to-list 'athenacle|setup-package-lists
             '(neotree . (lambda ()
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
                                  (neotree-dir athenacle|buffer-root-directory))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup packages end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup layers begin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; chinese layer
(add-to-list 'athenacle|setup-layer-lists
             '(chinese . (lambda ()
                           (spacemacs//set-monospaced-font
                            "Source Code Pro for Powerline"
                            "Noto Sans Mono CJK SC"
                            15
                            16))))


;;; rcirc layer
(add-to-list 'athenacle|setup-layer-lists
             '(rcirc . (lambda ()
                         (add-hook 'rcirc-mode-hook
                                   (lambda ()
                                     "This function read rcirc credentials in other file named private_things."
                                     (require 'privacy_things)
                                     (setq rcirc-server-alist athenacle|rcirc-server))))))


;;; elfeed layer
(add-to-list 'athenacle|setup-layer-lists
             '(elfeed . (lambda ()
                          (use-package elfeed
                            :bind
                            (:map elfeed-search-mode-map ("R" . elfeed-search-untag-all-unread))))))
;; (require 'elfeed-search)
;; (define-key elfeed-search-mode-map (kbd "R")
;;   (lambda ()
;;     "Mark all elfeed threads as read."
;;     (interactive)
;;     (mark-whole-buffer)
;;     (elfeed-search-untag-all-unread))))))


;;; git layer
(add-to-list 'athenacle|setup-layer-lists
             '(git . (lambda ()
                       (spacemacs/declare-prefix "gz" "others")
                       (spacemacs/set-leader-keys
                         "gzp" 'magit-pull-popup
                         "gzf" 'magit-fetch-popup
                         "gzP" 'magit-push-popup))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup layers end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine them in one
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun athenacle|setup-packages()
  "Setup PACKAGES."
  (athenacle|add-packages-to-load-path)
  (dolist (pf athenacle|setup-package-lists)
    (athenacle|setup-if-package-used (car pf) (cdr pf)))
  (dolist (pf athenacle|setup-layer-lists)
    (athenacle|setup-if-layer-used (car pf) (cdr pf))))

(provide 'init-packages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-packages.el ends here
