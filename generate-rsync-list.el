
;;; Code:

(require 'seq)

;;; get .emacs.d abslute path and output file from argv

(when (not (= (length argv) 2))
  (message "%s\n%s" "Argument error."
           "Usage: emacs -Q --script generate-rsync-list.el <path to .emacs.d> <dest file>")
  (kill-emacs 255))

(defvar athenacle|emacs-d-path (elt argv 0))
(defvar athenacle|dest-file-path (elt argv 1))

(defconst spacemacs-layer-directory (expand-file-name (concat athenacle|emacs-d-path "/layers")))

(unless (file-directory-p spacemacs-layer-directory)
  (message "PATH %s dot exist." spacemacs-layer-directory)
  (kill-emacs 255))

(message ".emacs.d true path: %s" athenacle|emacs-d-path) ;; .emacs.d true path
(message "layer directroy list file path: %s" athenacle|dest-file-path) ;; output dest file path

;;; get layer list from spacemacs configuration file.
(defvar dotspacemacs-configuration-layers)
(defconst dotspacemacs-configuration-file-home (expand-file-name "~/.spacemacs"))
(defconst dotspacemacs-configuration-file-dotspacmacs (expand-file-name "~/.spacemacs.d/init.el"))

(defconst dotspacemacs-configuration-file
  (if (file-exists-p dotspacemacs-configuration-file-home)
      dotspacemacs-configuration-file-home
    dotspacemacs-configuration-file-dotspacmacs))

(load-file dotspacemacs-configuration-file)
(dotspacemacs/layers)
(defvar dotspacemacs-layers (seq-map (lambda(n) (if (listp n) (car n) n)) dotspacemacs-configuration-layers))

;;; get all layer categories from spacemacs


(defvar spacemacs-layer-cates
  (seq-filter
   (lambda (f)
     (and  (string-prefix-p "+" f)
           (file-directory-p (expand-file-name (concat spacemacs-layer-directory "/" f)))))
   (directory-files spacemacs-layer-directory)))

;;; find out each layer belongs to which category, build a list, result may like
;; (("+web-services"
;;   ("elfeed"))
;;  ("+tools"
;;   ("shell"))
;;  ("+tags"
;;   ("gtags"))
;;  ("+source-control"
;;   ("git"))
;;  ("+lang"
;;   ("shell-scripts" "scheme" "python" "lua" "javascript" "java" "go" "emacs-lisp" "c-c++"))
;;  ("+intl"
;;   ("chinese"))
;;  ("+fun"
;;   ("emoji"))
;;  ("+emacs"
;;   ("org"))
;;  ("+completion"
;;   ("helm" "auto-completion"))
;;  ("+checkers"
;;   ("syntax-checking" "spell-checking"))
;;  ("+chat"
;;   ("rcirc")))

(defvar athenacle|enabled-layers-cates '())

(dolist (cate spacemacs-layer-cates)
  (let ((layers-list '()))
    (let ((layers (directory-files (concat spacemacs-layer-directory "/" cate))))
      (dolist (layer layers)
        (when (member (intern layer) dotspacemacs-layers)
          (add-to-list 'layers-list layer))))
    (when (not (eq 0 (length layers-list)))
      (add-to-list 'athenacle|enabled-layers-cates (cons cate (list layers-list))))))

;;; generate file list output
(defvar athenacle|layers-list-output "")
(defvar athenacle|additional-list
  '(
   "layers/+distributions/spacemacs-bootstrap"
   "layers/+distributions/spacemacs"
   "layers/+distributions/spacemacs-base"
   "layers/+distributions/spacemacs-docker"
   "layers/+spacemacs/spacemacs-editing"
   "layers/+spacemacs/spacemacs-editing"
   "layers/+spacemacs/spacemacs-visual"
   "layers/+spacemacs/spacemacs-navigation"
   "layers/+spacemacs/spacemacs-org"
   "layers/+spacemacs/spacemacs-editing-visual"
   "layers/+spacemacs/spacemacs-evil"
   "layers/+spacemacs/spacemacs-layouts"
   "layers/+spacemacs/spacemacs-language"
   "layers/+spacemacs/spacemacs-purpose"
   "layers/+spacemacs/spacemacs-modeline"
   "layers/+spacemacs/spacemacs-misc"
   "layers/+spacemacs/spacemacs-completion"
   "layers/+filetree/neotree"
    ))

(seq-do
 (lambda (n)
   (let ((cate (car n)))
     (seq-do (lambda (l) (setq athenacle|layers-list-output (concat athenacle|layers-list-output (format "layers/%s/%s\n" cate l)))) (car(cdr n)))))
 athenacle|enabled-layers-cates)

(dolist (add athenacle|additional-list)
  (setq athenacle|layers-list-output (concat athenacle|layers-list-output (format "%s\n" add))))

;;; write file to dest
(write-region athenacle|layers-list-output nil athenacle|dest-file-path 'append)

(provide 'generate-rsync-list)
;;; generate-rsync-list.el ends here
