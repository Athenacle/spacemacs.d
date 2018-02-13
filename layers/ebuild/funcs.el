
(defun ebuild/execute-digest()
  (interactive)
  (ebuild-run-command "digest"))

(defun ebuild/execute-fetch()
  (interactive)
  (ebuild-run-command "fetch"))

(defun ebuild/execute-configure()
  (interactive)
  (ebuild-run-command "configure"))

(defun ebuild/execute-compile()
  (interactive)
  (ebuild-run-command "compile"))


(defconst ebuild/functions '("pkg_pretend"
                             "pkg_nofetch"
                             "src_unpack"
                             "src_prepare"
                             "src_configure"
                             "src_compile"
                             "src_test"
                             "src_install"
                             "pkg_preinst"
                             "pkg_postinst"
                             "pkg_prerm"
                             "pkg_postrm"
                             "pkg_config"
                             "pkg_info"))

(defun ebuild/insert-function(func)
  (interactive
   (list (completing-read "Insert ebuild function: "
                          (mapcar 'list ebuild/functions)
                          nil t)))
  (or (member func ebuild/functions)
      (error "ebuild function \"%s\" not known" func))
  (goto-char (point-max))
  (insert (format "\n%s {\n}\n" func)))


(defun ebuild/insert-useful-functions()
  (interactive)
  (ebuild/insert-function "src_unpack")
  (ebuild/insert-function "src_prepare")
  (ebuild/insert-function "src_configure")
  (ebuild/insert-function "src_install"))

(defun ebuild/insert-all-functions()
  (interactive)
  (dolist (f ebuild/functions)
    (ebuild/insert-function f)))

(defun ebuild/insert-skeleton()
  (interactive)
  (ebuild-mode-insert-skeleton)
  (ebuild/insert-useful-functions))

;; CATEGORY	Package's category, for example app-editors.
;; FILESDIR	Path to the ebuild's files/ directory, commonly used for small patches and files. Value: "${PORTDIR}/${CATEGORY}/${PN}/files".
;; WORKDIR	Path to the ebuild's root build directory. Value: "${PORTAGE_BUILDDIR}/work".
;; T	Path to a temporary directory which may be used by the ebuild. Value: "${PORTAGE_BUILDDIR}/temp".
;; D	Path to the temporary install directory. Value: "${PORTAGE_BUILDDIR}/image".
;; ROOT	Path to the root directory. When not using ${D}, always prepend ${ROOT} to the path.
;; DISTDIR	Contains the path to the directory where all the files fetched for the package are stored

(defun ebuild/get-info-from-etc-portage-make-conf (table)
  (when (file-exists-p "/etc/portage/make.conf")
    (let ((bash-script (make-temp-file "portage")))
      (write-region "#/bin/bash \n source /etc/portage/make.conf \n echo -n \"$PORTAGE_TMPDIR\" \"$DISTDIR\" \"$PKGDIR\"" nil bash-script 'append)
      (chmod bash-script #o755)
      (setq  dirs (shell-command-to-string bash-script))
      (setq  dirs-list (split-string dirs " "))
      (when table
        (let* ((tmp-dir (nth 0 dirs-list))
               (build-dir (expand-file-name (concat tmp-dir "/" (gethash "CATEGROY" table) "/" (gethash "PF" table)))))
          (puthash "PORTAGE_TMPDIR" tmp-dir table)
          (puthash "DISTDIR" (nth 1 dirs-list) table)
          (puthash "PKGDIR" (nth 2 dirs-list) table)
          (puthash "PORTAGE_BUILDDIR"  build-dir table)
          (puthash "WORKDIR" (expand-file-name (concat build-dir "/work")) table)
          (puthash "T" (expand-file-name (concat build-dir "/tmp")) table)
          (puthash "D" (expand-file-name (concat build-dir "/image")) table))))))

(defun ebuild/variables()
  (let* ((filename (buffer-file-name))
         (cate (file-name-base (directory-file-name (file-name-directory (directory-file-name (file-name-directory filename)))))))
    (make-local-variable 'ebuild/category)
    (make-local-variable 'ebuild/variables-table)
    (setq-local ebuild/variables-table (make-hash-table :test 'equal))
    (setq vs (ebuild/calc-V filename))
    (puthash "CATEGROY" cate ebuild/variables-table)
    (puthash "FILESDIR" (format "%sfiles" (file-name-directory filename)) ebuild/variables-table)
    (puthash "P" (nth 0 vs) ebuild/variables-table)
    (puthash "PN" (nth 1 vs) ebuild/variables-table)
    (puthash "PV" (nth 2 vs) ebuild/variables-table)
    (puthash "PR" (nth 3 vs) ebuild/variables-table)
    (puthash "PVR" (nth 4 vs) ebuild/variables-table)
    (puthash "PF" (nth 5 vs) ebuild/variables-table)
    (ebuild/get-info-from-etc-portage-make-conf ebuild/variables-table)))

;; P    Package name and version (excluding revision, if any), for example vim-6.3.
;; PN	Package name, for example vim.
;; PV	Package version (excluding revision, if any), for example 6.3. It should reflect the upstream versioning scheme.
;; PR	Package revision, or r0 if no revision exists.
;; PVR	Package version and revision (if any), for example 6.3, 6.3-r1.
;; PF	Full package name, ${PN}-${PVR}, for example vim-6.3-r1.

(defun ebuild/calc-V (name)
  (when (string-suffix-p ".ebuild" name)
    (setq name (file-name-base name)))
  (setq vs (split-string name "-"))

  (setq PN (car vs))
  (setq PV (nth 1 vs))
  (setq PR (nth 2 vs))
  (setq PF name)
  (setq P (if PV (concat PN "-" PV) PN))
  (setq PVR (if PR (concat P "-" PR) PV))
  (if (not PR) (setq PR "r0"))
  (list P PN PV PR PVR PF))

(require 'eldoc)

(defun ebuild-eldoc|documentation-function ()
  (if (thing-at-point-looking-at "\\${[[:upper:]]*}")
      (let* ((name (thing-at-point 'symbol))
             (value (gethash name ebuild/variables-table)))
        (when value
          (format "${%s}: %s" name value)))))

(defun ebuild-eldoc|init ()
  (ebuild/variables)
  (set (make-local-variable 'eldoc-documentation-function)
       #'ebuild-eldoc|documentation-function)
  (turn-on-eldoc-mode))
