(setq valgrind-highlights
      '(("^==[0-9]*== " . font-lock-function-name-face)
        ("Thread #[0-9]+ .*$" . font-lock-comment-face)
        ("  at " . font-lock-keyword-face)
        ("0x[0-9A-Z]*" . font-lock-string-face)
        ))

(define-derived-mode valgrind-mode fundamental-mode "valgrind"
  "major mode for editing mymath language code."
  (setq font-lock-defaults '(valgrind-highlights)))
