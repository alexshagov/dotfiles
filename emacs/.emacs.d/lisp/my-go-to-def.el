;;; my-go-to-def.el --- Custom go-to-definition functionality using rg

(require 'rg)
(require 'thingatpt)

(defun my/ruby-find-definition ()
  "Find the definition of the Ruby symbol at point.
If a specific definition pattern (class, def) is detected,
it searches for *both* that pattern AND the plain symbol as a
fallback, combined with a regex OR.
e.g., '(\\bdef my_method\\b)|(\\bmy_method\\b)'"
  (interactive)
  (let* ((sym (thing-at-point 'symbol t)))
    (if (not sym)
        (message "No symbol at point")
      ;; If symbol is found, proceed
      (let* ((sym-quoted (regexp-quote sym))
             ;; This is the default/fallback pattern
             (default-pat (format "\\b%s\\b" sym-quoted))
             (pat
              (cond
               ;; 1. Check for Constant (starts with uppercase)
               ((string-match-p "\\`[A-Z][A-Za-z0-9_:]*\\'" sym)
                (let ((specific-pat (format "\\b\\s*(class|module)\\s+%s\\b" sym-quoted)))
                  ;; Combine specific pattern with default fallback
                  (format "(%s)|(%s)" specific-pat default-pat)))

               ;; 2. Check if it's a method call (followed by optional whitespace and '(')
               ((save-excursion (skip-chars-forward " \t") (looking-at "("))
                (let ((specific-pat (format "\\b\\s*def\\s+(self\\.)?%s\\b" sym-quoted)))
                  ;; Combine specific pattern with default fallback
                  (format "(%s)|(%s)" specific-pat default-pat)))

               ;; 3. Check if it's a method call with a receiver (e.g., 'foo.bar')
               ((save-excursion
                  (let ((bounds (bounds-of-thing-at-point 'symbol)))
                    (when bounds
                      (goto-char (car bounds)) ; Go to start of symbol
                       ;; Check for 'something.' before the symbol
                      (looking-back "\\b[A-Za-z0-9_:]+\\.\\s-*" (line-beginning-position)))))
                (let ((specific-pat (format "\\b\\s*def\\s+(self\\.)?%s\\b" sym-quoted)))
                  ;; Combine specific pattern with default fallback
                  (format "(%s)|(%s)" specific-pat default-pat)))

               ;; 4. Default: Just search for the word as a whole word.
               (t
                default-pat)
               )))
        ;; Run rg with the constructed pattern
        (rg-project pat "ruby")))))

(defun my/rg-find-definition-at-point ()
  "Find definition at point, with special handling for Ruby mode."
  (interactive)
  (cond
   ;; Ruby mode uses advanced search
   ((derived-mode-p 'ruby-mode)
    (my/ruby-find-definition))
   
   ;; Default case for all other modes
   (t
    (let* ((search-term
            (cond
             ;; 1. Use active region (selected area)
             ((use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end)))
             ;; 2. Use symbol at point (as a string)
             (t
              (thing-at-point 'symbol t)))))
      
      (if (and search-term (> (length search-term) 0))
          ;; Search for the term as a whole word, properly quoted
          (rg-project (format "\\b%s\\b" (regexp-quote search-term)) "*")
        (message "No symbol or active region at point"))))))

(provide 'my-go-to-def)

;;; my-go-to-def.el ends here
