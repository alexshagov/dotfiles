;;; my-go-to-def.el --- Custom go-to-definition functionality using rg

(require 'rg)
(require 'thingatpt)

(defun my/ruby-find-definition ()
  "Find the definition of the Ruby symbol at point or the selected region.

1. If a region is selected, search for that text exactly.
2. If no region, detect the symbol at point.
   - If it looks like a class/module, search for 'class X' OR 'X'.
   - If it looks like a method def, search for 'def X' OR 'X'.
   - Fallback to the plain symbol."
  (interactive)
  (cond
   ;; CASE 1: Active Region (Selected Text)
   ((use-region-p)
    (let ((search-term (buffer-substring-no-properties (region-beginning) (region-end))))
      ;; Deactivate the mark so the highlighting disappears after search starts
      (deactivate-mark)
      (when (> (length search-term) 0)
        ;; Search for the literal string. We use regexp-quote to handle
        ;; special ruby chars like '::', '?', or '!' correctly.
        ;; We generally want word boundaries for definitions, but for manual selections,
        ;; we usually trust the user selected exactly what they wanted.
        (rg-project (regexp-quote search-term) "ruby"))))

   ;; CASE 2: Smart Logic (Symbol at point)
   (t
    (let* ((sym (thing-at-point 'symbol t)))
      (if (not sym)
          (message "No symbol at point")
        ;; If symbol is found, proceed with your custom logic
        (let* ((sym-quoted (regexp-quote sym))
               ;; This is the default/fallback pattern
               (default-pat (format "\\b%s\\b" sym-quoted))
               (pat
                (cond
                 ;; A. Check for Constant (starts with uppercase)
                 ((string-match-p "\\`[A-Z][A-Za-z0-9_:]*\\'" sym)
                  (let ((specific-pat (format "\\b\\s*(class|module)\\s+%s\\b" sym-quoted)))
                    (format "(%s)|(%s)" specific-pat default-pat)))

                 ;; B. Check if it's a method call (followed by optional whitespace and '(')
                 ((save-excursion (skip-chars-forward " \t") (looking-at "("))
                  (let ((specific-pat (format "\\b\\s*def\\s+(self\\.)?%s\\b" sym-quoted)))
                    (format "(%s)|(%s)" specific-pat default-pat)))

                 ;; C. Check if it's a method call with a receiver (e.g., 'foo.bar')
                 ((save-excursion
                    (let ((bounds (bounds-of-thing-at-point 'symbol)))
                      (when bounds
                        (goto-char (car bounds))
                        (looking-back "\\b[A-Za-z0-9_:]+\\.\\s-*" (line-beginning-position)))))
                  (let ((specific-pat (format "\\b\\s*def\\s+(self\\.)?%s\\b" sym-quoted)))
                    (format "(%s)|(%s)" specific-pat default-pat)))

                 ;; D. Default
                 (t default-pat))))
          ;; Run rg with the constructed pattern
          (rg-project pat "ruby")))))))

(defun my/rg-find-definition-at-point ()
  "Find definition at point, with special handling for Ruby mode."
  (interactive)
  (cond
   ;; Ruby mode uses the advanced function above
   ((derived-mode-p 'ruby-mode)
    (my/ruby-find-definition))
   
   ;; Default case for all other modes
   (t
    (let* ((search-term
            (cond
             ;; 1. Use active region
             ((use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end)))
             ;; 2. Use symbol at point
             (t
              (thing-at-point 'symbol t)))))
       
      (if (and search-term (> (length search-term) 0))
          (rg-project (format "\\b%s\\b" (regexp-quote search-term)) "*")
        (message "No symbol or active region at point"))))))

(provide 'my-go-to-def)
;;; my-go-to-def.el ends here
