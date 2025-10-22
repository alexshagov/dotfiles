;;; my-utils.el --- A collection of personal utility functions for Emacs

;;;---------------------------------------------------------------------
;;; Go to Definition
;;;---------------------------------------------------------------------
(require 'my-go-to-def)
(global-set-key (kbd "M-.") 'my/rg-find-definition-at-point)

;;;---------------------------------------------------------------------
;;; Window switching
;;;---------------------------------------------------------------------

(defun my/select-window-by-number ()
  "Select a window by its geometric number (1-9).

Windows are numbered 1, 2, 3... from top-to-bottom,
left-to-right.

If only two windows are present (the current one and one other),
this command behaves like `other-window` and switches immediately."
  (interactive)
  (let* (;; Get all windows, filter out minibuffer
         (windows (delete (minibuffer-window) (window-list nil 0)))

         (sorted-windows
          (sort windows
                (lambda (w1 w2)
                  (let* (;; Get (LEFT TOP RIGHT BOTTOM)
                         (edges1 (window-edges w1))
                         (edges2 (window-edges w2))
                         ;; Get TOP coordinate
                         (top1 (nth 1 edges1))
                         (top2 (nth 1 edges2))
                         ;; Get LEFT coordinate
                         (left1 (nth 0 edges1))
                         (left2 (nth 0 edges2)))

                    ;; Compare by top coordinate first
                    (if (< top1 top2)
                        t ;; w1 is higher, so it comes first
                      (if (> top1 top2)
                          nil ;; w1 is lower, it comes second
                        ;; They are on the same row, so compare by left
                        (< left1 left2)))))))

         (count (length sorted-windows)))

    (cond
     ;; Case 1: Only one window. Do nothing.
     ((< count 2)
      (message "Only one window."))

     ;; Case 2: Exactly two windows. Just toggle.
     ((= count 2)
      (other-window 1))

     ;; Case 3: 3 or more windows. Prompt for a number.
     (t
      (message "Select window (1-%d):" (min count 9))
      (let* (;; Read a single character from the user
             (key (read-char-exclusive))
             ;; Convert the char '1' (ASCII 49) to index 0, '2' to 1, etc.
             (index (- key ?0 1)))

        ;; Check if the number is valid
        (if (and (>= index 0) (< index count))
            (select-window (nth index sorted-windows))
          (message "Invalid window: %c" key)))))))

;; Bind this new function to M-o
(global-set-key (kbd "M-o") 'my/select-window-by-number)

(provide 'my-utils)

;;; my-utils.el ends here

