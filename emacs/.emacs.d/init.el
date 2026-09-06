;;; init.el -*- lexical-binding: t; -*-

;;----------------------------------------------------------------------------
;; 1. Core Emacs Behavior & UI Tweaks
;;----------------------------------------------------------------------------
(setq load-prefer-newer t)
(menu-bar-mode -1)
(column-number-mode 1)
(setq column-number-indicator-zero-based nil)
(global-auto-revert-mode t)

;;----------------------------------------------------------------------------
;; 2. Package Management Bootstrap (package.el + use-package)
;;----------------------------------------------------------------------------
(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; Auto-install packages declared with use-package
(setq use-package-always-ensure t)

;;----------------------------------------------------------------------------
;; 3. Built-in Feature Configuration
;;----------------------------------------------------------------------------
;; Fido in the minibuffer; corfu + orderless handle in-buffer completion.
(fido-mode 1)
(fido-vertical-mode 1)

;; Repeat Mode: repeat commands via a transient keymap
(repeat-mode 1)
(setq repeat-exit-timeout 3)

;; Terminal Emacs clipboard support via Ghostty's OSC 52
(setq xterm-extra-capabilities
      '(getSelection setSelection))

;; Winner Mode: undo/redo for window configurations
(winner-mode 1)

;;----------------------------------------------------------------------------
;; 4. External Package Configuration (via use-package)
;;----------------------------------------------------------------------------

;; KKP lets terminal Emacs disambiguate modified keys (e.g. C-i vs TAB)
;; in KKP-capable terminals like Ghostty.
(use-package kkp
  :ensure t
  :config
  (global-kkp-mode +1))

(use-package ghostel
  :ensure t)

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package fzf
  :bind (("C-c f" . my-fzf-git-files))
  :config
  (setq fzf/position-bottom t
        fzf/window-height 13)
  (defun my-fzf-git-files ()
    "Run fzf-git-files from a non-side window (Treemacs workaround)."
    (interactive)
    (if (window-parameter (selected-window) 'window-side)
        (select-window (get-window-with-predicate
                        (lambda (w) (not (window-parameter w 'window-side))))))
    (fzf-git-files))

  (add-to-list 'display-buffer-alist
               '("\\*fzf\\*"
                 (display-buffer-reuse-window
                  display-buffer-at-bottom)
                 (window-height . 0.3))))

(use-package rg
  :hook (after-init . rg-enable-default-bindings))

(use-package avy
  :bind (("M-s" . avy-goto-char-timer)
         ("M-l" . avy-goto-line)))

(use-package expand-region
  :bind (("M-2" . er/expand-region)))

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t t"   . treemacs)
        ("C-x t C-t" . treemacs-find-file))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-mode 'deferred))

;; --- COMPLETIONS ---

;; Corfu (UI)
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match 'separator)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

;; Orderless (filtering)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (orderless-matching-styles '(orderless-literal orderless-flex)))

;; Cape (backends)
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :config
  (setq cape-dabbrev-check-other-buffers t))

;; Popup in terminal Emacs (< 31)
(use-package corfu-terminal
  :ensure t
  :after corfu
  :config
  (corfu-terminal-mode +1))

;; --- END COMPLETIONS ---

;; --- Major Modes + Tree-sitter grammars ---

;; Ruby: grammar source and install location
(setq treesit-language-source-alist
      '((ruby "https://github.com/tree-sitter/tree-sitter-ruby")))

(add-to-list 'treesit-extra-load-path
             (expand-file-name "tree-sitter" user-emacs-directory))

;; Install missing grammars at startup
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (dolist (lang (mapcar #'car treesit-language-source-alist))
    (unless (treesit-language-available-p lang)
      (condition-case err
          (progn
            (make-directory (expand-file-name "tree-sitter" user-emacs-directory) t)
            (treesit-install-language-grammar lang))
        (error
         (message "treesit install failed for %s: %s" lang err))))))

(add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;----------------------------------------------------------------------------
;; 5. Custom Lisp Code
;;----------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; my-utils may not exist on a fresh clone, so guard the require.
(let ((my-utils-file (expand-file-name "lisp/my-utils.el" user-emacs-directory)))
  (when (file-exists-p my-utils-file)
    (require 'my-utils)))

;;----------------------------------------------------------------------------
;; 6. Global Key Remaps & Aliases
;;----------------------------------------------------------------------------
;; Use ibuffer (a better buffer list) instead of the default
(defalias 'list-buffers 'ibuffer)

;; Use hippie-expand (more powerful) instead of dabbrev-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;;----------------------------------------------------------------------------
;; 7. Emacs Customize Block (Auto-Generated)
;;----------------------------------------------------------------------------
;; This section is managed by the Emacs 'customize' interface.
;; It should remain at the end of your init file.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(leuven-dark))
 '(package-selected-packages nil)
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
