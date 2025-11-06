;;; init.el -*- lexical-binding: t; -*-

;; TODO: check with benchmark-init
;; (use-package benchmark-init
;;   :ensure t ; Ensures benchmark-init is installed if not already present
;;   :init
;;   ;; Activate benchmark-init early in the startup process
;;   (benchmark-init/activate)
;;   :hook
;;   ;; Deactivate benchmark-init after Emacs has finished initializing
;;   (after-init . benchmark-init/deactivate)
;;   :config
;;   )

;;----------------------------------------------------------------------------
;; 1. Core Emacs Behavior & UI Tweaks
;;----------------------------------------------------------------------------
;; Set basic Emacs behaviors that don't require external packages.

(setq load-prefer-newer t)  ;; Prefer newer compiled files
(menu-bar-mode -1)         ;; Disable the graphical menu bar
(column-number-mode 1)     ;; Show column numbers in the mode line
(setq column-number-indicator-zero-based nil) ;; Start column count from 1
(global-auto-revert-mode t) ;; Automatically reload files changed on disk

;;----------------------------------------------------------------------------
;; 2. Package Management Bootstrap (package.el + use-package)
;;----------------------------------------------------------------------------
;; This section sets up the package system, defines archives (MELPA),
;; and ensures that the 'use-package' macro is available.

(require 'package)

;; Define package archives (GNU ELPA and MELPA)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; Don't auto-enable packages before init finishes
(setq package-enable-at-startup nil)
(package-initialize)

;; Fetch archive metadata on first run
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package (our config manager) if it's not already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Load use-package
(require 'use-package)

;; Configure use-package to auto-install missing packages
(setq use-package-always-ensure t)

;;----------------------------------------------------------------------------
;; 3. Built-in Feature Configuration
;;----------------------------------------------------------------------------
;; Configure features that come with Emacs or are commonly enabled,
;; like Ido, Winner, and Repeat modes.

;; --- Ido (Interactive Do) ---
(require 'ido)
(setq ido-enable-flex-matching t) ;; Enable flexible, fuzzy matching
(setq ido-everywhere t)           ;; Use Ido for all completions
(ido-mode t)                     ;; Enable Ido mode globally

;; --- Repeat Mode ---
(repeat-mode 1) ;; Allow repeating commands
;; Optional: Set timeout (in seconds) before repeat mode auto-exits
(setq repeat-exit-timeout 3)
;; Optional: Disable repeat-mode for specific commands if they cause issues
;; (put 'other-window 'repeat-map nil)

;; --- Winner Mode ---
(winner-mode 1) ;; Enable undo/redo for window configurations

;;----------------------------------------------------------------------------
;; 4. External Package Configuration (via use-package)
;;----------------------------------------------------------------------------
;; This is the main section for adding and configuring third-party packages.

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package fzf
  :bind (("C-c f" . fzf-git-files))
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

(use-package rg
  :hook (after-init . rg-enable-default-bindings))

(use-package vterm)

(use-package avy
  :bind (("M-s" . avy-goto-char-timer)
         ("M-l" . avy-goto-line)))

(use-package expand-region
  :bind (("M-2" . er/expand-region)))

;; --- Major Modes ---
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;----------------------------------------------------------------------------
;; 5. Custom Lisp Code
;;----------------------------------------------------------------------------
;; Load personal Emacs Lisp files from a local directory.

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load `my-utils`. Guard with file existence to avoid errors
;; on fresh clones where the file may not yet exist.
(let ((my-utils-file (expand-file-name "lisp/my-utils.el" user-emacs-directory)))
  (when (file-exists-p my-utils-file)
    (require 'my-utils)))
;; NOTE: to use `use-package` semantics, might be possible to  replace the block above with:
;; (use-package my-utils :load-path "lisp") ; then add :bind/:custom etc.

;;----------------------------------------------------------------------------
;; 6. Global Key Remaps & Aliases
;;----------------------------------------------------------------------------
;; Redefine built-in functions and remap keys.

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
 '(package-selected-packages nil)
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
