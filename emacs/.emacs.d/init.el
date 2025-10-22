;;; init.el -*- lexical-binding: t; -*-

(require 'package)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(require 'ido)
(ido-mode t) ;; interactive mode

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; Don't auto-enable packages before init finishes
(setq package-enable-at-startup nil)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))  ;; Fetch archive metadata first run
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)  ;; Auto-install missing packages declared

;; === community packages ===
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

;; === custom packages ===
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; Load `my-utils`. Guard with file existence to avoid errors
;; on fresh clones where the file may not yet exist.
(let ((my-utils-file (expand-file-name "lisp/my-utils.el" user-emacs-directory)))
  (when (file-exists-p my-utils-file)
    (require 'my-utils)))

;; === remaps ===
;; NOTE: doesn't work for some reasons
(add-hook 'after-init-hook (lambda () (define-key global-map (kbd "C-x C-b") #'ibuffer)))

;; If you prefer use-package semantics you could replace the block above with:
;; (use-package my-utils :load-path "lisp") ; then add :bind/:custom etc.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
