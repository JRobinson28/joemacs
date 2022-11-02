(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

;; Initialize package sources

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Global key bindings

(use-package general)
(general-define-key
 "C-x M-t" 'load-theme
 "C-x M-e" 'enable-theme)

;; UI

(set-face-attribute 'default nil :height 140)
(setq inhibit-startup-message t)   ; Disable startup message
(tool-bar-mode -1)                 ; Disable the toolbar
(tooltip-mode -1)                  ; Disable tooltips
(set-fringe-mode 1)                ; Smaller fringes
(column-number-mode)               ; Show column number
(global-display-line-numbers-mode) ; Show line numbers
(dolist (mode '(term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0)))) ; But not in term-mode
(setq display-time-default-load-average nil) ; Don't show load average
(display-time)                               ; Show time
;(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ; Maximise window on startup
(fset 'yes-or-no-p 'y-or-n-p) ; Better yes/no prompts
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p) ; Make a shell script executable automatically on save

(use-package solarized-theme)
(use-package doom-themes)
(load-theme 'doom-palenight t) ; Set the theme

(use-package all-the-icons)
(use-package command-log-mode)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Navigation etc.

(setq default-directory "~/projects")
(when (eq system-type 'darwin)
  (setq insert-directory-program "/usr/local/bin/gls"))
(setq confirm-kill-processes nil) ; Quit directly when there are running processes
(delete-selection-mode) ; Yank replaces selections
(setq require-final-newline t) ; Newline at end of file

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))) ; store all backup and autosave files in the tmp dir

(use-package windmove
  :init (windmove-default-keybindings))

(use-package ace-window
  :bind ("s-w" . ace-window))

(use-package super-save
  :init (super-save-mode +1)
  :config
  (add-to-list 'super-save-triggers 'ace-window))

(use-package crux
  :ensure t
  :bind (("M-o" . crux-smart-open-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-M-z" . crux-indent-defun)
         ("C-c w" . crux-swap-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("s-o" . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)))

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package ivy
  :config (ivy-mode 1))

(use-package ivy-rich
  :after counsel
  :init (ivy-rich-mode 1))

(global-hl-line-mode +1)

(use-package counsel
  :after ivy
  :bind (("C-x b" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1)
  (setq ivy-initial-inputs-alist nil)) ; Don't start searches with ^

(use-package projectile
  :init (projectile-mode +1)
  :custom ((setq projectile-project-search-path '("~/projects/")))
  :bind
  (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  (:map projectile-command-map
        ("g" . projectile-ripgrep)))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package company
  :config
  (setq company-show-numbers t)
  ;(setq company-tooltip-limit 10)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package browse-kill-ring
  :bind
  ("M-y" . browse-kill-ring))

(use-package easy-kill
  :bind
  ([remap kill-ring-save] . 'easy-kill)
  ("C-M-m" . 'easy-mark))
 
;; Structural editing

;; (use-package paredit
;;   :hook (prog-mode . enable-paredit-mode))

(use-package smartparens
  :init (smartparens-global-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Dired

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Magit

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :custom
  (setq  forge-topic-list-limit '(100 . 0)))

;; Clojure

(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :config
  (setq nrepl-log-messages t)
  (setq cider-clojure-cli-global-options "-A:dev:test")
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

;; Golang

(use-package go-mode)

;; Terraform

(use-package terraform-mode
  :config (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package terraform-doc)

;; Markdown

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package grip-mode
  :ensure t
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))

;; Docker

(use-package dockerfile-mode)

;; yaml

(use-package yaml-mode)

(use-package neotree
  :init (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :config (setq projectile-switch-project-action 'neotree-projectile-action)
  :bind ([f8] . neotree-toggle))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "0c08a5c3c2a72e3ca806a29302ef942335292a80c2934c1123e8c732bb2ddd77" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" default))
 '(package-selected-packages
   '(smartparens dired-single all-the-icons-dired neotree easy-kill browse-kill-ring company super-save forge crux command-log-mode go-mode counsel-projectile grip-mode general yaml-mode doom-themes solarized-theme dockerfile-mode docker-mode helpful counsel ivy-rich all-the-icons which-key ace-window magit markdown-mode terraform-doc terraform-mode projectile cider clojure-mode use-package swiper paredit doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
