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
 "C-x M-t" 'load-theme)

;; UI

(set-face-attribute 'default nil :height 140)
(setq inhibit-startup-message t)   ; Disable startup message
(tool-bar-mode -1)                 ; Disable the toolbar
(tooltip-mode -1)                  ; Disable tooltips
(set-fringe-mode 1)                ; Smaller fringes
(column-number-mode)               ; Show column number
(global-display-line-numbers-mode) ; Show line numbers
(display-time)                     ; Show time
(dolist (mode '(term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0)))) ; But not in term-mode
;(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ; Maximise window on startup

(use-package solarized-theme)
(use-package doom-themes)
(load-theme 'solarized-selenized-black t) ; Set the theme

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Navigation

(setq default-directory "~/Projects")
(setq confirm-kill-processes nil) ; Quit directly when there are running processes


(use-package swiper
  :bind (("C-s" . swiper)))

(use-package ivy
  :config (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-x b" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1)
  (setq ivy-initial-inputs-alist nil)) ; Don't start searches with ^

(use-package projectile
  :init (projectile-mode +1)
  :custom ((setq projectile-project-search-path '("~/projects/")))
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package ace-window
  :bind ("s-w" . ace-window))

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

;; Structural editing

(use-package paredit
  :hook (prog-mode . enable-paredit-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Magit

(use-package magit)

;; Clojure

(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

;; Terraform

(use-package terraform-mode
  :config (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package terraform-doc)

;; Markdown

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; Docker

(use-package dockerfile-mode)

;; yaml

(use-package yaml-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0c08a5c3c2a72e3ca806a29302ef942335292a80c2934c1123e8c732bb2ddd77" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" default))
 '(package-selected-packages
   '(general yaml-mode doom-themes solarized-theme dockerfile-mode docker-mode helpful counsel ivy-rich all-the-icons which-key ace-window magit markdown-mode terraform-doc terraform-mode projectile cider clojure-mode use-package swiper paredit doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
