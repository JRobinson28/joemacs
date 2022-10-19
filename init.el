(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(setq inhibit-startup-message t) ; Disable startup message
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips

(load-theme 'misterioso) ; Set the theme

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

;; Navigation

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package ivy
  :config (ivy-mode 1))

(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :custom ((setq projectile-project-search-path '("~/projects/")))
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; Doom modeline

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


;; Magit

(use-package magit
  :ensure t)

;; Clojure

(use-package paredit
  :ensure t
  :init (enable-paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :init (rainbow-delimiters-mode))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

;; Terraform

(use-package terraform-mode
  :ensure t
  :config (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package terraform-doc
  :ensure t)

;; Markdown

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


