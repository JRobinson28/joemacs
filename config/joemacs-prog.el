;; LSP mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l") 
  :config
  (lsp-enable-which-key-integration t)
  (lsp-headerline-breadcrumb-mode nil))

(use-package lsp-ivy
  :after lsp)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; Clojure
(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :config
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'flycheck-mode)
  (require 'flycheck-clj-kondo))

(use-package cider
  :config
  (setq nrepl-log-messages t)
  (setq cider-clojure-cli-global-options "-A:dev:test")
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

;; Golang
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
   :config
   (add-hook 'go-mode-hook #'rainbow-delimiters-mode)
   (add-hook 'go-mode-hook #'lsp-mode)
   (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
   :bind
   (("C-c d" . go-doc)
    ("C-c f" . go-fmt)))

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

;; justfile
(use-package just-mode)

;; feature-mode
(use-package feature-mode)
