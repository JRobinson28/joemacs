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
;(add-to-list 'load-path "~/golib/pkg/mod/github.com/dougm/goflymake@v0.0.0-20140731161037-3b9634ef394a")
					;(require 'go-flycheck)
(defun set-go-build-flags ()
  (lsp-register-custom-settings '(("gopls.buildFlags" ["-tags=unit"] t))))

(use-package go-mode
  :init
  (setq lsp-go-build-flags ["-tags=unit,acceptance"])
  :config
  (require 'lsp-go)
  (add-hook 'go-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'go-mode-hook #'lsp-mode)
  (add-hook 'go-mode-hook (lambda ()
			    (add-hook 'before-save-hook 'gofmt-before-save)
			    (add-hook 'before-save-hook #'lsp-organize-imports t t)
			    (setq tab-width 4)
			    (setq indent-tabs-mode 1)))
  :bind
  (("C-c d" . go-doc)
   ("C-c f" . go-fmt)))

(use-package go-guru)

(use-package gotest)

;; Python
(add-hook 'python-mode-hook 'lsp-deferred)

;; Javascript
(add-hook 'javascript-mode-hook 'lsp-deferred)

;; Typescript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

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
