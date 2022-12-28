(setq default-directory "~/projects")

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package ivy
  :config (ivy-mode 1))

(use-package ivy-rich
  :after counsel
  :init (ivy-rich-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

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
  :custom ((setq projectile-project-search-path '("~/projects/"))
	   (setq projectile-indexing-method 'hybrid)
	   (add-to-list 'projectile-globally-ignored-directories "*.clj-kondo")
	   (add-to-list 'projectile-globally-ignored-directories "*.lsp")
	   (add-to-list 'projectile-globally-ignored-directories "*.cpcache"))
  :bind
  (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  (:map projectile-command-map
        ("g" . projectile-ripgrep)))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Window management
(use-package windmove
  :init (windmove-default-keybindings))

(use-package ace-window
  :bind ("s-w" . ace-window))

;; Dired
(use-package dired
  :ensure nil
  :bind (("C-x C-j" . dired-jump))
  :hook (dired-mode . dired-hide-details-mode)
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Neotree
(use-package neotree
  :init (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :config (setq projectile-switch-project-action 'neotree-projectile-action)
  :bind ([f8] . neotree-toggle))
