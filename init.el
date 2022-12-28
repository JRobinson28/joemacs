;; Add the config directory to the load path
(add-to-list 'load-path (concat user-emacs-directory "config/"))

;; Initialises package archives- must go first
(load "joemacs-startup")

;; Visual settings
(load "joemacs-ui")

;; Global keybindings
(load "joemacs-keybindings")

;; Navigation tools and utilities
(load "joemacs-nav")

;; Text editing configuration
(load "joemacs-editor")

;; General quality of life improvements
(load "joemacs-qol")

;; Help tools
(load "joemacs-help")

;; Git/github integration
(load "joemacs-git")

;; Programming language support
(load "joemacs-prog")

;; Org mode configuration
(load "joemacs-org")

