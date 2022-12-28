;; UI
(set-face-attribute 'default nil :height 140) ; Font size
(setq inhibit-startup-message t)   ; Disable startup message
(tool-bar-mode -1)                 ; Disable the toolbar
(tooltip-mode -1)                  ; Disable tooltips
(set-fringe-mode 1)                ; Smaller fringes
(global-hl-line-mode +1)           ; Highlight current line
(column-number-mode)               ; Show column number
(global-display-line-numbers-mode) ; Show line numbers
(dolist (mode '(term-mode-hook
		dired-mode-hook
		neotree-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0)))) ; But not in some modes
(setq display-time-default-load-average nil) ; Don't show load average
(display-time)                               ; Show time

(use-package solarized-theme)
(use-package doom-themes)
(load-theme 'doom-palenight t) ; Set the theme
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
