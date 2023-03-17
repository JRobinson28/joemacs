(use-package general
  :init (general-define-key
	 "C-x M-t" 'load-theme
	 "C-x M-e" 'enable-theme
	 "C-c c" 'clipboard-kill-ring-save
	 "C-c v" 'clipboard-yank
	 "C-c x" 'clipboard-kill-region
	 "£" '(lambda () (interactive) (insert "#"))))

(use-package command-log-mode)
