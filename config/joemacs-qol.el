(fset 'yes-or-no-p 'y-or-n-p) ; Better yes/no prompts

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p) ; Make a shell script executable automatically on save

(when (eq system-type 'darwin)
  (setq insert-directory-program "/usr/local/bin/gls"))

(setq confirm-kill-processes nil) ; Quit directly when there are running processes

(use-package super-save
  :init (super-save-mode +1)
  :config
  (add-to-list 'super-save-triggers 'ace-window))
