(delete-selection-mode) ; Yank replaces selections

(setq require-final-newline t) ; Newline at end of file

(setq default-tab-width 2)

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
  :init (smartparens-global-strict-mode)
  :bind (:map smartparens-mode-map
	      ("C-x M-s" . smartparens-strict-mode)
	      ("C-M-a" . sp-beginning-of-sexp)
	      ("C-M-e" . sp-end-of-sexp)
	      ("M-r" . raise-sexp)
	      
              ("M-<down>" . sp-backward-down-sexp)
	      ("M-<up>"   . sp-backward-up-sexp)

	      ("C-M-f" . sp-forward-sexp)
	      ("C-M-b" . sp-backward-sexp)

	      ("C-M-n" . sp-next-sexp)
	      ("C-M-p" . sp-previous-sexp)

	      ("C-S-f" . sp-forward-symbol)
	      ("C-S-b" . sp-backward-symbol)

	      ("C-<right>" . sp-forward-slurp-sexp)
              ("C-<left>"  . sp-backward-slurp-sexp)
              
	      ("C-M-t" . sp-transpose-sexp)
	      ("C-M-k" . sp-kill-sexp)
	      ("C-k"   . sp-kill-hybrid-sexp)
	      ("M-k"   . sp-backward-kill-sexp)
	      ("C-M-w" . sp-copy-sexp)
	      ("C-M-d" . delete-sexp)

	      ("M-<backspace>" . backward-kill-word)
	      ("C-<backspace>" . sp-backward-kill-word)
	      ([remap sp-backward-kill-word] . backward-kill-word)

	      ("M-[" . sp-backward-unwrap-sexp)
	      ("M-]" . sp-unwrap-sexp)

	      ("C-x C-t" . sp-transpose-hybrid-sexp)
	      
	      ("C-c ("  . wrap-with-parens)
	      ("C-c ["  . wrap-with-brackets)
	      ("C-c {"  . wrap-with-braces)
	      ("C-c '"  . wrap-with-single-quotes)
	      ("C-c \"" . wrap-with-double-quotes)
	      ("C-c _"  . wrap-with-underscores)
	      ("C-c `"  . wrap-with-back-quotes)))

(use-package undo-tree
  :init (global-undo-tree-mode +1))
