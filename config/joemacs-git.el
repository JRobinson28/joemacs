(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :custom
  (setq  forge-topic-list-limit '(100 . 0)))
