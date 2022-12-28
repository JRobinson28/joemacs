(defun org-mode-setup ()
  (org-indent-mode)
  ;(variable-pitchmode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-files '("~/projects/orgfiles/")))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
