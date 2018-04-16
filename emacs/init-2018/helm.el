(require 'helm)
(helm-mode 1)
(setq helm-mode-fuzzy-match t)

(global-set-key "\M-x" 'helm-M-x)
(global-set-key "\C-x\C-f" 'helm-find-files)
(global-set-key "\C-x\C-b" 'helm-buffers-list)
(global-set-key "\C-ha" 'helm-apropos)
