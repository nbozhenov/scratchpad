(use-package helm :demand
  :config
  (helm-mode 1)
  (setq helm-mode-fuzzy-match t)

  ;; For some reason, binding \M-x doesn't work in :bind section. So, I moved
  ;; all hotkeys to the :config section (that's fine given the :demand keyword).
  (global-set-key "\M-x"     #'helm-M-x)
  (global-set-key "\C-x\C-f" #'helm-find-files)
  (global-set-key "\C-xb"    #'helm-buffers-list)
  (global-set-key "\C-ha"    #'helm-apropos)
)
