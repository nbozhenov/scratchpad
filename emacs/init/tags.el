;; Поиск тегов организован довольно интересным образом. Определяется список
;; функций, каждая из которых ищет тег по-своему, со своей точностью. Замыкающие
;; этот список функции ищут совсем неразборчиво.

(load-file "~/SkyDrive/code/emacs/init/etags-select.el")
(define-key etags-select-mode-map [tab] (lambda () (interactive) (etags-select-goto-tag 4 t)))

;; (defadvice etags-select-find-tag-at-point (before y-push-mark-onto-ring)
;;   (ring-insert find-tag-marker-ring (point-marker)))

(defun y-find-tag ()
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  (etags-select-find-tag-at-point))

(define-key evil-insert-state-map (kbd "C-]") 'y-find-tag)
(define-key evil-normal-state-map (kbd "C-]") 'y-find-tag)
(define-key evil-motion-state-map (kbd "C-]") 'y-find-tag)

(define-key evil-insert-state-map (kbd "C-t") 'pop-tag-mark)
(define-key evil-normal-state-map (kbd "C-t") 'pop-tag-mark)
(define-key evil-motion-state-map (kbd "C-t") 'pop-tag-mark)


;; (define-key evil-insert-state-map (kbd "C-]") 'evil-jump-to-tag)
;; (define-key evil-normal-state-map (kbd "C-]") 'evil-jump-to-tag)
;; (define-key evil-motion-state-map (kbd "C-]") 'evil-jump-to-tag)
