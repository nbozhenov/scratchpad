;; ** read-key-sequence
;;    Считывает и преобразует ввод.
;;    Введенные символы преобразуются в соответствии со следующими
;;    таблицами (список может быть не полный, но порядок правильный):
;;      + input-decode-map
;;      + local-function-key-map / function-key-map
;;      + key-translation-map

;; perceive (kbd "<tab>") as (kdb "C-i")
;; and (kbd "C-i") as (kbd "<tab>")
;; не самая удачная идея, т.к. идентичность C-i и TAB -- штука общепринятая,
;; (в т.ч. в виме) и в терминале, к примеру, вообще трудно преодолимая.
;(setq function-key-map (delete '(tab . [9]) function-key-map))
;(setq function-key-map (delete '(kp-tab . [9]) function-key-map))
;(setq function-key-map (append function-key-map '((kp-tab . [tab]))))
;(setq local-function-key-map (delete '(tab . [9]) local-function-key-map))
;(setq local-function-key-map (delete '(kp-tab . [9]) local-function-key-map))
;(setq local-function-key-map (append local-function-key-map '((kp-tab . [tab]))))
;(define-key key-translation-map [tab] (kbd "C-i"))
;(define-key key-translation-map (kbd "C-i") [tab])

;; perceive (kbd "<escape>") as (kdb "C-[")
;; and (kbd "C-[") as (kbd "<escape>")
;(setq input-decode-map (make-sparse-keymap))
;(setq evil-esc-mode t) ; prevent evil-mode from recreating input-decode-map
                        ; see evil.el
;(setq function-key-map (delete '(escape . [27]) function-key-map))
;(setq local-function-key-map (delete '(escape . [27]) local-function-key-map))
;(define-key key-translation-map [escape] (kbd "C-["))  
;(define-key key-translation-map (kbd "C-[") [escape])  
;; use "C-[" to quit any state
;(define-key global-map [escape] 'keyboard-escape-quit)
