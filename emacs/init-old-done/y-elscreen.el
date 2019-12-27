;; -*- lexical-binding: t -*-

(require 'cl)
(require 'y-harness)
(require 'y-hs)

(defun y-list-diff (lold lnew)
  "Сравнить два списка LOLD и LNEW и вернуть пару, первый элемент которой
содержит элементы, содержащиеся в LNEW и отсутствующие в LOLD, а второй --
наоборот."
  (let (added removed)
    (dolist (elt lold)
      (unless (member elt lnew)
        (push elt removed)))
    (dolist (elt lnew)
      (unless (member elt lold)
        (push elt added)))
    (cons added removed)))

(assert (let ((one (list 1 4 7))
              (two (list 1 4 9))
              (thr (list 1 4))
              (aux nil))
          (fset 'aux (lambda (diff)
                       (let ((a (sort (car diff) '<))
                             (b (sort (cdr diff) '<)))
                         (cons a b))))
          (and (equal (cons '(9) '(7)) (y-list-diff one two))
               (equal (cons one nil) (aux (y-list-diff nil one)))
               (equal (cons nil one) (aux (y-list-diff one nil)))
               (equal '(() 7) (y-list-diff one thr)))))

(defun y-list-patch (lold diff)
  "Применить DIFF к списку LOLD (см. y-list-diff).
Если DIFF был получен сравнением LOLD и LNEW, то после вызова
\(y-list-patch lold diff) получим список, содержащий все элементы из
LNEW и не содержащий элементов, не содержащихся в LNEW. Однако может
быть другой порядок элементов и отсутствовать повторяющиеся элементы.
Гарантировано, что эта функция не добавит в LOLD элемент, который уже
содержится в LOLD."
  (dolist (elt (cdr diff))
    (setq lold (delete elt lold)))
  (dolist (elt (car diff))
    (unless (member elt lold)
      (push elt lold)))
  lold)

(assert (let ((diff (cons (list 4 2 6) nil)))
          (equal (y-list-patch nil diff) (list 6 2 4))))
(assert (equal (y-list-patch (list 0 4 8)
                             (cons '(2) '(0 8)))
               '(2 4)))
(assert (equal (y-list-patch (list 0 4 8)
                             (cons '(2 4) '(0 8)))
               '(2 4)))
(assert (equal (y-list-patch (list 0 4 8) nil) (list 0 4 8)))

(defvar yel-prev-buffer-list (copy-list (buffer-list))
  "Сохраненный список буферов, с которым будет сравниваться текущее
значение при вызове buffer-list-update-hook.")

(defun yel-sieve-buffer-list (buflist)
  "Filter BUFLIST throwing away any buffer that is dead or which name begins with space."
  (setq buflist (y-filter 'buffer-live-p buflist))
  (y-filter (lambda (buf) (not (equal ?\s (elt (buffer-name buf) 0))))
            buflist))

(defun yel-get-screen-buflist (&optional screen)
  (unless screen
    (setq screen (elscreen-get-current-screen)))
  (let* ((property (elscreen-get-screen-property screen))
         (buflist (get-alist 'yel-buffer-list property)))
    buflist))

(defun yel-set-screen-buflist (buflist &optional screen)
  (unless screen
    (setq screen (elscreen-get-current-screen)))
  (let* ((property (elscreen-get-screen-property screen)))
    (setq property (elscreen--put-alist 'yel-buffer-list buflist property))
    (elscreen-set-screen-property screen property)))

    

(defun yel-buffer-list-update-hook ()
  (let* ((buflist (buffer-list))
         (diff (y-list-diff yel-prev-buffer-list buflist))
         (oldbufs (yel-get-screen-buflist))
         (newbufs (y-list-patch oldbufs diff)))
    ;; (message "inside yel-buffer-list-update-hook")
    ;; (message "current-screen = %d" (elscreen-get-current-screen))
    ;; (message "yel-prev-buffer-list = %s" yel-prev-buffer-list)
    ;; (message "buflist = %s" buflist)
    ;; (message "diff = %s" diff)
    ;; (message "(car diff) = %s" (car diff))
    ;; (message "(cdr diff) = %s" (cdr diff))
    ;; (message "oldbufs = %s" oldbufs)
    ;; (message "newbufs = %s" newbufs)
    (setq yel-prev-buffer-list (copy-list buflist))
    (setq newbufs (yel-sieve-buffer-list newbufs))
    (if (or newbufs (not oldbufs) (elscreen-one-screen-p))
        (when newbufs (yel-set-screen-buflist newbufs))
      (elscreen-kill (elscreen-get-current-screen)))))
  
(setq buffer-list-update-hook 'yel-buffer-list-update-hook)

(setq yel-aux-buflist nil)
(defun yel-break-condition()
  (let* ((cur-buflist (yel-get-screen-buflist 0))
         (old-buflist yel-aux-buflist))
    (setq yel-aux-buflist cur-buflist)
    (not (equal old-buflist cur-buflist))))

(defun yel-buffer-menu ()
  "Invoke buffer-menu containing only buffers bound to the current screen."
  (interactive)
  (let* ((buflist-prop (yel-get-screen-buflist))
         (buflist (buffer-list)))
    (yel-set-screen-buflist (yel-sieve-buffer-list buflist-prop))
    (setq buflist
          (y-filter (lambda (buf) (member buf buflist-prop)) buflist))
    (switch-to-buffer (list-buffers-noselect t buflist))))

(global-set-key "\C-x\C-b" 'yel-buffer-menu)

(defun yel-current-screen-reorder-buffers ()
  (let ((total (buffer-list))
        (local (yel-get-screen-buflist)))
  (dolist (buf total)
    (unless (member buf local)
      (bury-buffer buf)))))
    
(defadvice elscreen-goto-internal (after yel-reorder-buffers (&rest args))
  (yel-current-screen-reorder-buffers))
