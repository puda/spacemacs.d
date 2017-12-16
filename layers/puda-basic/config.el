;;; config.el --- Colors Layer configuration File for Puda Config


;; disable mouse
(defun puda-turn-off-mouse ()
  (defun turn-off-mouse (&optional frame)
    (interactive)
    (let ((inhibit-message t) (default-directory "~"))
      (shell-command "synclient TouchpadOff=1")))

  (defun turn-on-mouse (&optional frame)
    (interactive)
    (let ((inhibit-message t) (default-directory "~"))
      (shell-command "synclient TouchpadOff=0")))
  (add-hook 'focus-in-hook #'turn-off-mouse)
  (add-hook 'focus-out-hook #'turn-on-mouse)
  (add-hook 'kill-emacs-hook #'turn-on-mouse)
  (add-hook 'delete-frame-functions #'turn-on-mouse))

;; (puda-turn-off-mouse)

;; Web Mode
;; Enable web-mode for blade files
(add-to-list 'auto-mode-alist '("\\.blade.php\\'" . web-mode))
;; Indentation to 4 spaces
(setq web-mode-indent-style 4)

;; Prettify-Symbols
(let ((puda-prettify-hooks '(js2-mode-hook php-mode-hook emacs-lisp-mode-hook)))
  (spacemacs/add-to-hooks
   (defun puda-prettify-symbols ()
     (push '("function" . ?ƒ) prettify-symbols-alist)
     (push '("->" . ?→) prettify-symbols-alist)
     (push '("=>" . ?⇒) prettify-symbols-alist)
     (push '("!=" . ?≠) prettify-symbols-alist)
     (global-prettify-symbols-mode +1)
     )
   puda-prettify-hooks))
