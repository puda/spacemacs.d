;;; config.el --- Colors Layer configuration File for Puda Config

;; config
(evil-leader/set-key
  "." 'dumb-jump-go
  ">" 'dumb-jump-back)

(delete-selection-mode t)
(global-auto-revert-mode t) ;; reload open files
(setq default-directory "/var/www/community/" )
(setq initial-scratch-message ";; Puda loves Rachael!! \n;; Puda Emacs Custom Config!!")

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

(puda-turn-off-mouse)

(evil-leader/set-key
  "." 'dumb-jump-go
  ">" 'dumb-jump-prev)
