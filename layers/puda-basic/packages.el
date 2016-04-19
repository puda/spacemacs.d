(defconst puda-basic-packages
  '(
    vertigo
    key-chord
    php-mode
    dumb-jump
    geben
    )
  )


(defun puda-basic/init-geben ()
  (use-package geben
    :config
    (progn
      (evil-leader/set-key-for-mode 'php-mode "gg" 'geben)
      (evil-leader/set-key-for-mode 'php-mode "gf" 'geben-find-file)
      (evil-leader/set-key-for-mode 'php-mode "gc" 'geben-run-to-cursor)
      (evil-leader/set-key-for-mode 'php-mode "go" 'geben-step-over)
      (evil-leader/set-key-for-mode 'php-mode "gi" 'geben-step-into)
      (evil-leader/set-key-for-mode 'php-mode "gv" 'geben-display-context)
      (evil-leader/set-key-for-mode 'php-mode "gq" 'geben-stop)
      )))

(defun puda-basic/init-key-chord ()
  (use-package key-chord
    :defer t
    :init
    (progn
      (setq key-chord-two-keys-delay 0.1)
      (key-chord-define minibuffer-local-map "jk" 'helm-like-unite/body)
      ;; (key-chord-define evil-normal-state-map "jk" 'puda-scroll-inside-file/body)

      (defhydra helm-like-unite ()
        "movement"
        ("?" helm-help "help")
        ("<escape>" nil "exit")
        ;; ("<escape>" keyboard-escape-quit "exit")
        ("<SPC>" helm-toggle-visible-mark "mark")
        ("a" helm-toggle-all-marks "(un)mark all")
        ("/" (lambda ()
               (interactive)
               (execute-kbd-macro [?\C-s]))
         "search")
        ("v" helm-execute-persistent-action)
        ("g" helm-beginning-of-buffer "top")
        ("G" helm-end-of-buffer "bottom")
        ("j" helm-next-line "down")
        ("k" helm-previous-line "up"))

      )
    :config
    (key-chord-mode t)))

(defun puda-basic/init-vertigo ()
  (use-package vertigo
    :config
    (progn
      (key-chord-define evil-normal-state-map "fk" 'vertigo-jump-up)
      (key-chord-define evil-normal-state-map "fj" 'vertigo-jump-down)
      (key-chord-define evil-visual-state-map "fk" 'vertigo-jump-up)
      (key-chord-define evil-visual-state-map "fj" 'vertigo-jump-down)
      (key-chord-define evil-motion-state-map "fk" 'vertigo-jump-up)
      (key-chord-define evil-motion-state-map "fj" 'vertigo-jump-down)
      )))

(defun puda-basic/pre-init-php-mode ()
  ;; use gtags layer and make sure the ~/.globalrc file is in place and generate tags using
  ;; gtags --gtagslabel drupal
  (use-package php-mode
    :mode (
           ("\\.module\\'" . php-mode)
           ("\\.views\\.inc\\'" . php-mode)
           ("\\.admin\\.inc\\'" . php-mode)
           )
    :config
    (progn
      (defun puda-drupal-gtags-create ()
        (interactive)
        (let ((inhibit-message t) (default-directory "/var/www/community/"))
          (shell-command "gtags --gtagslabel drupal")))

      (evil-leader/set-key-for-mode 'php-mode "." 'ggtags-find-definition)
      (evil-leader/set-key-for-mode 'php-mode ">" 'ggtags-prev-mark)
      (evil-leader/set-key-for-mode 'php-mode "oc" 'puda-drupal-gtags-create)

      )))

(defun puda-basic/init-dumb-jump ()
  (use-package dumb-jump
    :ensure t
    ))

;;; packages.el ends here
