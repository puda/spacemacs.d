(defconst puda-basic-packages
  '(
    vertigo
    key-chord
    php-mode
    dumb-jump
    geben
    geben-helm-projectile
    beacon
    )
  )

(defun puda-basic/init-geben ()
  ;; needs chrome/firefox extension for 9000 port signal
  ;; https://chrome.google.com/webstore/detail/xdebug-helper/eadndfjplgieldjbigjakmdgkmoaaaoc
  (use-package geben
    :defer t
    :init
    (progn
      (evil-leader/set-key-for-mode 'php-mode "gg" 'geben)
      (evil-leader/set-key-for-mode 'php-mode "gf" 'geben-find-file)
      ;; (evil-leader/set-key-for-mode 'php-mode "of" 'geben-helm-projectile/open-file)
      (evil-leader/set-key-for-mode 'php-mode "gc" 'geben-run-to-cursor)
      (evil-leader/set-key-for-mode 'php-mode "go" 'geben-step-over)
      (evil-leader/set-key-for-mode 'php-mode "gi" 'geben-step-into)
      (evil-leader/set-key-for-mode 'php-mode "gv" 'geben-display-context)
      (evil-leader/set-key-for-mode 'php-mode "gq" 'geben-stop)
      (evil-leader/set-key-for-mode 'php-mode "gQ" 'geben-end)

;;       (spacemacs|define-transient-state geben
;;         :title "Geben Transient State"
;;         :doc "
;; [_o/i_] step over/in            [_v_]^^     persistent action     [_e_]^^    edit occurrences
;; [_O_] step out                  [_1_.._0_]  action 1..10          [_t_/_T_]  toggle visible/all mark
;; [_q_]^^    quit                 [_a_]^^     action selection pg"
;;         :foreign-keys run
;;         :on-enter (spacemacs//helm-navigation-ms-on-enter)
;;         :on-exit  (spacemacs//helm-navigation-ms-on-exit)
;;         :bindings
;;         ("1" spacemacs/helm-action-1 :exit t)
;;         ("2" spacemacs/helm-action-2 :exit t)
;;         ("3" spacemacs/helm-action-3 :exit t)
;;         ("4" spacemacs/helm-action-4 :exit t)
;;         ("5" spacemacs/helm-action-5 :exit t)
;;         ("6" spacemacs/helm-action-6 :exit t)
;;         ("7" spacemacs/helm-action-7 :exit t)
;;         ("8" spacemacs/helm-action-8 :exit t)
;;         ("9" spacemacs/helm-action-9 :exit t)
;;         ("0" spacemacs/helm-action-10 :exit t)
;;         ("<tab>" helm-select-action :exit t)
;;         ("TAB" helm-select-action :exit t)
;;         ("<RET>" helm-maybe-exit-minibuffer :exit t)
;;         ;; ("?" nil :doc (spacemacs//helm-navigation-ms-full-doc))
;;         ("a" spacemacs/helm-transient-state-select-action)
;;         ("e" spacemacs/helm-edit)
;;         ("g" helm-beginning-of-buffer)
;;         ("G" helm-end-of-buffer)
;;         ("h" helm-previous-source)
;;         ("j" helm-next-line)
;;         ("k" helm-previous-line)
;;         ("l" helm-next-source)
;;         ("q" nil :exit t)
;;         ("t" helm-toggle-visible-mark)
;;         ("T" helm-toggle-all-marks)
;;         ("v" helm-execute-persistent-action))
      ;; (define-key helm-map (kbd "M-SPC")
      ;;   'spacemacs/helm-navigation-transient-state/body)
      )
    ))

(defun puda-basic/init-key-chord ()
  (use-package key-chord
    :defer t
    :init
    (progn
      (setq key-chord-two-keys-delay 0.1)
      ;; use M-space instead
      ;; (key-chord-define minibuffer-local-map "jk" 'spacemacs/helm-navigation-transient-state/body)
      ;; (key-chord-define minibuffer-local-map "jk" 'spacemacs/ivy-navigation-transient-state/body)
      )
    :config
    (key-chord-mode t)))

(defun puda-basic/init-vertigo ()
  (use-package vertigo
    ;; depends on keychord
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
    :ensure t))

(defun puda-basic/init-geben-helm-projectile ()
  (use-package geben-helm-projectile
    :ensure t))

(defun puda-basic/init-beacon ()
  (use-package beacon
    :ensure t
    :config
    (progn
      (beacon-mode t)
      )))

;; FOR IVY PACKAGE
;; keybinding rebinding
;; (define-key ivy-minibuffer-map (kbd "M-SPC") 'spacemacs/ivy-transient-state/body)
;; (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
;; byte-compile-file hydra-ivy
;; spacemacs-ivy
;; (global-set-key (kbd "<escape>") 'keyboard-quit)

;;; packages.el ends here
