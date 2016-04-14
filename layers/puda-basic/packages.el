;;; packages.el --- puda-basic layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: puda <puda@puda-SP4>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `puda-basic-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `defun puda-basic/init-PACKAGE' to load and initialize the package. ()

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `puda-basic/pre-init-PACKAGE' and/or
;;   `puda-basic/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst puda-basic-packages
  '(
    geben
    vertigo
    key-chord
    php-mode
    hydra
    evil-mc
    evil-magit
    dumb-jump
    )
  )

(defun puda-basic/init-geben ()
  (use-package geben
    :defer t))

(defun puda-basic/init-evil-mc ()
  (use-package evil-mc
    :config
    (global-evil-mc-mode t)))

(defun puda-basic/init-key-chord ()
  (use-package key-chord
    :defer t
    :init
    (progn
      (setq key-chord-two-keys-delay 0.1)
      (key-chord-define minibuffer-local-map "jk" 'helm-like-unite/body)

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
      (key-chord-define evil-normal-state-map "fj" 'vertigo-jump-down))))

(defun puda-basic/post-init-evil-magit ()
  (use-package evil-magit
    ;; :defer t
    :config
    (progn
      (add-hook 'with-editor-mode-hook 'evil-hybrid-state))))

(defun puda-basic/pre-init-php-mode ()
  ;; use gtags layer and make sure the ~/.globalrc file is in place and generate tags using
  ;; gtags --gtagslabel drupal
  ;; make sure the keybindings work out of the box for spacemacs
  (use-package php-mode
    ;; :defer t
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

      (evil-leader/set-key
        "." 'ggtags-find-definition
        ">" 'ggtags-prev-mark
        "oc" 'puda-drupal-gtags-create)
      (add-hook 'php-mode-hook
                (lambda ()
                  ;; this mode is giving errors when completing with arguments that have data type, making it slow
                  (show-smartparens-mode -1)
                  )))))

(defun puda-basic/init-hydra ()
  (use-package hydra
    :defer t
    ))

;;; packages.el ends here
