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
    dumb-jump
    vertigo
    golden-ratio
    key-chord
    ac-php
    php-mode
    hydra
    )
  )

(defun puda-basic/init-geben ()
  (use-package geben
    :defer t))

(defun puda-basic/init-golden-ratio ()
  (use-package golden-ratio
    :diminish golden-ratio-mode
    :config
    (golden-ratio-mode t)))

(defun puda-basic/init-evil-mc ()
  (use-package evil-mc
    :diminish evil-mc-mode
    :config
    (global-evil-mc-mode t)))

(defun puda-basic/init-dumb-jump ()
  (use-package dumb-jump
    :diminish dumb-jump-mode
    :config
    (progn
      (dumb-jump-mode t)
      (evil-leader/set-key
        "." 'dumb-jump-go
        ">" 'dumb-jump-back))))

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
    :defer t
    :config
    (progn
      (add-hook 'with-editor-mode-hook 'evil-insert-state))))

(defun puda-basic/init-shell-pop ()
  (use-package shell-pop
    :defer t
    :config
    (progn
      (add-hook 'term-mode-hook
                (lambda ()
                  (define-key term-raw-map (kbd "C-y") 'term-paste))))))

(defun puda-basic/init-ac-php ()
  (use-package ac-php
    :config
    (progn
      (evil-leader/set-key-for-mode 'php-mode "." 'ac-php-find-symbol-at-point)
      (evil-leader/set-key-for-mode 'php-mode ">" 'ac-php-location-stack-back)
      (evil-leader/set-key-for-mode 'php-mode "rt" 'ac-php-remake-tags)
      (evil-leader/set-key-for-mode 'php-mode "ra" 'ac-php-remake-tags-all)
      (add-hook 'php-mode-hook
                '(lambda ()
                   (require 'ac-php-company)
                   (company-mode t)
                   (add-to-list 'company-backends 'company-ac-php-backend)))
      )
    ))

(defun puda-basic/init-php-mode ()
  (use-package php-mode
    :defer t
    :mode (
           ("\\.module\\'" . php-mode)
           ("\\.views\\.inc\\'" . php-mode)
           ("\\.admin\\.inc\\'" . php-mode)
           )))

(defun puda-basic/init-hydra ()
  (use-package hydra
    :defer t
    ))

;;; packages.el ends here
