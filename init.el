(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   dotspacemacs-configuration-layers
   '(
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t)
     (git :variables
          git-magit-status-fullscreen t)
     php html gtags
     (javascript :variables
                 javascript-disable-tern-port-files t)
     (shell :variables
            shell-default-position 'right)
     (ranger :variables
             ranger-show-preview t)
     vim-empty-lines syntax-checking colors
     better-defaults emacs-lisp evil-snipe org ibuffer
     (theming :variables
              theming-headings-inherit-from-default 'all
              theming-headings-same-size 'all
              theming-headings-bold 'all)
     ;; custom config
     puda-basic
     )
   dotspacemacs-additional-packages '()
   dotspacemacs-excluded-packages '(
                                    drupal-mode
                                    )
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'hybrid
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(
                         monokai
                         material
                         spacemacs-dark
                         )
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro for Powerline"
                               :size 35
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location nil
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.5
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup t
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers 'relative
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  ;;variables
  (setq
   evil-shift-round nil
   ;; smartparens
   sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   sp-highlight-wrap-tag-overlay nil
   ;; random
   default-directory "/var/www/community/"
   initial-scratch-message ";; Puda loves Rachael!! \n;; Puda Emacs Custom Config!!"
   )

  ;;modes
  (delete-selection-mode t)
  (global-auto-revert-mode t) ;; reload open files

  )

(defun dotspacemacs/user-config ()
  (setq powerline-default-separator 'alternate)
  (with-eval-after-load 'smartparens
    (show-smartparens-global-mode -1))
  (let ((comint-hooks '(
                        term-mode-hook
                        eshell-mode-hook
                        messages-buffer-mode-hook
                        )))
    (spacemacs/add-to-hooks
     (defun puda-disable-hl-for-term ()
       (setq global-hl-line-mode nil))
     comint-hooks))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
