(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   dotspacemacs-delete-orphan-packages t
   dotspacemacs-configuration-layers
   '(
     (git :variables
          git-magit-status-fullscreen t)
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-enable-help-tooltip nil
                      )
     (evil-snipe :variables
                 evil-snipe-enable-alternate-f-and-t-behaviors t
                 )
     php
     html
     gtags
     ;; learn languages
     react
     ruby
     python
     shell-scripts
     ;; end
     command-log
     (javascript :variables
                 javascript-disable-tern-port-files t)
     (shell :variables
            shell-default-shell 'ansi-term
            shell-default-position 'right)
     (ranger :variables
             ranger-show-preview t)
     vim-empty-lines
     (spell-checking :variables spell-checking-enable-by-default nil)
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     (version-control :variables version-control-diff-tool 'diff-hl)
     colors
     unimpaired ;;maybe
     better-defaults
     emacs-lisp
     org
     ibuffer
     ;; custom config
     puda-basic
     puda-theming
     (theming :variables
              theming-headings-inherit-from-default 'all
              theming-headings-same-size 'all
              theming-headings-bold 'all)
     )

   dotspacemacs-additional-packages '()
   dotspacemacs-excluded-packages '(
                                    drupal-mode
                                    )
   ))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'vim ;;hybrid mode makes it laggy?
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
   dotspacemacs-default-font '
   ("Source Code Pro for Powerline"
    :size 35
    :weight demibold
    :width normal
    :powerline-scale 1.15)
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
   dotspacemacs-which-key-delay 1
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup t
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers 'relative
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'changed
   ))

(defun dotspacemacs/user-init ()
  ;; Variables
  (setq
   ;; Evil
   evil-shift-round nil
   ;; Smartparens
   sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   sp-highlight-wrap-tag-overlay nil
   ;; Avy
   avy-all-windows 'all-frames
   ;; Ranger
   ranger-override-dired t
   ;; Spaceline
   spaceline-buffer-encoding-abbrev-p nil
   spaceline-version-control-p nil
   ;; Shell
   shell-default-term-shell "/bin/zsh"
   ;; General
   ring-bell-function 'ignore
   default-directory "/var/www/community/"
   initial-scratch-message ";; Puda loves Rachael!! \n;; Puda Emacs Custom Config!!"
   )

  ;; Modes
  (delete-selection-mode t)
  (global-auto-revert-mode t) ;; reload open files
  )

(defun dotspacemacs/user-config ()
  ;; Variable
  (setq
   powerline-default-separator 'arrow
   )
  ;; Modes
  (global-evil-mc-mode t)
  ;; hooks
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
  (evil-set-initial-state 'term-mode 'emacs)
  (define-key evil-emacs-state-map (kbd "C-y") 'term-paste)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
