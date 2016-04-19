(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   dotspacemacs-delete-orphan-packages t
   dotspacemacs-configuration-layers
   '((git :variables
          git-magit-status-fullscreen t)
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-enable-help-tooltip nil)
     (evil-snipe :variables
                 evil-snipe-enable-alternate-f-and-t-behaviors 't)
     php
     html
     gtags
     (javascript :variables
                 javascript-disable-tern-port-files t)
     (shell :variables
            shell-default-shell 'ansi-term
            shell-default-position 'right)
     (ranger :variables
             ranger-show-preview t)
     vim-empty-lines
     (spell-checking :variables spell-checking-enable-by-default nil)
     (syntax-checking :variables syntax-checking-enable-by-default 't)
     (version-control :variables version-control-diff-tool 'diff-hl)
     colors
     better-defaults
     emacs-lisp
     org
     ibuffer
     unimpaired
     ;; custom config
     puda-basic
     puda-theming
     ;; others
     (theming :variables
              theming-headings-inherit-from-default 'all
              theming-headings-same-size 'all
              theming-headings-bold 'all)
     )

   dotspacemacs-additional-packages '()
   dotspacemacs-excluded-packages '(drupal-mode vi-tilde-fringe evil-search-highlight-persist)
   ))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 10
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(recents bookmarks projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(monokai material spacemacs-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro for Powerline" :size 35 :weight demibold :width normal :powerline-scale 1.15)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-command-key "SPC"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Home"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location nil
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 1.0
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup t
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide nil
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers 'relative
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'changed
   ))

(defun dotspacemacs/user-init ()
  ;; Variables
  (setq-default
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
   helm-echo-input-in-header-line nil
   ;; Evil
   evil-move-beyond-eol nil
   ;; Snipe
   evil-snipe-scope 'visible
   evil-snipe-repeat-scope 'visible
   evil-snipe-spillover-scope 'visible
   )
  ;; Modes
  (global-evil-mc-mode t)
  ;; hooks
  (add-hook 'evil-mc-after-cursors-deleted
            (defun puda/clear-anzu () (setq anzu--state nil)))
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
  (push 'term-mode evil-escape-excluded-major-modes)
  (define-key evil-emacs-state-map (kbd "C-y") 'term-paste)

  ;; leader keybindings
  (evil-leader/set-key
    "." 'dumb-jump-go
    ">" 'dumb-jump-back
    "bk" 'kill-this-buffer
    )
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
