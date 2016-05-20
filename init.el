(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation nil
   dotspacemacs-ask-for-lazy-installation nil
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   dotspacemacs-delete-orphan-packages t
   dotspacemacs-configuration-layers
   '((git :variables
          git-magit-status-fullscreen t)
     github
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-private-snippets-directory "~/.spacemacs.d/puda-snippets")
     (evil-snipe :variables
                 evil-snipe-enable-alternate-f-and-t-behaviors t)
     php
     html
     gtags
     (javascript :variables
                 javascript-disable-tern-port-files t)
     react ;;learn
     (shell :variables
            shell-default-position 'bottom
            shell-default-height 70
            )
     (ranger :variables
             ranger-show-preview t)
     (spell-checking :variables spell-checking-enable-by-default nil)
     syntax-checking
     (version-control :variables version-control-diff-tool 'diff-hl)
     colors
     better-defaults
     emacs-lisp
     org
     (deft :variables
       deft-extensions '("org" "md" "txt")
       deft-directory '"~/Documents/")
     ibuffer
     unimpaired
     games
     ;; custom config
     puda-basic
     puda-theming
     evil-little-word
     )

   dotspacemacs-additional-packages '()
   dotspacemacs-excluded-packages '(drupal-mode
                                    vi-tilde-fringe)
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
   dotspacemacs-themes '(monokai spacemacs-dark material leuven cyberpunk solarized-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro" :size 35 :weight demibold :width normal :powerline-scale 1.15)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-command-key "SPC"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 1.0
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide nil
   dotspacemacs-mode-line-unicode-symbols t ;; do not change
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-line-numbers nil ;; change back to relative
   dotspacemacs-large-file-size 1
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'changed))

(defun dotspacemacs/user-init ()
  ;; Variables
  (setq-default
   ;; Smartparens
   sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   sp-highlight-wrap-tag-overlay nil
   ;; Avy
   avy-all-windows 'all-frames
   ;; General
   default-directory "/var/www/community/"
   initial-scratch-message ";; Puda loves Rachael!! \n;; Puda Emacs Custom Config!!"
   )
  ;; nice scrolling
  (setq scroll-margin 0
        scroll-conservatively 10000
        scroll-preserve-screen-position 1)
  ;; transparency
  (let ((tp (cons dotspacemacs-active-transparency dotspacemacs-inactive-transparency)))
    (push `(alpha . ,tp) default-frame-alist)
    (set-frame-parameter (selected-frame) 'alpha tp))
  ; (push '(alpha . (0.94 . 0.94)) default-frame-alist)
  ; (set-frame-parameter (selected-frame) 'alpha '(0.94 . 0.94))
  ;; Modes
  (delete-selection-mode t)
  (global-auto-revert-mode t))

(defun dotspacemacs/user-config ()
  ;; Variable
  (setq
   ;; Snipe
   evil-snipe-scope 'visible
   evil-snipe-repeat-scope 'visible
   evil-snipe-spillover-scope 'visible
   ;; Evil
   evil-move-beyond-eol nil
   ;; powerline
   powerline-default-separator 'arrow
   ;; Helm Swoop
   helm-swoop-speed-or-color nil
   ;; Helm Mini
   helm-mini-default-sources '(helm-source-buffers-list helm-source-recentf helm-source-buffer-not-found helm-source-bookmarks)
   ;; Helm
   helm-echo-input-in-header-line nil
   ;; Spaceline
   spaceline-new-version-p nil
   spaceline-minor-modes-p nil
   spaceline-buffer-encoding-abbrev-p nil
   spaceline-version-control-p nil
   )
  ;; Modes
  (global-evil-mc-mode t)
  ;; clear anzu after mc
  (add-hook 'evil-mc-after-cursors-deleted
            (defun puda/clear-anzu () (setq anzu--state nil)))
  ;; disable show-smartparens-global-mode for performance
  (with-eval-after-load 'smartparens
    (show-smartparens-global-mode -1))
  ;; terminal hooks
  (let ((comint-hooks '(term-mode-hook eshell-mode-hook messages-buffer-mode-hook)))
    (spacemacs/add-to-hooks
     (defun puda-disable-hl-for-term ()
       (setq global-hl-line-mode nil)
       (define-key evil-emacs-state-local-map (kbd "C-y") 'term-paste))
     comint-hooks))
  (evil-set-initial-state 'term-mode 'emacs)
  (push 'term-mode evil-escape-excluded-major-modes)
  ;; leader keybindings
  (evil-leader/set-key
    "." 'dumb-jump-go
    ">" 'dumb-jump-back
    "bk" 'kill-this-buffer
    "bd" 'kill-buffer
    "j;" 'avy-isearch
    "qq" 'spacemacs/frame-killer
    "qz" 'spacemacs/prompt-kill-emacs
    )
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
