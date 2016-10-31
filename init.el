(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation nil
   dotspacemacs-ask-for-lazy-installation nil
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   dotspacemacs-download-packages 'used-but-keep-unused

   dotspacemacs-configuration-layers
   '((auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-private-snippets-directory "~/.spacemacs.d/puda-snippets"
                      )
     (evil-snipe :variables
                 evil-snipe-enable-alternate-f-and-t-behaviors t)
     (git :variables
          git-magit-status-fullscreen t)
     php
     html
     gtags
     (javascript :variables
                 javascript-disable-tern-port-files t)
     react
     (shell :variables
            shell-default-position 'bottom
            shell-default-height 70
            )
     (ranger :variables
             ranger-show-preview t
             ranger-show-dotfiles t
             )
     (spell-checking :variables spell-checking-enable-by-default nil)
     syntax-checking
     (version-control :variables version-control-diff-tool 'diff-hl)
     ;; ivy
     colors
     better-defaults
     emacs-lisp
     org
     (deft :variables
       deft-extensions '("org" "md" "txt")
       deft-directory '"~/Documents/")
     theming
     ;; custom config
     puda-basic
     puda-theming
     (theming :variables
              theming-headings-inherit-from-default 'all
              theming-headings-same-size 'all
              theming-headings-bold 'all)
     evil-little-word
     )

   dotspacemacs-excluded-packages '(
                                    persp-mode ;; causing errors
                                    drupal-mode
                                    eyebrowse
                                    evil-search-highlight-persist
                                    vi-tilde-fringe
                                    )

   dotspacemacs-additional-packages '()
   ))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 10
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'hybrid
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(recents bookmarks projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes
   '(monokai material spacemacs-dark darkokai base16-eighties solarized-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro" :size 28 :weight semibold :width normal :powerline-scale 1.2)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   dotspacemacs-ex-substitute-global t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-transient-state t
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
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers 'relative
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'nil))

(defun dotspacemacs/user-init ()
  ;; Variables
  (setq-default
   ;; Smartparens
   sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   sp-highlight-wrap-tag-overlay nil
   ;; Avy
   avy-all-windows 'all-frames
   ;; Shell
   shell-default-term-shell "/bin/zsh"
   ;; Web
   web-mode-markup-indent-offset 4
   web-mode-css-indent-offset 4
   web-mode-code-indent-offset 4
   ;; General
   default-directory "/var/www/community/"
   initial-scratch-message ";; Puda loves Rachael!! \n;; Puda Emacs Custom Config!!"
   ;; Ranger
   ranger-override-dired t
   ;; Monokai
   monokai-highlight-line "#3A3A3A"
   )
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
   evil-shift-round nil
   ;; Helm Mini
   helm-mini-default-sources '(helm-source-buffers-list helm-source-recentf helm-source-buffer-not-found helm-source-bookmarks)
   ;; Helm
   helm-echo-input-in-header-line nil
   ;; Spaceline
   powerline-default-separator 'arrow
   spaceline-new-version-p nil
   spaceline-buffer-encoding-abbrev-p nil
   spaceline-version-control-p nil
   ;; Magit
   magit-process-ensure-unix-line-ending nil
   )
  ;; Transparency
  ;; (spacemacs/toggle-transparency)
  ;; (add-hook 'after-make-frame-functions 'spacemacs/toggle-transparency)

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
    ;; "." 'dumb-jump-go
    ;; ">" 'dumb-jump-back
    "bk" 'kill-this-buffer
    "bd" 'kill-buffer
    )
  ;; multiple-cursors-mode
  (setq mc/always-run-for-all t)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

  ;; Diminish
  (spacemacs|diminish holy-mode)
  (spacemacs|diminish hybrid-mode)
  (spacemacs|diminish which-key-mode)
  (spacemacs|diminish evil-mc-mode)
  (spacemacs|diminish helm-gtags-mode)
  (spacemacs|diminish ggtags-mode)
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
