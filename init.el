(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation nil
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   dotspacemacs-delete-orphan-packages t
   dotspacemacs-configuration-layers
   '((git :variables
          git-magit-status-fullscreen t)
     spacemacs-helm
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle)
     (evil-snipe :variables
                 evil-snipe-enable-alternate-f-and-t-behaviors t)
     php
     html
     gtags
     (javascript :variables
                 javascript-disable-tern-port-files t)
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
     ibuffer
     unimpaired
     games
     ;; custom config
     puda-basic
     puda-theming
     evil-little-word
     )

   dotspacemacs-additional-packages '()
   dotspacemacs-excluded-packages '(drupal-mode vi-tilde-fringe evil-search-highlight-persist)
   ))


(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'hybrid
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(recents bookmarks projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(monokai material leuven cyberpunk spacemacs-dark solarized-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro" :size 38 :weight semibold :width normal :powerline-scale 1.1)
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
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup t
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title nil
   dotspacemacs-show-transient-state-color-guide nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-line-numbers 'relative
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository 'melpa
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

   ;; Spaceline
   spaceline-buffer-encoding-abbrev-p nil
   spaceline-version-control-p nil
   ;; General
   ;; default-directory "/var/www/community/"
   ;; initial-scratch-message ";; Puda loves Rachael!! \n;; Puda Emacs Custom Config!!"
   )
  ;; nice scrolling
  (setq scroll-margin 0
        scroll-conservatively 10000
        scroll-preserve-screen-position 1)
  ;; transparency
  (push '(alpha . (0.94 . 0.94)) default-frame-alist)
  (set-frame-parameter (selected-frame) 'alpha '(0.94 . 0.94))
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
   ;; Spaceline
   powerline-default-separator 'arrow
   ;; Helm Swoop
   helm-swoop-speed-or-color nil
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
    ;; swapping bk and bd
    "bk" 'kill-this-buffer
    "bd" 'kill-buffer
    "j;" 'avy-isearch
    )
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("51277c9add74612c7624a276e1ee3c7d89b2f38b1609eed6759965f9d4254369" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-annotation ((t (:foreground "#ff9eb8"))))
 '(company-tooltip-annotation-selection ((t (:background "#66d9ef"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(erc-timestamp-face ((t (:inherit font-lock-comment-face :foreground nil))))
 '(evil-search-highlight-persist-highlight-face ((t (:background "#fc5fef" :foreground "#000000"))))
 '(flycheck-fringe-error ((t (:background nil))))
 '(flycheck-fringe-info ((t (:background nil))))
 '(flycheck-fringe-warning ((t (:background nil))))
 '(font-latex-sectioning-0-face ((t (:inherit default :weight bold))))
 '(font-latex-sectioning-1-face ((t (:inherit default :weight bold))))
 '(font-latex-sectioning-2-face ((t (:inherit default :weight bold))))
 '(font-latex-sectioning-3-face ((t (:inherit default :weight bold))))
 '(font-latex-sectioning-4-face ((t (:inherit default :weight bold))))
 '(font-latex-sectioning-5-face ((t (:inherit default :weight bold))))
 '(font-latex-slide-title-face ((t (:inherit default :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "#ff9eb8" :weight semi-bold))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(font-lock-doc-face ((t (:slant italic))))
 '(font-lock-keyword-face ((t (:weight bold))))
 '(font-lock-string-face ((t (:slant italic))))
 '(font-lock-warning-face ((t (:underline nil))))
 '(header-line ((t (:box (:color "#555555" :line-width 1)))))
 '(helm-ff-prefix ((t (:background nil :foreground "#666666" :weight bold))))
 '(helm-prefarg ((t (:foreground "PaleGreen"))))
 '(info-title-1 ((t (:inherit default :weight bold))))
 '(info-title-2 ((t (:inherit default :weight bold))))
 '(info-title-3 ((t (:inherit default :weight bold))))
 '(info-title-4 ((t (:inherit default :weight bold))))
 '(markdown-header-face ((t (:inherit default :weight bold))))
 '(markdown-header-face-1 ((t (:inherit default :weight bold))))
 '(markdown-header-face-2 ((t (:inherit default :weight bold))))
 '(markdown-header-face-3 ((t (:inherit default :weight bold))))
 '(markdown-header-face-4 ((t (:inherit default :weight bold))))
 '(markdown-header-face-5 ((t (:inherit default :weight bold))))
 '(markdown-header-face-6 ((t (:inherit default :weight bold))))
 '(mode-line ((t (:box (:color "#999999" :line-width 1 :style released-button)))))
 '(mode-line-inactive ((t (:box (:color "#666666" :line-width 1 :style released-button)))))
 '(org-document-title ((t (:inherit default :weight bold))))
 '(org-done ((t (:foreground "MediumSpringGreen"))))
 '(org-level-1 ((t (:inherit default :weight bold))))
 '(org-level-2 ((t (:inherit default :weight bold))))
 '(org-level-3 ((t (:inherit default :weight bold))))
 '(org-level-4 ((t (:inherit default :weight bold))))
 '(org-level-5 ((t (:inherit default :weight bold))))
 '(org-level-6 ((t (:inherit default :weight bold))))
 '(org-level-7 ((t (:inherit default :weight bold))))
 '(org-level-8 ((t (:inherit default :weight bold))))
 '(powerline-active1 ((t (:box (:color "#999999" :line-width 1 :style released-button) :background "#5a5a5a"))))
 '(powerline-active2 ((t (:box (:color "#999999" :line-width 1 :style released-button)))))
 '(powerline-inactive1 ((t (:box (:color "#666666" :line-width 1 :style released-button)))))
 '(powerline-inactive2 ((t (:box (:color "#666666" :line-width 1 :style released-button)))))
 '(region ((t (:background "#998f84"))))
 '(spacemacs-transient-state-title-face ((t (:background nil :foreground nil :inherit font-lock-warning-face))))
 '(term ((t (:foreground nil :background nil))))
 '(web-mode-comment-face ((t (:inherit font-lock-comment-face :foreground nil))))
 '(web-mode-html-attr-name-face ((t (:inherit font-lock-variable-name-face :foreground nil))))
 '(web-mode-html-attr-value-face ((t (:inherit font-lock-string-face :foreground nil))))
 '(web-mode-html-tag-bracket-face ((t (:inherit web-mode-html-tag-face :foreground nil))))
 '(web-mode-html-tag-face ((t (:inherit font-lock-builtin-face :foreground nil :weight bold)))))
