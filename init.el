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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(paradox-github-token t))
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
 '(font-latex-sectioning-0-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-1-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-2-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-3-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-4-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-5-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-slide-title-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "#ff9eb8"))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(font-lock-doc-face ((t (:slant italic))))
 '(font-lock-keyword-face ((t (:weight bold))))
 '(font-lock-string-face ((t (:slant italic))))
 '(font-lock-warning-face ((t (:underline nil))))
 '(header-line ((t (:box (:color "#555555" :line-width 1)))))
 '(helm-ff-prefix ((t (:background nil :foreground "#666666" :weight bold))))
 '(helm-prefarg ((t (:foreground "PaleGreen"))))
 '(info-title-1 ((t (:inherit default :height 1.0 :weight bold))))
 '(info-title-2 ((t (:inherit default :height 1.0 :weight bold))))
 '(info-title-3 ((t (:inherit default :height 1.0 :weight bold))))
 '(info-title-4 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-1 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-2 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-3 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-4 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-5 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-6 ((t (:inherit default :height 1.0 :weight bold))))
 '(mode-line ((t (:box (:color "#999999" :line-width 1 :style released-button)))))
 '(mode-line-inactive ((t (:box (:color "#666666" :line-width 1 :style released-button)))))
 '(org-document-title ((t (:inherit default :height 1.0 :weight bold))))
 '(org-done ((t (:foreground "MediumSpringGreen"))))
 '(org-level-1 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-2 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-3 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-4 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-5 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-6 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-7 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-8 ((t (:inherit default :height 1.0 :weight bold))))
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
