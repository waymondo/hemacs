;; don't be annoying
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq inhibit-startup-echo-area-message "")
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq confirm-kill-emacs nil)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; always revert
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(setq global-auto-revert-non-file-buffers t)
(setq imenu-auto-rescan t)
(setq load-prefer-newer t)

;; basic editing
(transient-mark-mode t)
(delete-selection-mode t)
(setq require-final-newline t)
(setq default-major-mode 'text-mode)
(setq-default indent-tabs-mode nil)
(setq standard-indent 2)
(setq-default tab-width 2)
(setq kill-do-not-save-duplicates t)
(setq kill-whole-line t)
(setq x-select-enable-clipboard t)
(put 'downcase-region 'disabled nil)
(put 'capitalize-region 'disabled nil)
(allow-line-as-region-for-function comment-or-uncomment-region)
(allow-line-as-region-for-function capitalize-region)
(allow-line-as-region-for-function downcase-region)

;; electric pairs
(electric-pair-mode 1)
(setq electric-pair-pairs '
      ((?\( . ?\))
       (?\" . ?\")
       (?\{ . ?\})
       (?\[ . ?\])
       (?\| . ?\|)
       (?\< . ?\>)))
(setq electric-pair-text-pairs '
      ((?\" . ?\")
       (?\' . ?\')
       (?\` . ?\`)))
(setq electric-pair-delete-adjacent-pairs t)

;; subword
(global-subword-mode 1)
(setq subword-forward-regexp "\\W*\\(\\([_[:upper:]]*\\(\\W\\)?\\)[[:lower:][:digit:]]*\\)")
(setq subword-backward-regexp "\\(\\(\\W\\|[[:lower:][:digit:]]\\)\\([[:upper:]]+\\W*\\)\\|\\W\\w+\\|_\\w+\\)")
(define-key subword-mode-map [remap backward-kill-word] 'subword-backward-delete)

;; completion
(setq completion-auto-help 'lazy)
(setq dabbrev-case-distinction t)
(setq dabbrev-case-fold-search t)
(setq dabbrev-case-replace t)
(setq dabbrev-upcase-means-case-search t)
(setq dabbrev-abbrev-skip-leading-regexp ":")
(setq hippie-expand-verbose nil)
(setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible
                                         try-expand-dabbrev
                                         try-expand-dabbrev-matching-buffers
                                         try-expand-dabbrev-other-buffers
                                         try-expand-dabbrev-from-kill
                                         try-expand-all-abbrevs
                                         try-complete-file-name-partially
                                         try-complete-file-name))

(add-λ 'emacs-lisp-mode-hook
  (set (make-local-variable 'hippie-expand-try-functions-list)
       (append '(try-complete-lisp-symbol-partially
                 try-complete-lisp-symbol)
               hippie-expand-try-functions-list)))

;; files
(add-λ 'before-save-hook
  (unless (eq major-mode 'markdown-mode)
    (delete-trailing-whitespace)))
(add-λ 'before-save-hook
  (when (region-active-p) (deactivate-mark t)))
(add-hook 'after-save-hook 'byte-compile-current-buffer)
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(add-hook 'find-file-hook 'sm-try-smerge t)

;; history
(setq history-length 100)
(setq history-delete-duplicates t)
(savehist-mode 1)
(setq savehist-additional-variables
      '(search-ring regexp-search-ring comint-input-ring))
(setq savehist-autosave-interval 30)
(recentf-mode t)
(setq recentf-max-saved-items 500)
(setq recentf-exclude '(".ido.last" "COMMIT_EDITMSG"))
(setq initial-buffer-choice (car recentf-list))

;; comint & compilation
(setq explicit-shell-file-name "bash")
(setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
(setq comint-process-echoes t)
(setq-default comint-prompt-read-only t)
(setq-default comint-input-ignoredups t)
(setq comint-buffer-maximum-size 5000)
(add-to-list 'comint-output-filter-functions 'comint-truncate-buffer)
(add-to-list 'comint-output-filter-functions 'comint-strip-ctrl-m)
(setq compilation-disable-input t)
(setq compilation-message-face nil)
(setq compilation-always-kill t)
(setq next-error-recenter t)
(add-λ 'shell-mode-hook
  (turn-on-comint-history (getenv "HISTFILE")))
(dolist (hook '(comint-mode-hook compilation-mode-hook))
  (add-hook hook 'hemacs-shellish-hook))

;; minibuffer
(setq echo-keystrokes 0.1)
(setq completion-pcm-complete-word-inserts-delimiters t)
(setq minibuffer-eldef-shorten-default t)
(setq enable-recursive-minibuffers t)
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
(minibuffer-electric-default-mode t)
(minibuffer-depth-indicate-mode 1)

;; osx-ish
(setq ns-use-native-fullscreen nil)
(setq mac-function-modifier 'hyper)
(setq browse-url-browser-function 'browse-url-default-macosx-browser)
(setq ns-pop-up-frames nil)
(setq ns-use-srgb-colorspace t)
(setq delete-by-moving-to-trash t)
(setq gc-cons-threshold 20000000)

;; windows & scrolling
(setq split-height-threshold nil)
(setq split-width-threshold 0)
(setq pop-up-windows nil)
(setq scroll-margin 24)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)
(setq truncate-partial-width-windows 90)

;; visual
(setq-default cursor-type 'bar)
(setq blink-cursor-blinks 0)
(show-paren-mode t)
(setq linum-format " %3s ")
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)
(fringe-mode '(24 . 8))
(column-number-mode t)
(global-hl-line-mode t)
(set-face-attribute 'default nil :height 180 :font "Inconsolata")
(setq-default mode-line-format
              '("%e" mode-line-front-space mode-line-modified
                " " mode-line-buffer-identification " "
                mode-line-position (vc-mode vc-mode) " "
                mode-name " " mode-line-misc-info mode-line-end-spaces))
