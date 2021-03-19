;; -*- lexical-binding: t -*-

(load (concat user-emacs-directory "lib.el"))

;;;;; Personal Variables & Key Maps

(defconst indent-sensitive-modes '(coffee-mode slim-mode yaml-mode))
(defconst writing-modes '(org-mode markdown-mode fountain-mode git-commit-mode))
(defconst *is-mac* (eq system-type 'darwin))
(defconst default-font-size 15)
(defconst hemacs-posframe-delay 3)
(define-prefix-command 'hemacs-git-map)
(define-prefix-command 'hemacs-switch-project-map)
(define-prefix-command 'hemacs-help-map)
(bind-key "s-g" #'hemacs-git-map)
(bind-key "s-o" #'hemacs-switch-project-map)
(bind-key "s-h" #'hemacs-help-map)

;;;;; Bootstrap

(use-feature emacs
  :custom
  (load-prefer-newer t)
  (history-length 128)
  (history-delete-duplicates t)
  (maximum-scroll-margin 0.5)
  (scroll-margin 50)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position 'always)
  (auto-window-vscroll nil)
  (echo-keystrokes 1e-6)
  (ns-use-native-fullscreen nil)
  (delete-by-moving-to-trash t)
  (ring-bell-function #'ignore)
  (ns-function-modifier 'control)
  (create-lockfiles nil)
  (disabled-command-function nil)
  (ad-redefinition-action 'accept)
  (custom-safe-themes t)
  (custom-file (make-temp-file "emacs-custom"))
  (initial-scratch-message nil)
  (inhibit-startup-echo-area-message "")
  (standard-indent 2)
  (enable-recursive-minibuffers t)
  (kill-buffer-query-functions nil)
  (ns-pop-up-frames nil)
  (shell-file-name (getenv "SHELL"))
  (frame-inhibit-implied-resize t)
  (fast-but-imprecise-scrolling t)
  (redisplay-skip-fontification-on-input t)
  (read-process-output-max (* 1024 1024))
  (use-short-answers t)
  (x-underline-at-descent-line t)
  :config
  (setq-default indent-tabs-mode nil
                line-spacing 2
                tab-width 2
                c-basic-offset 2
                cursor-type 'bar
                cursor-in-non-selected-windows nil
                bidi-display-reordering 'left-to-right
                fill-column 100
                truncate-lines t)
  (defun hemacs-minibuffer-setup-hook ()
    (defer-garbage-collection)
    (set-window-fringes (minibuffer-window) 0 0 nil)
    (setq-local input-method-function nil))
  (add-hook 'minibuffer-setup-hook #'hemacs-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'restore-garbage-collection)
  (add-hook 'emacs-startup-hook #'restore-garbage-collection 100))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package dash
  :config
  (dash-enable-font-lock))

(use-package transform-string-at-point
  :straight
  (:host github :repo "waymondo/transform-string-at-point")
  :custom
  (transform-string-at-point-cursor-after-transform 'next-string)
  :bind-keymap
  ("s-;" . transform-string-at-point-map))

(use-feature tool-bar
  :config
  (tool-bar-mode -1))

(use-feature scroll-bar
  :config
  (scroll-bar-mode -1))

(use-feature menu-bar
  :bind
  ("s-w" . kill-this-buffer))

;;;;; Processes, Shells, Compilation

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (push "HISTFILE" exec-path-from-shell-variables)
  (exec-path-from-shell-initialize))

(use-package alert
  :custom
  (alert-default-style (if *is-mac* 'osx-notifier 'message))
  :config
  (defun alert-after-finish-in-background (buf str)
    (when (or (not (get-buffer-window buf 'visible)) (not (frame-focus-state)))
      (alert str :buffer buf))))

(use-feature comint
  :bind
  (:map comint-mode-map
        ("RET"       . comint-return-dwim)
        ("C-r"       . comint-history-isearch-backward-regexp)
        ("s-k"       . comint-clear-buffer)
        ("M-TAB"     . comint-previous-matching-input-from-input)
        ("<M-S-tab>" . comint-next-matching-input-from-input))
  :custom
  (comint-prompt-read-only t)
  :hook
  (comint-mode . text-smaller-no-truncation)
  :config
  (setq-default comint-input-ignoredups t
                comint-scroll-show-maximum-output nil
                comint-output-filter-functions
                '(ansi-color-process-output
                  comint-truncate-buffer
                  comint-watch-for-password-prompt))
  (defun turn-on-comint-history (history-file)
    (setq comint-input-ring-file-name history-file)
    (comint-read-input-ring 'silent))
  (def comint-return-dwim
    (cond
     ((comint-after-pmark-p)
      (comint-send-input))
     ((ffap-url-at-point)
      (browse-url (ffap-url-at-point)))
     ((ffap-file-at-point)
      (find-file (ffap-file-at-point)))
     (t
      (comint-next-prompt 1))))
  (defun write-input-ring-for-shellish-modes ()
    (when (-any? #'derived-mode-p '(comint-mode term-mode))
      (comint-write-input-ring)))
  (defun write-input-ring-for-all-shellish-modes ()
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer (write-input-ring-for-shellish-modes))))
  (add-hook 'kill-buffer-hook #'write-input-ring-for-shellish-modes)
  (add-hook 'kill-emacs-hook #'write-input-ring-for-all-shellish-modes))

(use-feature compile
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  :hook
  (compilation-mode . text-smaller-no-truncation)
  :init
  (add-hook 'compilation-finish-functions #'alert-after-finish-in-background))

(use-package profiler
  :bind
  ("C-x p r"  . profiler-report)
  ("C-x p 1"  . profiler-start)
  ("C-x p 0"  . profiler-stop))

(use-package warnings
  :custom
  (warning-suppress-types '((comp) (undo discard-info))))

(use-package shell
  :defer t
  :custom
  (explicit-shell-file-name (getenv "SHELL"))
  :config
  (defun make-shell-command-behave-interactively (f &rest args)
    (let ((shell-command-switch "-ic"))
      (apply f args)))
  (advice-add 'shell-command :around #'make-shell-command-behave-interactively)
  (advice-add 'start-process-shell-command :around #'make-shell-command-behave-interactively)
  (add-λ 'shell-mode-hook
    (turn-on-comint-history (getenv "HISTFILE"))))

(use-package term
  :bind
  (:map term-raw-map
        ("s-k" . comint-clear-buffer)
        ("s-v" . term-paste)
        ([remap term-send-input] . term-return-dwim))
  :custom
  (term-input-ring-file-name (getenv "HISTFILE"))
  :hook
  (term-mode . text-smaller-no-truncation)
  :config
  (def term-return-dwim
    (cond
     ((term-after-pmark-p)
      (term-send-input))
     ((ffap-url-at-point)
      (browse-url (ffap-url-at-point)))
     ((ffap-file-at-point)
      (find-file (ffap-file-at-point)))
     (t
      (term-next-prompt 1)))))

(use-package sh-script
  :ensure-system-package shfmt
  :mode
  ((rx (and (? ".") (or "bash" "zsh" "zprofile"))) . sh-mode)
  :config
  (setq-default sh-indentation 2
                sh-basic-offset 2))

(use-package executable
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package repl-toggle
  :custom
  (rtog/mode-repl-alist
   '((emacs-lisp-mode . ielm)
     (ruby-mode . inf-ruby)
     (js-mode . nodejs-repl)
     (typescript-mode . run-ts)))
  :config
  (repl-toggle-mode))

;;;;; Files & History

(use-feature image-mode
  :hook
  (image-mode . show-image-dimensions-in-mode-line)
  :custom
  (image-animate-loop t)
  :mode "\\.otf\\'"
  :config
  (after moody
    (defun show-image-dimensions-in-mode-line ()
      (let* ((image-dimensions (image-size (image-get-display-property) :pixels))
             (width (car image-dimensions))
             (height (cdr image-dimensions)))
        (setq moody-mode-line-buffer-identification
              `(:eval (moody-tab (format "%s %dx%d" (propertized-buffer-identification "%b") ,width ,height)
                                 20 'down)))))))

(use-feature files
  :custom
  (require-final-newline t)
  (confirm-kill-processes nil)
  (confirm-kill-emacs nil)
  (enable-local-variables :safe)
  (confirm-nonexistent-file-or-buffer nil)
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (large-file-warning-threshold 50000000)
  :config
  (defun hemacs-save-hook ()
    (unless (member major-mode '(markdown-mode gfm-mode sql-mode))
      (delete-trailing-whitespace))
    (when (region-active-p)
      (deactivate-mark t)))
  (add-hook 'before-save-hook #'hemacs-save-hook)
  (defun find-file-maybe-make-directories ()
    (let ((dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p dir)
        (make-directory dir t))))
  (push #'find-file-maybe-make-directories find-file-not-found-functions))

(use-package savehist
  :custom
  (savehist-additional-variables
   '(search-ring regexp-search-ring comint-input-ring projector-command-history))
  :config
  (savehist-mode))

(use-package saveplace
  :init
  (save-place-mode))

(use-package recentf
  :custom
  (recentf-auto-cleanup 200)
  (recentf-max-saved-items 200)
  :config
  (advice-add 'recentf-cleanup :around #'inhibit-message-in-minibuffer)
  (recentf-mode))

(use-feature dired
  :custom
  (dired-create-destination-dirs t)
  (dired-use-ls-dired nil)
  (dired-dwim-target t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-auto-revert-buffer t))

(use-package dired-sidebar
  :bind
  ("s-\\" . dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-theme 'vscode))

(use-package osx-trash
  :if *is-mac*
  :ensure-system-package trash
  :init (osx-trash-setup))

(use-package terminal-here
  :if *is-mac*)

(use-package reveal-in-osx-finder
  :if *is-mac*)

(when *is-mac*
  (bind-keys
   :prefix-map hemacs-osx-map
   :prefix "C-c o"
   ("t" . terminal-here)
   ("f" . reveal-in-osx-finder)))

;;;;; Editing

(use-feature newcomment
  :bind
  ("s-." . insert-todo-comment)
  ("s-/" . comment-line)
  :config
  (def insert-todo-comment
    (call-interactively #'comment-dwim)
    (ensure-space :before)
    (insert "TODO:")
    (ensure-space :after)))

(use-package face-remap
  :hook
  (writing-modes . variable-pitch-mode))

(use-feature simple
  :custom
  (set-mark-command-repeat-pop t)
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  (yank-pop-change-selection t)
  (idle-update-delay 2)
  (next-error-recenter t)
  (async-shell-command-buffer 'new-buffer)
  (shell-command-prompt-show-cwd t)
  (what-cursor-show-names t)
  :bind
  ("s-k" . kill-whole-line)
  ("M-`" . list-processes)
  ("s-z" . undo-only)
  ("s-Z" . undo-redo)
  (:map minibuffer-local-map
        ("<escape>"  . abort-recursive-edit)
        ("M-TAB"     . previous-complete-history-element)
        ("<M-S-tab>" . next-complete-history-element))
  :hook
  (writing-modes . auto-fill-mode)
  :config
  (column-number-mode)
  (defun pop-to-mark-command-until-new-point (f &rest args)
    (let ((p (point)))
      (dotimes (_i 10)
        (when (= p (point))
          (apply f args)))))
  (defun maybe-indent-afterwards (&optional _)
    (and (not current-prefix-arg)
         (not (member major-mode indent-sensitive-modes))
         (or (-any? #'derived-mode-p '(prog-mode sgml-mode)))
         (indent-region (region-beginning) (region-end) nil)))
  (defun pop-to-process-list-buffer ()
    (pop-to-buffer "*Process List*"))
  (defun kill-or-join-line (f &rest args)
    (if (not (eolp))
        (apply f args)
      (delete-indentation 1)
      (when (and (eolp) (not (eq (point) (point-max))))
        (kill-or-join-line f args))))
  (defun move-beginning-of-line-or-indentation (f &rest args)
    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (apply f args))))
  (advice-add 'pop-to-mark-command :around #'pop-to-mark-command-until-new-point)
  (advice-add 'yank :after #'maybe-indent-afterwards)
  (advice-add 'yank-pop :after #'maybe-indent-afterwards)
  (advice-add 'list-processes :after #'pop-to-process-list-buffer)
  (advice-add 'backward-kill-word :around #'delete-region-instead-of-kill-region)
  (advice-add 'kill-line :around #'kill-or-join-line)
  (advice-add 'kill-visual-line :around #'kill-or-join-line)
  (advice-add 'move-beginning-of-line :around #'move-beginning-of-line-or-indentation)
  (advice-add 'beginning-of-visual-line :around #'move-beginning-of-line-or-indentation))

(use-package delsel
  :init
  (delete-selection-mode))

(use-package elec-pair
  :init
  (electric-pair-mode))

(use-package electric
  :custom
  (electric-quote-string t)
  (electric-quote-context-sensitive t)
  :hook
  (writing-modes . electric-quote-local-mode))

(use-package subword
  :init
  (global-subword-mode))

(use-package expand-region
  :bind*
  ("C-," . er/expand-region))

(use-package change-inner
  :bind
  ("M-i" . change-inner)
  ("M-o" . change-outer)
  :config
  (advice-add 'change-inner* :around #'delete-region-instead-of-kill-region))

(use-package avy
  :custom
  (avy-style 'de-bruijn)
  :bind
  (:map dired-mode-map ("." . avy-goto-word-or-subword-1))
  :chords
  ("jj" . avy-goto-char-timer)
  ("jk" . avy-goto-word-or-subword-1)
  ("jl" . avy-goto-line)
  :config
  (avy-setup-default))

(use-package ace-link
  :bind
  ("M-g e" . avy-jump-error)
  :config
  (ace-link-setup-default)
  (defun avy-jump-error-next-error-hook ()
    (let ((compilation-buffer (compilation-find-buffer)))
      (quit-window nil (get-buffer-window compilation-buffer))
      (recenter)))
  (def avy-jump-error
    (let ((compilation-buffer (compilation-find-buffer))
          (next-error-hook '(avy-jump-error-next-error-hook)))
      (when compilation-buffer
        (with-current-buffer compilation-buffer
          (when (derived-mode-p 'compilation-mode)
            (pop-to-buffer compilation-buffer)
            (ace-link-compilation)))))))

(use-package smart-newline
  :bind
  ("<s-return>" . eol-then-smart-newline)
  :hook
  (prog-mode . maybe-enable-smart-newline-mode)
  :init
  (defun maybe-enable-smart-newline-mode ()
    (when (not (member major-mode indent-sensitive-modes))
      (smart-newline-mode)))
  (def eol-then-smart-newline
    (move-end-of-line nil)
    (smart-newline)))

(use-package easy-kill
  :bind
  ([remap kill-ring-save] . easy-kill)
  ([remap mark-sexp]      . easy-mark))

(use-package shift-number
  :bind
  ("<M-up>"   . shift-number-up)
  ("<M-down>" . shift-number-down))

(use-package multiple-cursors
  :bind
  ("s-d"     . mc/mark-next-like-this)
  ("s-D"     . mc/mark-previous-like-this)
  ("C-c s-d" . mc/mark-all-like-this-dwim)
  :hook
  (before-save . mc/keyboard-quit))

(use-package crux
  :bind
  ("s-," . crux-find-user-init-file)
  ("s-D" . crux-duplicate-current-line-or-region)
  ("s-W" . crux-delete-file-and-buffer)
  ("s-S" . crux-rename-file-and-buffer)
  :config
  (defun crux-ignore-vc-backend (orig-fun &rest args)
    (cl-letf (((symbol-function 'vc-backend) #'ignore))
      (apply orig-fun args)))
  (advice-add 'crux-rename-file-and-buffer :around #'crux-ignore-vc-backend)
  (advice-add 'crux-delete-file-and-buffer :around #'crux-ignore-vc-backend)
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-point-to-eol kill-ring-save))

(use-package cycle-quotes
  :bind
  ("C-'" . cycle-quotes))

(use-package scratch
  :bind
  ("s-N" . scratch))

(use-package flyspell
  :hook
  (writing-modes . flyspell-mode))

(use-package flyspell-correct-ivy
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-previous-word-generic)))

(use-package smart-backspace
  :config
  (bind-key "<backspace>" #'smart-backspace global-map
            (and (member (char-before) (string-to-list " \t"))
                 (not (or (region-active-p) (member major-mode indent-sensitive-modes))))))

(use-package smart-shift
  :bind
  ("s-[" . smart-shift-left)
  ("s-]" . smart-shift-right)
  :config
  (advice-add 'smart-shift-override-local-map :override #'ignore))

(use-package drag-stuff
  :bind
  ("<s-down>" . drag-stuff-down)
  ("<s-up>"   . drag-stuff-up)
  :config
  (defun indent-unless-sensitive (_arg)
    (unless (member major-mode indent-sensitive-modes)
      (indent-according-to-mode)))
  (advice-add 'drag-stuff-line-vertically :after #'indent-unless-sensitive)
  (advice-add 'drag-stuff-lines-vertically :after #'indent-unless-sensitive))

;;;;; Completion

(use-package ivy
  :custom
  (ivy-extra-directories nil)
  (ivy-re-builders-alist
   '((counsel-ag . ivy--regex-plus)
     (t . ivy--regex-fuzzy)))
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-format-function #'ivy-format-function-arrow)
  :bind
  ("s-b" . ivy-switch-buffer)
  (:map ivy-switch-buffer-map
        ("s-k" . ivy-switch-buffer-kill))
  :chords
  (";s" . ivy-switch-buffer)
  :init
  (ivy-mode))

(use-package ivy-hydra
  :after ivy)

(use-package prescient
  :config
  (prescient-persist-mode))

(use-package ivy-prescient
  :custom
  (ivy-prescient-retain-classic-highlighting t)
  :config
  (ivy-prescient-mode))

(use-package company-prescient
  :config
  (company-prescient-mode))

(use-package ivy-xref
  :after ivy
  :custom
  (xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package hippie-exp
  :custom
  (hippie-expand-verbose nil)
  (hippie-expand-try-functions-list
   '(yas-hippie-try-expand
     try-expand-dabbrev-visible
     try-expand-dabbrev
     try-expand-dabbrev-matching-buffers
     try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-dabbrev-other-buffers))
  :bind
  ([remap dabbrev-expand] . hippie-expand)
  (:map read-expression-map ("TAB" . hippie-expand))
  (:map minibuffer-local-map ("TAB" . hippie-expand))
  :bind*
  ("M-'" . hippie-expand-line)
  :hook
  ((emacs-lisp-mode ielm-mode) . hippie-expand-allow-lisp-symbols)
  :init
  (defun try-expand-dabbrev-matching-buffers (old)
    (let ((hippie-expand-only-buffers `(,major-mode)))
      (try-expand-dabbrev-all-buffers old)))
  (defun try-expand-dabbrev-other-buffers (old)
    (let ((hippie-expand-ignore-buffers `(,major-mode)))
      (try-expand-dabbrev-all-buffers old)))
  (defun hippie-expand-case-sensitive (f &rest args)
    (let ((case-fold-search nil))
      (apply f args)))
  (defun inhibit-message-in-minibuffer (f &rest args)
    (let ((inhibit-message (minibufferp)))
      (apply f args)))
  (defun hippie-expand-maybe-kill-to-eol (f &rest args)
    (unless (eolp)
      (kill-line))
    (apply f args))
  (defalias 'hippie-expand-line (make-hippie-expand-function
                                 '(try-expand-line
                                   try-expand-line-all-buffers)))
  (advice-add 'hippie-expand :around #'hippie-expand-case-sensitive)
  (advice-add 'hippie-expand :around #'inhibit-message-in-minibuffer)
  (advice-add 'hippie-expand-line :around #'hippie-expand-maybe-kill-to-eol)
  (defun hippie-expand-allow-lisp-symbols ()
    (setq-local hippie-expand-try-functions-list
                (append '(try-complete-lisp-symbol-partially
                          try-complete-lisp-symbol)
                        hippie-expand-try-functions-list))))

(use-package yasnippet
  :custom
  (yas-wrap-around-region t)
  :mode
  ("\\.yasnippet\\'" . snippet-mode)
  :init
  (defun yas-indent-unless-case-sensitive (f &rest args)
    (let ((yas-indent-line (if (member major-mode indent-sensitive-modes) nil 'auto)))
      (apply f args)))
  (delete 'yas-installed-snippets-dir yas-snippet-dirs)
  (advice-add 'yas--indent :around #'yas-indent-unless-case-sensitive)
  (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand)
  (yas-global-mode))

(use-package company
  :custom
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-require-match nil)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-idle-delay #'company-conditional-idle-delay)
  (company-occurrence-weight-function #'company-occurrence-prefer-any-closest)
  (company-transformers '(company-sort-prefer-same-case-prefix))
  (company-dabbrev-minimum-length 2)
  (company-dabbrev-code-modes t)
  (company-dabbrev-code-everywhere t)
  (company-backends '(company-capf company-files (company-dabbrev-code company-etags) company-dabbrev))
  :bind
  ([remap completion-at-point] . company-manual-begin)
  ([remap complete-symbol] . company-manual-begin)
  :init
  (defun company-conditional-idle-delay ()
    (if (company-in-string-or-comment) nil 0.3))
  (dolist (command '(comint-previous-matching-input-from-input comint-next-matching-input-from-input))
    (add-to-list 'company-continue-commands command))
  (global-company-mode))

(use-package company-posframe
  :custom
  (company-posframe-show-indicator nil)
  (company-posframe-show-metadata nil)
  (company-posframe-quickhelp-delay nil)
  :config
  (company-posframe-mode))

(use-package bash-completion
  :init
  (bash-completion-setup))

(use-package company-emoji
  :after company
  :hook
  (writing-modes . company-add-local-emoji-backend)
  :config
  (when *is-mac*
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))
  (defun company-add-local-emoji-backend ()
    (setq-local company-backends (append '(company-emoji) company-backends))))

(use-package emoji-cheat-sheet-plus
  :bind
  ("C-c e" . emoji-cheat-sheet-plus-insert)
  :hook
  (writing-modes . emoji-cheat-sheet-plus-display-mode))

(use-package smart-tab
  :config
  (global-smart-tab-mode)
  (push 'shell-mode smart-tab-disabled-major-modes)
  :custom
  (smart-tab-using-hippie-expand t)
  (smart-tab-completion-functions-alist '()))

;;;;; Navigation & Search

(use-package goto-line-preview
  :bind
  ([remap goto-line] . goto-line-preview)
  :config
  (defun with-display-line-numbers (f &rest args)
    (make-local-variable 'display-line-numbers)
    (let ((display-line-numbers t))
      (apply f args)))
  (advice-add 'goto-line-preview :around #'with-display-line-numbers))

(use-feature window
  :chords
  (";w" . toggle-split-window)
  (":W" . delete-other-windows)
  (":Q" . delete-side-windows)
  :custom
  (display-buffer-alist
   `((,(rx (or "ivy-todo.org"
               (and bos (or "*Flycheck errors*" "*Backtrace" "*Warnings" "*compilation" "*Help"
                            "*helpful" "*ivy-occur" "*less-css-compilation" "*format-all-errors"
                            "*Packages" "*Flymake" "*SQL" "*Occur" "*helm emoji" "*Process List"
                            "*Free keys" "new-issue" "COMMIT_EDITMSG" "*MDN CSS" "*xref" "*rails"
                            "*rspec-compilation" "*lsp-help"))))
      (display-buffer-reuse-window
       display-buffer-in-side-window)
      (side            . bottom)
      (reusable-frames . visible)
      (window-height   . 0.37))
     ("." nil (reusable-frames . visible))))
  (switch-to-buffer-obey-display-actions t)
  (split-width-threshold 190)
  :config
  (def toggle-split-window
    (if (eq last-command 'toggle-split-window)
        (progn
          (jump-to-register :toggle-split-window)
          (setq this-command 'toggle-unsplit-window))
      (window-configuration-to-register :toggle-split-window)
      (switch-to-buffer-other-window nil)))
  (def delete-side-windows
    (dolist (window (window-at-side-list nil 'bottom))
      (quit-window nil window)
      (when (window-live-p window)
        (delete-window window)))))

(use-package wgrep-ag
  :after ag
  :custom
  (wgrep-auto-save-buffer t)
  :hook
  (rg-mode . wgrep-ag-setup))

(use-package rg
  :ensure-system-package rg
  :chords
  (":G" . rg-project)
  :hook
  (after-init . rg-enable-default-bindings))

(use-package bm
  :bind
  ("s-1" . bm-toggle)
  ("s-2" . bm-next)
  ("s-@" . bm-previous)
  :custom
  (bm-cycle-all-buffers t))

(use-package anzu
  :bind
  ([remap query-replace] . anzu-query-replace)
  ("s-q" . anzu-query-replace)
  ("C-q" . anzu-query-replace-at-cursor-thing)
  :config
  (global-anzu-mode))

(use-package imenu
  :custom
  (imenu-auto-rescan t)
  :hook
  (emacs-lisp-mode . hemacs-imenu-elisp-expressions)
  :config
  (defun hemacs-imenu-elisp-expressions ()
    (dolist (pattern '((nil "^[[:space:]]*(def \\(.+\\)$" 1)
                       ("Features" "^(use-feature \\(.+\\)$" 1)
                       ("Sections" "^;;;;; \\(.+\\)$" 1)))
      (add-to-list 'imenu-generic-expression pattern))))

(use-package imenu-anywhere
  :chords
  (";r" . ivy-imenu-anywhere))

(use-package projectile
  :bind
  ("s-p" . projectile-command-map)
  ("C-x m" . projectile-run-shell)
  (:map hemacs-switch-project-map
        ("t" . projectile-switch-project)
        ("m" . projectile-switch-project-projectile-run-shell)
        ("g" . projectile-switch-project-projectile-vc)
        ("u" . projectile-switch-project-projectile-run-project)
        ("f" . projectile-switch-project-projectile-find-file))
  :chords
  (";t" . projectile-find-file)
  :custom
  (projectile-enable-caching t)
  (projectile-verbose nil)
  (projectile-completion-system 'ivy)
  (projectile-require-project-root nil)
  :config
  (put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
  (defmacro make-projectile-switch-project-defun (func)
    `(let ((defun-name (format "projectile-switch-project-%s" (symbol-name ,func))))
       (defalias (intern defun-name)
         (function
          (lambda ()
            (interactive)
            (let ((projectile-switch-project-action ,func))
              (projectile-switch-project)))))))
  (make-projectile-switch-project-defun #'projectile-run-shell)
  (make-projectile-switch-project-defun #'projectile-run-project)
  (make-projectile-switch-project-defun #'projectile-find-file)
  (make-projectile-switch-project-defun #'projectile-vc)
  (projectile-mode)
  (projectile-cleanup-known-projects))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode)
  :bind
  ("s-t" . counsel-projectile)
  :chords
  (";g" . counsel-projectile-rg))

(use-package projector
  :after projectile
  :bind
  ("C-x RET"        . projector-run-shell-command-project-root)
  ("C-x <C-return>" . projector-run-default-shell-command)
  (:map comint-mode-map ("s-R" . projector-rerun-buffer-process))
  (:map hemacs-switch-project-map
        ("<C-return>" . projectile-switch-project-projector-run-default-shell-command)
        ("M" . projectile-switch-project-projector-run-shell-command-project-root))
  :custom
  (projector-completion-system 'ivy)
  (projector-command-modes-alist
   '(("^heroku run console" . inf-ruby-mode)))
  :config
  (make-projectile-switch-project-defun #'projector-run-shell-command-project-root)
  (make-projectile-switch-project-defun #'projector-run-default-shell-command))

(use-package ctrlf
  :config
  (ctrlf-mode)
  :custom
  (ctrlf-auto-recenter t))

(use-package counsel
  :bind
  ([remap execute-extended-command] . counsel-M-x)
  ("s-P" . counsel-M-x)
  (:map hemacs-help-map ("o" . counsel-find-library))
  (:map minibuffer-local-map ("C-r" . 'counsel-minibuffer-history))
  :chords
  (";f" . counsel-find-file))

(use-package ivy-todo
  :after projectile
  :bind
  ("s-n" . ivy-todo)
  :config
  (defun ivy-todo-use-local-project-todo-file (f &rest args)
    (let ((ivy-todo-file (expand-file-name "ivy-todo.org" (projectile-project-root))))
      (apply f args)))
  (advice-add 'ivy-todo :around #'ivy-todo-use-local-project-todo-file))

(use-package beginend
  :config
  (beginend-global-mode))

;;;;; External Utilities

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server)
  :custom
  (atomic-chrome-default-major-mode 'gfm-mode))

(use-package restart-emacs
  :bind
  ([remap save-buffers-kill-terminal] . restart-emacs))

;;;;; Major Modes

(use-package csv-mode
  :hook
  (csv-mode . csv-align-mode))

(use-package org
  :bind
  (:map org-mode-map
        ("," . self-with-space)
        ("C-c C-." . org-todo)
        ("C-c t" . insert-date))
  :custom
  (org-support-shift-select t)
  (org-startup-indented t)
  :config
  (def insert-date
    (insert (format-time-string "%m/%d/%Y")))
  (advice-add 'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window))

(use-package org-autolist
  :after org
  :hook
  (org-mode . org-autolist-mode))

(use-package sgml-mode
  :ensure-system-package
  (html-languageserver . "npm i -g vscode-html-languageserver-bin")
  :bind
  (:map html-mode-map
        ("," . self-with-space)
        ("<C-return>" . html-newline-dwim))
  :chords
  ("<>" . sgml-close-tag)
  :hook
  (sgml-mode . sgml-electric-tag-pair-mode)
  :config
  (add-λ 'sgml-mode-hook
    (run-hooks 'prog-mode-hook))
  (modify-syntax-entry ?= "." html-mode-syntax-table)
  (modify-syntax-entry ?\' "\"'" html-mode-syntax-table)
  (def html-newline-dwim
    (move-end-of-line nil)
    (smart-newline)
    (sgml-close-tag)
    (move-beginning-of-line nil))
  (bind-key "'" "’" html-mode-map (eq 0 (car (syntax-ppss)))))

(use-package web-mode
  :mode
  ("\\.erb\\'"        . web-mode)
  ("\\.php\\'"        . web-mode)
  ("\\.hbs\\'"        . web-mode)
  ("\\.handlebars\\'" . web-mode)
  ("\\.tsx\\'"        . web-mode)
  :bind
  (:map web-mode-map
        ("," . self-with-space)
        ("<C-return>" . html-newline-dwim)
        ("C-c C-." . typescript-mode))
  :custom
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-current-element-highlight t))

(use-package lorem-ipsum
  :config
  (lorem-ipsum-use-default-bindings))

(use-package emmet-mode
  :after web-mode
  :hook
  (sgml-mode web-mode))

(use-package fountain-mode
  :mode "\\.fountain$")

(use-package markdown-mode
  :mode
  ("\\.md\\'" . gfm-mode)
  ("\\.markdown\\'" . gfm-mode)
  :bind
  (:map markdown-mode-map ("," . self-with-space))
  :ensure-system-package (marked . "npm i -g marked")
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "marked")
  (markdown-indent-on-enter nil))

(use-package vmd-mode
  :after markdown-mode
  :bind
  (:map markdown-mode-map ("C-x p" . vmd-mode))
  :ensure-system-package (vmd . "npm i -g vmd"))

(use-package pandoc-mode
  :after (markdown-mode org-mode)
  :ensure-system-package pandoc
  :hook
  (markdown-mode org-mode)
  (pandoc-mode . pandoc-load-default-settings))

(use-package css-mode
  :ensure-system-package
  (css-languageserver . "npm i -g vscode-css-languageserver-bin")
  :mode "\\.css\\.erb\\'"
  :bind
  (:map css-mode-map
        ("," . self-with-space)
        ("{" . open-brackets-newline-and-indent))
  :custom
  (css-indent-offset 2))

(use-package less-css-mode
  :ensure-system-package (lessc . "npm i -g less")
  :custom
  (less-css-lessc-options '("--no-color" "-x")))

(use-package js
  :ensure-system-package
  (eslint_d . "npm install -g eslint_d")
  :bind
  (:map js-mode-map
        ("," . self-with-space)
        ("=" . pad-equals)
        (":" . self-with-space))
  :mode
  ("\\.js\\'" . js-mode)
  :custom
  (js-indent-level 2)
  (js-switch-indent-offset 2))

(use-package nodejs-repl
  :ensure-system-package node
  :defer t)

(use-package json-mode
  :mode
  ("\\.bowerrc$"     . json-mode)
  ("\\.jshintrc$"    . json-mode)
  ("\\.json_schema$" . json-mode)
  :custom
  (js-indent-level 2))

(use-package graphql-mode)

(use-package coffee-mode
  :ensure-system-package (coffee . "npm i -g coffeescript")
  :mode "\\.coffee\\.*"
  :bind
  (:map coffee-mode-map
        (","          . self-with-space)
        ("="          . pad-equals)
        ("C-c C-c"    . coffee-compile-region))
  :custom
  (coffee-args-repl '("-i" "--nodejs"))
  :config
  (defun coffee-indent ()
    (if (coffee-line-wants-indent)
        (coffee-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
      (coffee-insert-spaces (coffee-previous-indent))))
  (add-λ 'coffee-mode-hook
    (setq-local indent-line-function #'coffee-indent))
  (add-to-list 'coffee-args-compile "--no-header"))

(use-package import-js
  :ensure-system-package
  (importjsd . "npm i -g import-js")
  :hook
  (typescript-mode . run-import-js)
  :bind
  (:map typescript-mode-map
        ("C-s-s" . import-js-fix)))

(use-package swift-mode)

(use-package dotenv-mode
  :mode "\\.env\\..*\\'")

(use-package typescript-mode
  :ensure-system-package
  (typescript-language-server . "npm i -g typescript-language-server")
  :bind
  (:map typescript-mode-map
        ("," . self-with-space)
        ("=" . pad-equals)
        (":" . self-with-space)
        ("C-c C-." . web-mode))
  :custom
  (typescript-indent-level 2))

(use-package ts-comint
  :ensure-system-package (tsun . "npm i -g tsun"))

(use-package ember-mode
  :ensure-system-package (ember . "npm i -g ember-cli"))

(use-package vue-mode)

(use-package mmm-mode
  :custom
  (mmm-submode-decoration-level 0))

(use-package format-all
  :ensure-system-package
  (prettier . "npm i -g prettier")
  (rufo . "gem install rufo"))

(use-package slim-mode
  :bind
  (:map slim-mode-map
        (","          . self-with-space)
        (":"          . smart-ruby-colon)
        ("<C-return>" . slim-newline-dwim))
  :custom
  (slim-backspace-backdents-nesting nil)
  :config
  (def slim-newline-dwim
    (move-end-of-line nil)
    (newline-and-indent))
  (add-λ 'slim-mode-hook (modify-syntax-entry ?\= ".")))

(use-package ruby-mode
  :mode ("Appraisals$" (rx (and (group (= 1 upper) (1+ lower)) (not (any "Proc"))) "file" eos))
  :bind
  (:map ruby-mode-map
        (","          . self-with-space)
        ("="          . pad-equals)
        (":"          . smart-ruby-colon)
        ("<C-return>" . ruby-newline-dwim))
  :ensure-system-package
  (rubocop     . "gem install rubocop")
  (ruby-lint   . "gem install ruby-lint")
  (ripper-tags . "gem install ripper-tags")
  (pry         . "gem install pry")
  (solargraph  . "gem install solargraph")
  :custom
  (ruby-insert-encoding-magic-comment nil)
  :config
  (def smart-ruby-colon
    (if (and (looking-back "[[:word:]]" nil)
             (not (memq font-lock-type-face (list (get-text-property (- (point) 1) 'face)))))
        (insert ": ")
      (insert ":")))
  (def ruby-newline-dwim
    (let ((add-newline (or (eolp)
                           (looking-at "\|$")
                           (looking-at "\)$"))))
      (move-end-of-line nil)
      (smart-newline)
      (insert "end")
      (move-beginning-of-line nil)
      (if add-newline
          (smart-newline)
        (indent-according-to-mode))))
  (defun hippie-expand-ruby-symbols (f &rest args)
    (if (eq major-mode 'ruby-mode)
        (let ((table (make-syntax-table ruby-mode-syntax-table)))
          (modify-syntax-entry ?: "." table)
          (with-syntax-table table (apply f args)))
      (apply f args)))
  (advice-add 'hippie-expand :around #'hippie-expand-ruby-symbols)
  (add-λ 'ruby-mode-hook
    (setq-local projectile-tags-command "ripper-tags -R -f TAGS")))

(use-package ruby-tools
  :after ruby-mode)

(use-package rspec-mode
  :after ruby-mode
  :bind
  ("s-R" . rspec-rerun)
  (:map inf-ruby-mode-map ("s-R" . rspec-rerun))
  :config
  (after yasnippet
    (rspec-install-snippets)))

(use-package inf-ruby
  :config
  (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter t)
  (add-hook 'after-init-hook #'inf-ruby-switch-setup)
  (add-λ 'inf-ruby-mode-hook
    (turn-on-comint-history ".pry_history"))
  (bind-key "M-TAB" #'comint-previous-matching-input-from-input inf-ruby-mode-map)
  (bind-key "<M-S-tab>" #'comint-next-matching-input-from-input inf-ruby-mode-map))

(use-package rake)

(use-package projectile-rails
  :bind
  (:map projectile-rails-mode-map ("C-c r" . hydra-projectile-rails/body))
  :init
  (projectile-rails-global-mode))

(use-package ruby-hash-syntax
  :after ruby-mode
  :bind
  (:map ruby-mode-map ("C-c C-:" . ruby-hash-syntax-toggle)))

(use-package yaml-mode
  :mode "\\.yml\\'"
  :bind
  (:map yaml-mode-map (":" . self-with-space))
  :hook
  (yaml-mode . text-smaller-no-truncation))

(use-package restclient
  :defer t)

(use-feature text-mode
  :bind
  (:map text-mode-map ("," . self-with-space)))

;;;;; Version Control

(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-feature vc-hooks
  :custom
  (vc-follow-symlinks t))

(use-package diff-mode
  :custom
  (diff-font-lock-prettify t))

(use-package magit
  :bind
  ("s-m" . magit-status)
  (:map hemacs-git-map ("l" . magit-clone))
  (:map hemacs-git-map ("f" . magit-diff-buffer-file))
  (:map magit-mode-map ("C-c C-a" . magit-just-amend))
  :custom
  (magit-log-section-commit-count 0)
  (magit-completing-read-function #'ivy-completing-read)
  (magit-log-auto-more t)
  (magit-branch-prefer-remote-upstream t)
  (magit-diff-refine-hunk 'all)
  (magit-no-confirm t)
  :hook
  (magit-process-mode . text-smaller-no-truncation)
  :config
  (def magit-just-amend
    (save-window-excursion
      (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
      (magit-refresh)))
  (after projectile
    (setq magit-repository-directories projectile-known-projects))
  (after alert
    (defun magit-process-alert-after-finish-in-background (f &rest args)
      (let* ((process (nth 0 args))
             (event (nth 1 args))
             (buf (process-get process 'command-buf))
             (buff-name (buffer-name buf)))
        (when (and buff-name (stringp event) (s-match "magit" buff-name) (s-match "finished" event))
          (alert-after-finish-in-background buf (concat (capitalize (process-name process)) " finished")))
        (apply f (list process event))))
    (advice-add 'magit-process-sentinel :around #'magit-process-alert-after-finish-in-background)))

(use-package forge
  :after magit
  :custom
  (forge-topic-list-limit '(60 . 0)))

(use-package magit-todos
  :config
  (magit-todos-mode)
  :custom
  (magit-todos-max-items 30))

(use-package epa
  :bind
  ("C-c g d f" . epa-decrypt-file)
  ("C-c g d r" . epa-decrypt-region)
  ("C-c g e r" . epa-encrypt-region)
  ("C-c g e f" . epa-encrypt-file))

(use-package browse-at-remote
  :bind
  (:map hemacs-git-map ("o" . browse-at-remote)))

(use-package diff-hl
  :hook
  (dired-mode . diff-hl-dired-mode)
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;;;;; Emacs Lisping

(use-package eldoc
  :custom
  (eldoc-idle-delay hemacs-posframe-delay))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  (:map hemacs-help-map ("." . helpful-at-point)))

(use-package elisp-slime-nav
  :hook
  ((emacs-lisp-mode ielm-mode) . turn-on-elisp-slime-nav-mode))

;;;;; Help & Docs

(use-package know-your-http-well)

(use-package define-word
  :bind
  (:map hemacs-help-map ("w" . define-word-at-point)))

(use-package google-this
  :bind
  (:map hemacs-help-map ("g" . google-this)))

(use-package etags
  :custom
  (tags-add-tables nil)
  (tags-revert-without-query t))

(use-package gist
  :defer t
  :bind
  (:map hemacs-git-map ("g" . gist-region-or-buffer-private)))

(use-package gitattributes-mode)

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package dash-at-point
  :if *is-mac*
  :straight
  (:host github :repo "waymondo/dash-at-point")
  :ensure-system-package
  ("/Applications/Dash.app" . "brew cask install dash")
  :bind
  (:map hemacs-help-map
        ("d" . dash-at-point)
        ("D" . dash-at-point-with-docset)))

(use-feature flymake
  :bind
  (:map hemacs-help-map ("f" . flymake-show-diagnostics-buffer))
  :custom
  (flymake-start-syntax-check-on-newline nil))

(use-package list-environment
  :bind
  (:map hemacs-help-map ("l" . list-environment)))

(use-package memory-usage
  :bind
  (:map hemacs-help-map ("m" . memory-usage)))

;;;;; Posframe

(use-package posframe
  :custom
  (posframe-arghandler #'hemacs-posframe-arghandler)
  :config
  (defun hemacs-posframe-arghandler (posframe-buffer arg-name value)
    (let ((info '(:internal-border-width 12 :min-width 80)))
      (or (plist-get info arg-name) value))))

(use-package which-key-posframe
  :config
  (which-key-posframe-mode)
  :custom
  (which-key-posframe-poshandler 'posframe-poshandler-point-bottom-left-corner))

(use-package eldoc-box
  :hook
  (emacs-lisp-mode . eldoc-box-hover-at-point-mode))

(use-package frog-menu
  :custom
  (frog-menu-avy-padding t))

(use-package frog-jump-buffer
  :chords
  (";a" . frog-jump-buffer)
  :custom
  (frog-jump-buffer-posframe-handler #'posframe-poshandler-frame-center)
  (frog-jump-buffer-default-filters-capital-letters t)
  (frog-jump-buffer-filter-actions '(("X" "[special]" frog-jump-buffer-filter-special-buffers)))
  :config
  (dolist (regexp '("TAGS" "-lsp\\*$" "^\\*lsp-" "^\\*straight-process" "^\\magit-" "^\\*Compile-log"
                    "-debug\\*$" "^\\:" "^\\*helpful" "^\\*Async" "errors\\*$" "^\\*Backtrace" "-ls\\*$"
                    "stderr\\*$" "^\\*Flymake" "^\\*direnv" "^\\*vc" "^\\*Warnings" "^\\*eldoc" "\\^*Shell Command"))
    (push regexp frog-jump-buffer-ignore-buffers))
  (defun frog-jump-buffer-filter-special-buffers (buffer)
    (with-current-buffer buffer
      (-any? #'derived-mode-p '(comint-mode magit-mode inf-ruby-mode rg-mode compilation-mode)))))

(use-package flymake-diagnostic-at-point
  :straight
  (:host github :repo "waymondo/flymake-diagnostic-at-point")
  :after
  flymake
  :bind
  ("s-?" . flymake-diagnostic-at-point-maybe-display)
  :custom
  (flymake-diagnostic-at-point-timer-delay hemacs-posframe-delay)
  (flymake-diagnostic-at-point-display-diagnostic-function #'flymake-diagnostic-at-point-display-posframe)
  :hook
  (flymake-mode . flymake-diagnostic-at-point-mode))

;;;;; Language Server

(use-package lsp-mode
  :hook
  ((typescript-mode sgml-mode web-mode html-mode css-mode less-css-mode scss-mode ruby-mode) . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (lsp-solargraph-use-bundler t)
  :config
  (add-λ 'lsp-mode-hook
    (setq-local lsp-headerline-breadcrumb-enable nil))
  (defun lsp-format-buffer-maybe-call-format-all (f &rest args)
    (condition-case err
        (apply f args)
      (error
       (call-interactively 'format-all-buffer))))
  (advice-add 'lsp-format-buffer :around #'lsp-format-buffer-maybe-call-format-all)
  :bind*
  ("C-M-\\" . lsp-format-buffer))

(use-package company-lsp
  :custom
  (company-lsp-cache-candidates 'auto))

;;;;; Appearance

(use-package fira-code-mode
  :custom
  (fira-code-mode-enable-hex-literal nil)
  :config
  (unless (member "Fira Code" (font-family-list))
    (fira-code-mode-install-fonts))
  (global-fira-code-mode)
  (set-face-attribute 'default nil :height (* default-font-size 10)))

(use-package showtip
  :custom
  (showtip-top-adjust default-font-size)
  :config
  (when *is-mac*
    (shell-command
     (concat "defaults write org.gnu.Emacs NSToolTipsFontSize -int "
             (number-to-string default-font-size)))))

(use-package darkroom
  :hook
  (writing-modes . darkroom-tentative-mode))

(use-feature frame
  :custom
  (blink-cursor-blinks 0)
  :init
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (blink-cursor-mode))

(use-feature uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

(use-package beacon
  :custom
  (beacon-blink-when-focused t)
  (beacon-blink-when-window-scrolls nil)
  :config
  (defun maybe-recenter-current-window ()
    (when (and (equal (current-buffer) (window-buffer (selected-window)))
               (not (eq recenter-last-op 'middle)))
      (recenter-top-bottom)))
  (add-hook 'beacon-before-blink-hook #'maybe-recenter-current-window)
  (dolist (mode '(comint-mode term-mode))
    (push mode beacon-dont-blink-major-modes))
  (beacon-mode))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package symbol-overlay
  :custom
  (symbol-overlay-idle-time 0.2)
  :bind
  ("M-n" . symbol-overlay-jump-next)
  ("M-p" . symbol-overlay-jump-prev)
  :hook
  (prog-mode . symbol-overlay-mode))

(use-package highlight-indentation
  :hook
  (indent-sensitive-modes . highlight-indentation-current-column-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :custom
  (rainbow-delimiters-max-face-count 5))

(use-package moody
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :config
  (minions-mode))

(use-package hide-mode-line
  :hook
  ((dired-mode bs-mode helpful-mode magit-mode magit-popup-mode org-capture-mode help-mode) .
   hide-mode-line-mode))

(use-package paren
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode))

(use-package dimmer
  :config
  (dimmer-mode)
  (dimmer-configure-posframe)
  (dimmer-configure-magit)
  (dimmer-configure-hydra)
  (dimmer-configure-which-key))

(use-feature fringe
  :config
  (fringe-mode '(20 . 8))
  :custom
  (fringe-indicator-alist
   (delq (assq 'continuation fringe-indicator-alist) fringe-indicator-alist)))

(use-package solaire-mode
  :after diff-hl
  :hook
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (dolist (face '(diff-hl-insert diff-hl-unknown diff-hl-delete diff-hl-change))
    (add-to-list 'solaire-mode-remap-alist `((,face solaire-default-face) . t)))
  (solaire-global-mode))

(use-package apropospriate-theme
  :custom
  (apropospriate-org-level-resizing nil)
  :config
  (load-theme 'apropospriate-light t t)
  (load-theme 'apropospriate-dark t))

(use-package vscode-icon)

;;;;; Bindings & Chords

(use-package bind-key
  :bind
  (:map hemacs-help-map ("k" . describe-personal-keybindings)))

(use-package ffap
  :commands
  (ffap-file-at-point ffap-url-at-point)
  :bind
  (:map hemacs-help-map ("p" . ffap)))

(use-package key-chord
  :chords
  ("}|" . pad-pipes)
  ("[]" . pad-brackets)
  ("{}" . open-brackets-newline-and-indent)
  ("-=" . insert-arrow)
  ("_+" . insert-fat-arrow)
  ("''" . "’")
  ("^^" . "λ")
  :custom
  (key-chord-two-keys-delay 0.05)
  :config
  (key-chord-mode 1))

(use-package free-keys
  :bind
  (:map hemacs-help-map ("K" . free-keys)))

(use-package which-key
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-mode)
  (dolist (prefix '("projectile-switch-project" "ember" "magit" "projectile"))
    (let ((pattern (concat "^" prefix "-\\(.+\\)")))
      (push `((nil . ,pattern) . (nil . "\\1"))
            which-key-replacement-alist))))
