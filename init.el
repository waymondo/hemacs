;;; hemacs --- an emacs configuration -*- lexical-binding: t; flycheck-disabled-checkers: (emacs-lisp-checkdoc); -*-

;;;;; Source Variables

(setq load-prefer-newer t
      history-length 256
      history-delete-duplicates t
      scroll-conservatively most-positive-fixnum
      scroll-preserve-screen-position 'always
      auto-window-vscroll nil
      echo-keystrokes 0.02
      ns-use-native-fullscreen nil
      ns-use-srgb-colorspace t
      delete-by-moving-to-trash t
      ring-bell-function #'ignore
      ns-function-modifier 'control
      ns-right-option-modifier 'none
      create-lockfiles nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      gc-cons-threshold (* 10 1024 1024)
      disabled-command-function nil
      ad-redefinition-action 'accept
      custom-safe-themes t
      custom-file (make-temp-file "emacs-custom")
      inhibit-startup-screen t
      initial-scratch-message nil
      inhibit-startup-echo-area-message ""
      standard-indent 2
      enable-recursive-minibuffers t
      kill-buffer-query-functions nil)

(setq-default indent-tabs-mode nil
              line-spacing 1
              tab-width 2
              c-basic-offset 2
              cursor-type 'bar
              cursor-in-non-selected-windows nil
              bidi-display-reordering nil
              truncate-lines t)

(defalias 'yes-or-no-p #'y-or-n-p)
(prefer-coding-system 'utf-8)

;;;;; Personal Variables & Helper Macros

(defvar indent-sensitive-modes
  '(coffee-mode slim-mode))
(defvar progish-modes
  '(prog-mode css-mode sgml-mode))
(defvar lispy-modes
  '(emacs-lisp-mode ielm-mode eval-expression-minibuffer-setup))
(defvar ruby-modes
  '(ruby-mode slim-mode inf-ruby-mode))
(defvar writing-modes
  '(org-mode markdown-mode fountain-mode git-commit-mode))

(defmacro def (name &rest body)
  (declare (indent 1) (debug t))
  `(defun ,name (&optional _arg)
     ,(if (stringp (car body)) (car body))
     (interactive "p")
     ,@(if (stringp (car body)) (cdr `,body) body)))

(defmacro λ (&rest body)
  (declare (indent 1) (debug t))
  `(lambda ()
     (interactive)
     ,@body))

(defmacro add-λ (hook &rest body)
  (declare (indent 1) (debug t))
  `(add-hook ,hook (lambda () ,@body)))

(defmacro hook-modes (modes &rest body)
  (declare (indent 1) (debug t))
  `(dolist (mode ,modes)
     (add-λ (intern (format "%s-hook" mode)) ,@body)))

;;;;; Package Management

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (setq use-package-enable-imenu-support t)
  (require 'use-package))

(def upgrade-packages
  (package-refresh-contents)
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (condition-case nil
        (package-menu-execute t)
      (error
       (package-menu-execute)))))

(defmacro use-my-package (pkg &rest plist)
  (declare (indent 1) (debug t))
  (let* ((checkout-dir (format "lib/%s" (symbol-name pkg)))
         (ensure-or-load-path (if (file-directory-p checkout-dir)
                                  `(:load-path ,checkout-dir)
                                '(:ensure t))))
    `(use-package ,pkg ,@ensure-or-load-path ,@plist)))

(use-my-package use-package-chords)

(use-package no-littering
  :ensure t)

;;;;; Bootstrap

(defun ensure-space ()
  (unless (looking-back " " nil)
    (insert " ")))

(def self-with-space
  (call-interactively #'self-insert-command)
  (ensure-space))

(def pad-equals
  (if (nth 3 (syntax-ppss))
      (insert "=")
    (cond ((looking-back "=[[:space:]]" nil)
           (delete-char -1))
          ((looking-back "[^#/|!<>+]" nil)
           (ensure-space)))
    (call-interactively #'self-insert-command)
    (ensure-space)))

(def open-brackets-newline-and-indent
  (let ((inhibit-message t)
        (text
         (when (region-active-p)
           (buffer-substring-no-properties (region-beginning) (region-end)))))
    (when (region-active-p)
      (delete-region (region-beginning) (region-end)))
    (unless (looking-back (rx (or "(" "[")) nil)
      (ensure-space))
    (insert (concat "{\n" text "\n}"))
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))

(def pad-brackets
  (unless (looking-back (rx (or "(" "[")) nil)
    (ensure-space))
  (insert "{  }")
  (backward-char 2))

(add-λ 'after-init-hook
  (when (member "Fira Code" (font-family-list))
    (set-frame-font "Fira Code Retina-15")
    (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                   (35 . ".\\(?:###\\|##\\|[#(?[_{]\\)")
                   (36 . ".\\(?:>\\)")
                   (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                   (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                   (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                   (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                   (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                   (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                   (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                   (48 . ".\\(?:x[a-zA-Z]\\)")
                   (58 . ".\\(?:::\\|[:=]\\)")
                   (59 . ".\\(?:;;\\|;\\)")
                   (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                   (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                   (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                   (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                   (91 . ".\\(?:]\\)")
                   (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                   (94 . ".\\(?:=\\)")
                   (119 . ".\\(?:ww\\)")
                   (123 . ".\\(?:-\\)")
                   (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                   (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                   )
                 ))
      (dolist (char-regexp alist)
        (set-char-table-range composition-function-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))))
(add-λ 'minibuffer-setup-hook
  (setq-local input-method-function nil)
  (setq-local gc-cons-threshold most-positive-fixnum))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package dash
  :ensure t
  :config (dash-enable-font-lock))

(use-package s
  :ensure t
  :bind ("s-;" . transform-symbol-at-point)
  :config
  (def transform-symbol-at-point
    (let* ((choices '((?c . s-lower-camel-case)
                      (?C . s-upper-camel-case)
                      (?_ . s-snake-case)
                      (?- . s-dashed-words)
                      (?d . s-downcase)
                      (?u . s-upcase)))
           (chars (mapcar #'car choices))
           (prompt (concat "Transform symbol at point [" chars "]: "))
           (ch (read-char-choice prompt chars))
           (fn (assoc-default ch choices))
           (symbol (thing-at-point 'symbol t))
           (bounds (bounds-of-thing-at-point 'symbol)))
      (when fn
        (delete-region (car bounds) (cdr bounds))
        (insert (funcall fn symbol))))))

(use-package tool-bar
  :defer t
  :config (tool-bar-mode -1))

(use-package scroll-bar
  :defer t
  :config (scroll-bar-mode -1))

(use-package menu-bar
  :bind ("s-w" . kill-this-buffer))

(use-package mb-depth
  :defer t
  :config (minibuffer-depth-indicate-mode))

;;;;; Processes, Shells, Compilation

(use-package exec-path-from-shell
  :ensure t
  :config
  (push "HISTFILE" exec-path-from-shell-variables)
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package comint
  :bind
  (:map comint-mode-map
        ("RET"       . comint-return-dwim)
        ("s-k"       . comint-clear-buffer)
        ("M-TAB"     . comint-previous-matching-input-from-input)
        ("<M-S-tab>" . comint-next-matching-input-from-input))
  :config
  (setq comint-prompt-read-only t)
  (setq-default comint-process-echoes t
                comint-input-ignoredups t
                comint-scroll-show-maximum-output nil
                comint-output-filter-functions
                '(ansi-color-process-output
                  comint-truncate-buffer
                  comint-watch-for-password-prompt))
  (defun turn-on-comint-history (history-file)
    (setq comint-input-ring-file-name history-file)
    (comint-read-input-ring 'silent))
  (defun process-shellish-output ()
    (setq truncate-lines nil)
    (text-scale-decrease 1))
  (def comint-return-dwim
    (cond
     ((comint-after-pmark-p)
      (comint-send-input))
     ((ffap-guess-file-name-at-point)
      (ffap))
     (t
      (comint-next-prompt 1))))
  (defun improve-npm-process-output (output)
    (replace-regexp-in-string "\\[[0-9]+[GK]" "" output))
  (add-to-list 'comint-preoutput-filter-functions #'improve-npm-process-output)
  (add-hook 'kill-buffer-hook #'comint-write-input-ring)
  (add-hook 'comint-mode-hook #'process-shellish-output)
  (add-λ 'kill-emacs-hook
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer (comint-write-input-ring)))))

(use-package compile
  :defer t
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil)
  (add-hook 'compilation-mode-hook #'process-shellish-output)
  (add-hook 'compilation-finish-functions #'alert-after-finish-in-background))

(use-package warnings
  :config
  (setq warning-suppress-types '((undo discard-info))))

(use-package shell
  :defer t
  :config
  (setq explicit-bash-args '("-c" "export INSIDE_EMACS=; stty echo; bash"))
  (defun make-shell-command-behave-interactively (orig-fun &rest args)
    (let ((shell-command-switch "-ic"))
      (apply orig-fun args)))
  (advice-add 'shell-command :around #'make-shell-command-behave-interactively)
  (advice-add 'start-process-shell-command :around #'make-shell-command-behave-interactively)
  (add-λ 'shell-mode-hook
    (turn-on-comint-history (getenv "HISTFILE"))))

(use-package sh-script
  :mode (("\\.*bashrc$" . sh-mode)
         ("\\.*bash_profile" . sh-mode))
  :config
  (setq-default sh-indentation 2
                sh-basic-offset 2))

(use-package executable
  :defer t
  :config
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p))

(use-package repl-toggle
  :ensure t
  :config
  (repl-toggle-mode)
  (setq rtog/mode-repl-alist
        '((emacs-lisp-mode . ielm)
          (ruby-mode . inf-ruby)
          (js2-mode . nodejs-repl))))

;;;;; Files & History

(use-package image-mode
  :defer t
  :config
  (defun show-image-dimensions-in-mode-line ()
    (let* ((image-dimensions (image-size (image-get-display-property) :pixels))
           (width (car image-dimensions))
           (height (cdr image-dimensions)))
      (setq mode-line-buffer-identification
            (format "%s %dx%d" (propertized-buffer-identification "%12b") width height))))
  (add-hook 'image-mode-hook #'show-image-dimensions-in-mode-line))

(use-package files
  :config
  (defun hemacs-save-hook ()
    (unless (member major-mode '(markdown-mode gfm-mode sql-mode))
      (delete-trailing-whitespace))
    (when (region-active-p)
      (deactivate-mark t)))
  (defun find-file-maybe-make-directories (filename &optional _wildcards)
    (unless (file-exists-p filename)
      (let ((dir (file-name-directory filename)))
        (unless (file-exists-p dir)
          (make-directory dir :make-parents)))))
  (defun save-buffers-kill-emacs-no-process-query (orig-fun &rest args)
    (cl-letf (((symbol-function 'process-list) #'ignore))
      (apply orig-fun args)))
  (add-hook 'before-save-hook #'hemacs-save-hook)
  (advice-add 'find-file :before #'find-file-maybe-make-directories)
  (advice-add 'save-buffers-kill-emacs :around #'save-buffers-kill-emacs-no-process-query)
  (setq require-final-newline t
        confirm-kill-emacs nil
        confirm-nonexistent-file-or-buffer nil
        backup-directory-alist `((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))

(use-package autorevert
  :config
  (global-auto-revert-mode)
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t))

(use-package savehist
  :config
  (savehist-mode)
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring comint-input-ring)
        savehist-autosave-interval 30))

(use-package saveplace
  :init (save-place-mode))

(use-package recentf
  :config
  (recentf-mode)
  (setq recentf-exclude '(".ido.last")
        recentf-max-saved-items 1000))

(use-package dired
  :defer t
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-use-ls-dired nil
        dired-dwim-target t
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-auto-revert-buffer t))

(use-package dired-x
  :bind ("s-\\" . dired-jump-other-window)
  :after dired)

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode)
  :bind (("s-z" . undo-tree-undo)
         ("s-Z" . undo-tree-redo)))

(use-package osx-trash
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))

(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
  :ensure t
  :bind ("C-c f" . reveal-in-osx-finder))

;;;;; Editing

(use-package newcomment
  :bind ("s-/" . comment-or-uncomment-region))

(use-package simple
  :bind ("s-k" . kill-whole-line)
  :config
  (hook-modes writing-modes
    (auto-fill-mode)
    (visual-line-mode))
  (defun pop-to-mark-command-until-new-point (orig-fun &rest args)
    (let ((p (point)))
      (dotimes (_i 10)
        (when (= p (point))
          (apply orig-fun args)))))
  (defun maybe-indent-afterwards (&optional _)
    (and (not current-prefix-arg)
         (not (member major-mode indent-sensitive-modes))
         (or (-any? #'derived-mode-p progish-modes))
         (indent-region (region-beginning) (region-end) nil)))
  (defun pop-to-process-list-buffer ()
    (pop-to-buffer "*Process List*"))
  (defun kill-line-or-join-line (orig-fun &rest args)
    (if (not (eolp))
        (apply orig-fun args)
      (forward-line)
      (join-line)))
  (defun move-beginning-of-line-or-indentation (orig-fun &rest args)
    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (apply orig-fun args))))
  (defun backward-delete-subword (orig-fun &rest args)
    (cl-letf (((symbol-function 'kill-region) #'delete-region))
      (apply orig-fun args)))
  (advice-add 'pop-to-mark-command :around #'pop-to-mark-command-until-new-point)
  (advice-add 'yank :after #'maybe-indent-afterwards)
  (advice-add 'yank-pop :after #'maybe-indent-afterwards)
  (advice-add 'list-processes :after #'pop-to-process-list-buffer)
  (advice-add 'backward-kill-word :around #'backward-delete-subword)
  (advice-add 'kill-whole-line :after #'back-to-indentation)
  (advice-add 'kill-line :around #'kill-line-or-join-line)
  (advice-add 'move-beginning-of-line :around #'move-beginning-of-line-or-indentation)
  (setq set-mark-command-repeat-pop t
        next-error-recenter t
        async-shell-command-buffer 'new-buffer)
  (bind-keys
   :map minibuffer-local-map
   ("<escape>"  . abort-recursive-edit)
   ("M-TAB"     . previous-complete-history-element)
   ("<M-S-tab>" . next-complete-history-element)))

(use-package delsel
  :init (delete-selection-mode))

(use-package elec-pair
  :init (electric-pair-mode))

(use-package electric
  :init (electric-quote-mode)
  :config (setq electric-quote-string t))

(use-package subword
  :init (global-subword-mode))

(use-package expand-region
  :ensure t
  :bind* ("C-," . er/expand-region))

(use-package change-inner
  :ensure t
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package avy
  :ensure t
  :bind
  (:map dired-mode-map ("." . avy-goto-word-or-subword-1))
  :chords
  (("jj" . avy-goto-char-timer)
   ("jk" . avy-goto-word-or-subword-1)
   ("jl" . avy-goto-line))
  :config
  (avy-setup-default))

(use-package ace-link
  :ensure t
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

(use-my-package ace-jump-zap
  :chords ("jz" . ace-jump-zap-up-to-char))

(use-package ace-window
  :ensure t
  :bind (([remap next-multiframe-window] . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package smart-newline
  :ensure t
  :bind ("<s-return>" . eol-then-smart-newline)
  :init
  (defun smart-newline-no-reindent-first (orig-fun &rest args)
    (cl-letf (((symbol-function 'reindent-then-newline-and-indent) #'newline-and-indent))
      (apply orig-fun args)))
  (hook-modes progish-modes
    (when (not (member major-mode indent-sensitive-modes))
      (smart-newline-mode))
    (advice-add 'smart-newline :around #'smart-newline-no-reindent-first))
  (def eol-then-smart-newline
    (move-end-of-line nil)
    (smart-newline)))

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp]      . easy-mark)))

(use-package shift-number
  :ensure t
  :bind (("<M-up>"   . shift-number-up)
         ("<M-down>" . shift-number-down)))

(use-package multiple-cursors
  :ensure t
  :bind (("s-d"     . mc/mark-next-like-this)
         ("s-D"     . mc/mark-previous-like-this)
         ("C-c s-d" . mc/mark-all-like-this-dwim))
  :config
  (add-hook 'before-save-hook #'mc/keyboard-quit))

(use-package crux
  :ensure t
  :bind
  (("s-," . crux-find-user-init-file)
   ("s-D" . crux-duplicate-current-line-or-region)
   ("s-K" . crux-delete-file-and-buffer)
   ("s-S" . crux-rename-file-and-buffer)
   ("C-;" . crux-ispell-word-then-abbrev))
  :chords
  (":S" . crux-recentf-ido-find-file)
  :config
  (defun crux-ignore-vc-backend (orig-fun &rest args)
    (cl-letf (((symbol-function 'vc-backend) #'ignore))
      (apply orig-fun args)))
  (advice-add 'crux-rename-file-and-buffer :around #'crux-ignore-vc-backend)
  (advice-add 'crux-delete-file-and-buffer :around #'crux-ignore-vc-backend)
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-point-to-eol kill-ring-save))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package toggle-quotes
  :ensure t
  :bind ("C-'" . toggle-quotes))

(use-package scratch
  :ensure t
  :bind ("s-N" . scratch))

(use-package flyspell
  :config
  (hook-modes writing-modes
    (flyspell-mode)))

(use-package smart-shift
  :ensure t
  :bind (("s-[" . smart-shift-left)
         ("s-]" . smart-shift-right))
  :config
  (advice-add 'smart-shift-override-local-map :override #'ignore))

(use-package drag-stuff
  :ensure t
  :bind (("<C-s-down>" . drag-stuff-down)
         ("<C-s-up>"   . drag-stuff-up))
  :config
  (defun indent-unless-sensitive (_arg)
    (unless (member major-mode indent-sensitive-modes)
      (indent-according-to-mode)))
  (advice-add 'drag-stuff-line-vertically :after #'indent-unless-sensitive)
  (advice-add 'drag-stuff-lines-vertically :after #'indent-unless-sensitive))

;;;;; Completion

(use-package ido
  :bind
  (:map ido-common-completion-map
        ("M-TAB"     . previous-history-element)
        ("<M-S-tab>" . next-history-element))
  :config
  (setq ido-cannot-complete-command 'exit-minibuffer
        ido-use-virtual-buffers t
        ido-max-prospects 9
        ido-auto-merge-delay-time 2
        ido-create-new-buffer 'always))

(use-package flx-ido
  :ensure t
  :after ido
  :config
  (flx-ido-mode)
  (setq flx-ido-use-faces nil))

(use-package ivy
  :ensure t
  :bind
  (:map ivy-minibuffer-map
        ("<escape>"  . abort-recursive-edit))
  :chords
  (";s" . ivy-switch-buffer)
  :init
  (ivy-mode)
  (setq ivy-fixed-height-minibuffer t
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-use-virtual-buffers t))

(use-package hippie-exp
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (defun try-expand-dabbrev-matching-buffers (old)
    (let ((hippie-expand-only-buffers `(,major-mode)))
      (try-expand-dabbrev-all-buffers old)))
  (defun try-expand-dabbrev-other-buffers (old)
    (let ((hippie-expand-ignore-buffers `(,major-mode)))
      (try-expand-dabbrev-all-buffers old)))
  (defun hippie-expand-case-sensitive (orig-fun &rest args)
    (let ((case-fold-search nil))
      (apply orig-fun args)))
  (defun hippie-expand-inhibit-message-in-minibuffer (orig-fun &rest args)
    (let ((inhibit-message (minibufferp)))
      (apply orig-fun args)))
  (advice-add 'hippie-expand :around #'hippie-expand-case-sensitive)
  (advice-add 'hippie-expand :around #'hippie-expand-inhibit-message-in-minibuffer)
  (bind-key "TAB" #'hippie-expand read-expression-map)
  (bind-key "TAB" #'hippie-expand minibuffer-local-map)
  (bind-key* "M-?" (make-hippie-expand-function '(try-expand-line-all-buffers)))
  (setq hippie-expand-verbose nil
        hippie-expand-try-functions-list '(try-expand-dabbrev-visible
                                           try-expand-dabbrev
                                           try-expand-dabbrev-matching-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-dabbrev-other-buffers))
  (hook-modes lispy-modes
    (setq-local hippie-expand-try-functions-list
                (append '(try-complete-lisp-symbol-partially
                          try-complete-lisp-symbol)
                        hippie-expand-try-functions-list))))

(use-package yasnippet
  :ensure t
  :mode ("\\.yasnippet\\'" . snippet-mode)
  :init
  (yas-global-mode)
  (defun yas-indent-unless-case-sensitive (orig-fun &rest args)
    (let ((yas-indent-line (if (member major-mode indent-sensitive-modes) nil 'auto)))
      (apply orig-fun args)))
  (advice-add 'yas--indent :around #'yas-indent-unless-case-sensitive)
  (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand))

(use-package company
  :ensure t
  :bind ("s-y" . company-kill-ring)
  :init
  (def company-kill-ring
    (company-begin-with
     (mapcar #'substring-no-properties kill-ring))
    (company-filter-candidates))
  (global-company-mode)
  (setq company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-require-match nil
        company-minimum-prefix-length 2
        company-show-numbers t
        company-occurrence-weight-function #'company-occurrence-prefer-any-closest
        company-continue-commands
        (append company-continue-commands '(comint-previous-matching-input-from-input
                                            comint-next-matching-input-from-input))))

(use-package company-dabbrev
  :after company
  :config
  (setq company-dabbrev-minimum-length 2))

(use-package company-dabbrev-code
  :after company
  :config
  (setq company-dabbrev-code-modes t
        company-dabbrev-code-everywhere t))

(use-package company-emoji
  :ensure t
  :after company
  :config
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
  (hook-modes writing-modes
    (setq-local company-backends (append '(company-emoji) company-backends))))

(use-package company-shell
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends #'company-shell))

(use-package company-tern
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends #'company-tern))

(use-package company-web
  :ensure t
  :after company
  :config
  (with-eval-after-load 'web-mode
    (add-λ 'web-mode-hook
      (setq-local company-backends (append '(company-web-html) company-backends))))
  (with-eval-after-load 'html-mode
    (add-λ 'html-mode-hook
      (setq-local company-backends (append '(company-web-html) company-backends))))
  (with-eval-after-load 'slim-mode
    (add-λ 'slim-mode-hook
      (setq-local company-backends (append '(company-web-slim) company-backends))))
  (with-eval-after-load 'jade-mode
    (add-λ 'jade-mode-hook
      (setq-local company-backends (append '(company-web-jade) company-backends)))))

(use-package smart-tab
  :ensure t
  :config
  (global-smart-tab-mode)
  (setq smart-tab-using-hippie-expand t
        smart-tab-completion-functions-alist '()))

;;;;; Navigation & Search

(use-package window
  :preface (provide 'window)
  :chords ((";w" . toggle-split-window)
           (":W" . delete-other-windows)
           (":Q" . delete-side-windows))
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
        (delete-window window))))
  (setq display-buffer-alist
        `((,(rx bos (or "*Flycheck errors*"
                        "*Backtrace"
                        "*Warnings"
                        "*compilation"
                        "*Help"
                        "*less-css-compilation"
                        "*Packages"
                        "*magit-process"
                        "*SQL"
                        "*tldr"))
           (display-buffer-reuse-window
            display-buffer-in-side-window)
           (side            . bottom)
           (reusable-frames . visible)
           (window-height   . 0.33))
          ("." nil (reusable-frames . visible)))))

(use-package wgrep-ag
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package ag
  :ensure t
  :chords ((":G" . ag))
  :config
  (setq ag-reuse-buffers t
        ag-highlight-search t))

(use-package bm
  :ensure t
  :bind
  (("s-1" . bm-toggle)
   ("s-2" . bm-next)
   ("s-@" . bm-previous))
  :config
  (setq bm-cycle-all-buffers t))

(use-package anzu
  :ensure t
  :bind
  (([remap query-replace] . anzu-query-replace)
   ("s-q" . anzu-query-replace)
   ("C-q" . anzu-query-replace-at-cursor-thing))
  :config
  (global-anzu-mode))

(use-package imenu
  :config
  (defun hemacs-imenu-elisp-expressions ()
    (dolist (pattern '((nil "^(def \\(.+\\)$" 1)
                       ("sections" "^;;;;; \\(.+\\)$" 1)))
      (add-to-list 'imenu-generic-expression pattern)))
  (add-hook 'emacs-lisp-mode-hook #'hemacs-imenu-elisp-expressions)
  (setq imenu-auto-rescan t))

(use-package imenu-anywhere
  :ensure t
  :chords (";r" . ivy-imenu-anywhere))

(use-my-package ace-jump-buffer
  :bind ("s-\"" . ace-jump-buffer)
  :chords
  ((";a" . ace-jump-buffer)
   (":A" . ace-jump-buffer-other-window)
   (";x" . ace-jump-special-buffers))
  :config
  (make-ace-jump-buffer-function "special"
    (with-current-buffer buffer
      (and (not (derived-mode-p 'comint-mode))
           (not (derived-mode-p 'magit-mode))))))

(use-package projectile
  :ensure t
  :bind
  (("s-p" . projectile-command-map)
   ("C-x m" . projectile-run-shell))
  :chords
  ((";t" . projectile-find-file)
   (";g" . projectile-ag))
  :config
  (setq projectile-enable-caching t
        projectile-verbose nil
        projectile-completion-system 'ivy)
  (put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
  (defmacro make-projectile-switch-project-defun (func)
    `(function
      (lambda ()
        (interactive)
        (let ((projectile-switch-project-action ,func))
          (projectile-switch-project)))))
  (defun projectile-relevant-known-git-projects ()
    (mapcar
     (lambda (dir)
       (substring dir 0 -1))
     (cl-remove-if-not
      (lambda (project)
        (unless (file-remote-p project)
          (file-directory-p (concat project "/.git/"))))
      (projectile-relevant-known-projects))))
  (ivy-set-actions
   'projectile-find-file
   '(("j"
      ivy--switch-buffer-other-window-action
      "other window")))
  (projectile-global-mode)
  (projectile-cleanup-known-projects))

(use-my-package projector
  :after projectile
  :bind
  (("C-x RET"        . projector-run-shell-command-project-root)
   ("C-x <C-return>" . projector-run-default-shell-command)
   :map comint-mode-map ("s-R" . projector-rerun-buffer-process))
  :config
  (setq projector-completion-system 'ivy
        projector-always-background-regex
        '("^powder restart\\.*"
          "^heroku restart\\.*"
          "^heroku addons:open\\.*"
          "^spring stop"
          "^gulp publish\\.*"
          "^git push\\.*"
          "^pkill\\.*")
        projector-command-modes-alist
        '(("^heroku run console" . inf-ruby-mode))))

(use-package swiper
  :ensure t
  :bind
  (([remap isearch-forward]  . swiper)
   ([remap isearch-backward] . swiper)))

(use-package counsel
  :ensure t
  :bind
  (([remap execute-extended-command] . counsel-M-x)
   ("s-P" . counsel-M-x))
  :chords
  (";f" . counsel-find-file))

;;;;; External Utilities

(use-package edit-server
  :ensure t
  :config
  (edit-server-start)
  (setq edit-server-new-frame nil
        edit-server-default-major-mode 'gfm-mode))

(use-package crab
  :ensure t
  :defer 2
  :bind (("s-R" . crab-reload)
         ("<S-s-left>" . crab-prev-tab)
         ("<S-s-right>" . crab-next-tab))
  :config
  (crab-server-start))

(use-package restart-emacs
  :ensure t
  :bind ([remap save-buffers-kill-terminal] . restart-emacs))

;;;;; Major Modes

(use-package org
  :bind
  (:map org-mode-map
        ("," . self-with-space)
        ("C-c t" . timestamp))
  :config
  (def timestamp
    (insert (format-time-string "%m/%d/%Y")))
  (setq org-support-shift-select t
        org-completion-use-ido t
        org-startup-indented t))

(use-package org-autolist
  :ensure t
  :after org
  :config (add-hook 'org-mode-hook #'org-autolist-mode))

(use-my-package org-repo-todo
  :after org
  :bind (("s-`" . ort/goto-todos)
         ("s-n" . ort/capture-checkitem)))

(use-package sgml-mode
  :mode
  (("\\.hbs\\'"        . html-mode)
   ("\\.handlebars\\'" . html-mode))
  :bind
  (:map html-mode-map
        ("," . self-with-space)
        ("<C-return>" . html-newline-dwim))
  :chords
  ("<>" . sgml-close-tag)
  :config
  (modify-syntax-entry ?= "." html-mode-syntax-table)
  (modify-syntax-entry ?\' "\"'" html-mode-syntax-table)
  (add-hook 'sgml-mode #'sgml-electric-tag-pair-mode)
  (def html-newline-dwim
    (move-end-of-line nil)
    (smart-newline)
    (sgml-close-tag)
    (move-beginning-of-line nil))
  (bind-key "'" "’" html-mode-map (eq 0 (car (syntax-ppss)))))

(use-package handlebars-sgml-mode
  :ensure t
  :after sgml-mode
  :config (handlebars-use-mode 'global))

(use-package web-mode
  :ensure t
  :mode (("\\.erb\\'" . web-mode)
         ("\\.php\\'" . web-mode))
  :bind
  (:map web-mode-map
        ("," . self-with-space)
        ("<C-return>" . html-newline-dwim)))

(use-package fountain-mode
  :ensure t
  :mode "\\.fountain$")

(use-package markdown-mode
  :ensure t
  :mode
  (("\\.md\\'" . gfm-mode)
   ("\\.markdown\\'" . gfm-mode))
  :bind
  (:map markdown-mode-map ("," . self-with-space))
  :config
  (bind-key "'" "’" markdown-mode-map
            (not (or (markdown-code-at-point-p)
                     (memq 'markdown-pre-face
                           (face-at-point nil 'mult)))))
  (setq markdown-command "marked"
        markdown-indent-on-enter nil))

(use-package pandoc-mode
  :ensure t
  :after (markdown-mode org-mode)
  :config
  (add-hook 'markdown-mode-hook #'pandoc-mode)
  (add-hook 'org-mode-hook #'pandoc-mode))

(use-package markdown-preview-mode
  :ensure t
  :after markdown-mode
  :bind
  (:map markdown-mode-map ("C-x p" . markdown-preview-mode)))

(use-package css-mode
  :mode "\\.css\\.erb\\'"
  :bind
  (:map css-mode-map
        (":" . smart-css-colon)
        ("," . self-with-space)
        ("{" . open-brackets-newline-and-indent))
  :config
  (def smart-css-colon
    (let ((current-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (cond ((string-match "^\\(?:[^[:blank:]]+\\|[[:blank:]]+[[:word:]]*[#&.@,]+\\)" current-line)
             (insert ":"))
            ((looking-at "\;.*")
             (insert ": "))
            (:else
             (insert ": ;")
             (backward-char)))))
  (defun set-css-imenu-generic-expression ()
    (setq imenu-generic-expression '((nil "^\\([^\s-].*+\\(?:,\n.*\\)*\\)\\s-{$" 1))))
  (add-hook 'css-mode-hook #'set-css-imenu-generic-expression)
  (setq css-indent-offset 2))

(use-package less-css-mode
  :ensure t
  :mode "\\.less\\.erb\\'"
  :init (setq less-css-lessc-options '("--no-color" "-x")))

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'"  . js2-mode)
         ("\\.jsx\\'" . js2-mode))
  :bind
  (:map js2-mode-map
        ("," . self-with-space)
        ("=" . pad-equals)
        (":" . self-with-space))
  :chords
  :interpreter (("node" . js2-mode))
  :config
  (setenv "NODE_NO_READLINE" "1")
  (setq js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil
        js2-highlight-level 3
        js2-basic-offset 2)
  (setq-default js2-global-externs
                '("clearTimeout" "setTimeout" "module" "require" "_")))

(use-package xref-js2
  :ensure t
  :after js2-mode
  :bind (:map js2-mode-map ("M-." . nil))
  :config
  (add-λ 'js2-mode-hook
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(use-package json-mode
  :ensure t
  :mode (("\\.bowerrc$"     . json-mode)
         ("\\.jshintrc$"    . json-mode)
         ("\\.json_schema$" . json-mode))
  :config (setq js-indent-level 2))

(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\\.*"
  :bind
  (:map coffee-mode-map
        (","          . self-with-space)
        ("="          . pad-equals)
        ("<C-return>" . coffee-newline-dwim)
        ("C-c C-c"    . coffee-compile-region))
  :config
  (def coffee-newline-dwim
    (move-end-of-line nil)
    (ensure-space)
    (insert "=> ")
    (coffee-newline-and-indent))
  (setq coffee-args-repl '("-i" "--nodejs"))
  (add-to-list 'coffee-args-compile "--no-header"))

(use-package ember-mode
  :ensure t)

(use-package npm-mode
  :ensure t
  :config (npm-global-mode))

(use-package jade-mode
  :ensure t)

(use-package web-beautify
  :ensure t
  :config
  (with-eval-after-load 'js2-mode
    (bind-key "s-b" #'web-beautify-js js2-mode-map))
  (with-eval-after-load 'json-mode
    (bind-key "s-b" #'web-beautify-js json-mode-map))
  (with-eval-after-load 'sgml-mode
    (bind-key "s-b" #'web-beautify-html html-mode-map))
  (with-eval-after-load 'css-mode
    (bind-key "s-b" #'web-beautify-css css-mode-map)))

(use-package elm-mode
  :ensure t)

(use-package slim-mode
  :ensure t
  :bind
  (:map slim-mode-map
        (","          . self-with-space)
        (":"          . smart-ruby-colon)
        ("<C-return>" . slim-newline-dwim))
  :config
  (def slim-newline-dwim
    (move-end-of-line nil)
    (newline-and-indent))
  (setq slim-backspace-backdents-nesting nil)
  (add-λ 'slim-mode-hook (modify-syntax-entry ?\= ".")))

(use-package ruby-mode
  :mode
  (("Appraisals$"   . ruby-mode)
   ("\\.rabl\\'"    . ruby-mode)
   ("\\.builder\\'" . ruby-mode))
  :bind
  (:map ruby-mode-map
        (","          . self-with-space)
        ("="          . pad-equals)
        (":"          . smart-ruby-colon)
        ("<C-return>" . ruby-newline-dwim))
  :config
  (def smart-ruby-colon
    (if (looking-back "[[:word:]]" nil)
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
  (defun hippie-expand-ruby-symbols (orig-fun &rest args)
    (if (eq major-mode 'ruby-mode)
        (let ((table (make-syntax-table ruby-mode-syntax-table)))
          (modify-syntax-entry ?: "." table)
          (with-syntax-table table (apply orig-fun args)))
      (apply orig-fun args)))
  (advice-add 'hippie-expand :around #'hippie-expand-ruby-symbols)
  (add-λ 'ruby-mode-hook
    (setq-local projectile-tags-command "ripper-tags -R -f TAGS")))

(use-package ruby-tools
  :ensure t
  :after ruby-mode)

(use-package rspec-mode
  :ensure t
  :after yasnippet
  :config (rspec-install-snippets))

(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter)
  (add-hook 'after-init-hook #'inf-ruby-switch-setup)
  (add-λ 'inf-ruby-mode-hook
    (turn-on-comint-history ".pry_history"))
  (bind-key "M-TAB" #'comint-previous-matching-input-from-input inf-ruby-mode-map)
  (bind-key "<M-S-tab>" #'comint-next-matching-input-from-input inf-ruby-mode-map))

(use-package projectile-rails
  :ensure t
  :config
  (add-hook 'projectile-mode-hook #'projectile-rails-on))

(use-package chruby
  :ensure t
  :after projectile-rails
  :config
  (add-hook 'projectile-after-switch-project-hook #'chruby-use-corresponding)
  (defun run-ruby-with-corresponding-chruby (orig-fun &rest args)
    (call-interactively #'chruby-use-corresponding)
    (apply orig-fun args))
  (advice-add 'run-ruby :around #'run-ruby-with-corresponding-chruby)
  (bind-key "V" #'chruby-use-corresponding projectile-rails-command-map))

(use-package ruby-hash-syntax
  :ensure t
  :after ruby-mode
  :init
  (bind-key "C-c C-:" #'ruby-toggle-hash-syntax ruby-mode-map))

(use-package yaml-mode
  :ensure t)

(use-package restclient
  :ensure t
  :defer t)

(use-package text-mode
  :preface (provide 'text-mode)
  :bind (:map text-mode-map ("," . self-with-space)))

(use-package mmm-mode
  :ensure t
  :config
  (setq mmm-global-mode 'maybe
        mmm-submode-decoration-level 2
        mmm-parse-when-idle t)
  (mmm-add-classes
   '((jsx
      :submode web-mode
      :front "\\((\\)[[:space:]\n]*<"
      :front-match 1
      :back ">[[:space:]\n]*\\()\\)\n"
      :back-match 1)))
  (mmm-add-mode-ext-class 'js2-mode "\\.jsx\\'" 'jsx))

;;;;; Version Control

(use-package ediff
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package magit
  :ensure t
  :bind
  (("s-m" . magit-status)
   :map magit-mode-map ("C-c C-a" . magit-just-amend))
  :after alert
  :config
  (def magit-just-amend
    (save-window-excursion
      (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
      (magit-refresh)))
  (defun magit-process-alert-after-finish-in-background (orig-fun &rest args)
    (let* ((process (nth 0 args))
           (event (nth 1 args))
           (buf (process-get process 'command-buf))
           (buff-name (buffer-name buf)))
      (when (and buff-name (stringp event) (s-match "magit" buff-name) (s-match "finished" event))
        (alert-after-finish-in-background buf (concat (capitalize (process-name process)) " finished")))
      (apply orig-fun (list process event))))
  (advice-add 'magit-process-sentinel :around #'magit-process-alert-after-finish-in-background)
  (add-hook 'magit-process-mode-hook #'process-shellish-output)
  (setq git-commit-summary-max-length git-commit-fill-column
        magit-completing-read-function 'ivy-completing-read
        magit-log-auto-more t
        magit-repository-directories (funcall #'projectile-relevant-known-git-projects)
        magit-no-confirm t))

(use-package magit-gh-pulls
  :ensure t
  :after magit
  :config (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(use-package git-messenger
  :ensure t
  :config (setq git-messenger:show-detail t))

(use-package vc-git
  :config (setq vc-git-diff-switches '("--histogram")))

(use-package diff-hl
  :ensure t
  :after magit
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh t))

;;;;; Help & Docs

(use-package google-this
  :ensure t)

(use-package find-func
  :config
  (find-function-setup-keys))

(use-package etags
  :init (setq tags-revert-without-query t))

(use-package elisp-slime-nav
  :ensure t
  :config
  (hook-modes lispy-modes
    (elisp-slime-nav-mode)))

(use-package github-browse-file
  :ensure t
  :config
  (defun github-issues (&optional new)
    (interactive)
    (let ((url (concat "https://github.com/"
                       (github-browse-file--relative-url)
                       "/issues/" new)))
      (browse-url url)))
  (def github-new-issue
    (github-issues "new")))

(use-package git-timemachine
  :ensure t)

(use-package gist
  :ensure t
  :defer t)

(use-package gitattributes-mode
  :ensure t)

(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-my-package dash-at-point)

(use-package tldr
  :ensure t)

(use-package discover
  :ensure t
  :config (global-discover-mode))

(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers '(html-tidy javascript-jshint)
                flycheck-idle-change-delay 1
                flycheck-less-executable "/usr/local/bin/lessc")
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;;;; Appearance

(use-package ns-win
  :defer t
  :config (setq ns-pop-up-frames nil))

(use-package frame
  :init
  (setq blink-cursor-blinks 0)
  (add-to-list 'initial-frame-alist '(fullscreen . fullboth)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package page-break-lines
  :ensure t
  :config (global-page-break-lines-mode))

(use-package beacon
  :ensure t
  :config
  (beacon-mode)
  (setq beacon-blink-when-focused t
        beacon-blink-when-point-moves-vertically 4)
  (push 'comint-mode beacon-dont-blink-major-modes))

(use-package highlight-symbol
  :ensure t
  :config
  (hook-modes progish-modes
    (highlight-symbol-mode)
    (highlight-symbol-nav-mode))
  (setq highlight-symbol-idle-delay 0
        highlight-symbol-highlight-single-occurrence nil))

(use-package volatile-highlights
  :ensure t
  :config (volatile-highlights-mode))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'css-mode-hook #'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (hook-modes progish-modes
    (rainbow-delimiters-mode)))

(use-package powerline
  :defer t
  :config (setq powerline-default-separator 'utf-8))

(use-package spaceline
  :ensure t
  :preface (require 'spaceline-config)
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off))

(use-package mode-icons
  :ensure t
  :config
  (mode-icons-mode)
  (setq mode-icons-desaturate-active t))

(use-package paren
  :config
  (show-paren-mode)
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package alert
  :ensure t
  :config
  (defun alert-after-finish-in-background (buf str)
    (unless (get-buffer-window buf 'visible)
      (alert str :buffer buf)))
  (setq alert-default-style 'notifier))

(use-package auto-dim-other-buffers
  :ensure t
  :config (auto-dim-other-buffers-mode))

(use-package centered-cursor-mode
  :ensure t
  :config
  (hook-modes progish-modes
    (centered-cursor-mode)))

(use-package fringe
  :defer t
  :config (fringe-mode '(20 . 8)))

(use-my-package apropospriate-theme
  :config
  (load-theme 'apropospriate-light t t)
  (load-theme 'apropospriate-dark t t))

(use-package highlight-tail
  :ensure t
  :disabled t
  :config
  (setq highlight-tail-steps 16)
  (highlight-tail-mode))

(use-package cycle-themes
  :ensure t
  :config
  (setq cycle-themes-theme-list '(apropospriate-dark apropospriate-light))
  (add-hook 'cycle-themes-after-cycle-hook #'highlight-tail-reload)
  (cycle-themes-mode))

;;;;; Bindings & Chords

(use-package key-chord
  :chords
  (("}|" . pad-pipes)
   ("[]" . pad-brackets)
   ("{}" . open-brackets-newline-and-indent)
   ("-=" . insert-arrow)
   ("_+" . insert-fat-arrow)
   ("^^" . "λ")
   ("::" . "::"))
  :config
  (def insert-arrow
    (ensure-space)
    (insert "->")
    (ensure-space))
  (def insert-fat-arrow
    (ensure-space)
    (insert "=>")
    (ensure-space))
  (def pad-pipes
    (ensure-space)
    (insert "||")
    (backward-char))
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.05))

(use-package free-keys
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(bind-keys
 :prefix-map hemacs-help-map
 :prefix "s-h"
 ("k" . describe-personal-keybindings)
 ("K" . free-keys)
 ("m" . discover-my-major)
 ("g" . google-this)
 ("d" . dash-at-point)
 ("D" . dash-at-point-with-docset)
 ("i" . insert-local-ip-address)
 ("t" . tldr)
 ("o" . counsel-load-library))

(bind-keys
 :prefix-map switch-project-map
 :prefix "s-o"
 ("t"          . projectile-switch-project)
 ("<C-return>" . projector-switch-project-run-default-shell-command))

(bind-key "m" (make-projectile-switch-project-defun #'projectile-run-shell) switch-project-map)
(bind-key "M" (make-projectile-switch-project-defun #'projector-run-shell-command-project-root) switch-project-map)
(bind-key "g" (make-projectile-switch-project-defun #'projectile-vc) switch-project-map)
(bind-key "u" (make-projectile-switch-project-defun #'projectile-run-project) switch-project-map)
(bind-key "f" (make-projectile-switch-project-defun #'projectile-find-file) switch-project-map)
(bind-key "`" (make-projectile-switch-project-defun #'ort/goto-todos) switch-project-map)
(bind-key "n" (make-projectile-switch-project-defun #'ort/capture-checkitem) switch-project-map)

(bind-keys
 :prefix-map hemacs-git-map
 :prefix "s-g"
 ("o" . github-browse-file)
 ("b" . github-browse-file-blame)
 ("c" . github-browse-commit)
 ("l" . magit-clone)
 ("i" . github-new-issue)
 ("I" . github-issues)
 ("g" . gist-region-or-buffer-private)
 ("t" . git-timemachine)
 ("p" . git-messenger:popup-message))
