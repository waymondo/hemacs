;;; hemacs --- an emacs configuration

(defmacro make-projectile-switch-project-defun (func)
  (declare (indent 1) (debug t))
  (let ((defun-name (intern (format "projectile-switch-project-%s" (symbol-name func)))))
    `(defun ,defun-name ()
       (interactive)
       (let ((projectile-switch-project-action ',func))
         (call-interactively 'projectile-switch-project)))))

(defmacro make-transform-symbol-at-point-defun (func)
  (declare (indent 1) (debug t))
  (let ((defun-name (intern (format "%s-symbol-at-point" (symbol-name func)))))
    `(progn
       (defun ,defun-name ()
         (interactive)
         (save-excursion
           (er/mark-symbol)
           (let ((current-symbol (buffer-substring-no-properties (region-beginning) (region-end))))
             (call-interactively 'delete-region)
             (insert (funcall ',func current-symbol))))))))

(defmacro funcall-after-commands (func commands)
  (declare (indent 1) (debug t))
  `(progn
     (add-λ 'post-command-hook
       (when (member this-original-command ,commands)
         (run-at-time nil nil ,func)))))

(def browse-file-directory
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))

(def duplicate-dwim
  (let (beg end (origin (point)))
    (if (and (region-active-p) (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if (region-active-p)
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(def rename-file-and-buffer
  (let* ((filename (buffer-file-name))
         (old-name (if filename
                       (file-name-nondirectory filename)
                     (buffer-name)))
         (new-name (read-file-name "New name: " nil nil nil old-name)))
    (cond
     ((not (and filename (file-exists-p filename))) (rename-buffer new-name))
     (:else
      (rename-file filename new-name :force-overwrite)
      (set-visited-file-name new-name :no-query :along-with-file)))))

(def open-package
  (let* ((packages (mapcar 'symbol-name (mapcar 'car package-alist)))
         (package (completing-read "Open package: " packages nil t)))
    (find-library package)))

(def delete-file-and-buffer
  (let ((filename (buffer-file-name)))
    (when filename
      (system-move-file-to-trash filename))
    (kill-buffer)))

(def hemacs-delete
  (cond
   ((eq major-mode 'dired-mode)
    (dired-do-delete))
   ((or (derived-mode-p 'comint-mode)
        (eq major-mode 'inf-ruby-mode))
    (comint-clear-buffer))
   (:else
    (delete-file-and-buffer))))

(def eol-then-newline
  (move-end-of-line nil)
  (cond ((eq major-mode 'coffee-mode)
         (coffee-newline-and-indent))
        (t (newline-and-indent))))

(def html-smarter-newline
  (move-end-of-line nil)
  (smart-newline)
  (sgml-close-tag)
  (move-beginning-of-line nil)
  (smart-newline))

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

(def slim-newline-dwim
  (move-end-of-line nil)
  (newline)
  (indent-according-to-mode))

(def coffee-smarter-newline
  (move-end-of-line nil)
  (insert-arrow)
  (coffee-newline-and-indent))

(def move-text-up
  (transpose-lines 1)
  (forward-line -2)
  (unless (member major-mode indent-sensitive-modes)
    (indent-according-to-mode)))

(def move-text-down
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (unless (member major-mode indent-sensitive-modes)
    (indent-according-to-mode)))

(def shift-right
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) tab-width))))

(def shift-left
  (shift-right (* -1 (or arg 1))))

(def google
  (browse-url
   (concat
    "http://www.google.com/search?q="
    (if (region-active-p)
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google Search: "
                   (when (symbol-at-point) (symbol-name (symbol-at-point))))))))

(def insert-local-ip-address
  (insert (s-chomp (shell-command-to-string "resolveip -s $HOSTNAME"))))

(def magit-just-amend
  (save-window-excursion
    (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
    (magit-refresh)))

(def kill-symbol-at-point
  (er/mark-symbol)
  (kill-region (point) (mark)))

(def delete-symbol-at-point
  (er/mark-symbol)
  (call-interactively 'delete-region))

(defun ensure-space ()
  (unless (looking-back " ")
    (insert " ")))

(def insert-arrow
  (ensure-space)
  (insert "-> "))

(def insert-fat-arrow
  (ensure-space)
  (insert "=> "))

(def smart-ruby-colon
  (if (looking-back "[[:word:]]")
      (insert ": ")
    (insert ":")))

(def smart-css-colon
  (let ((current-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (cond ((string-match "^\\(?:[^[:blank:]]+\\|[[:blank:]]+[[:word:]]*[#&.@,]+\\)" current-line)
           (insert ":"))
          ((looking-at "\;.*")
           (insert ": "))
          (:else
           (insert ": ;")
           (backward-char)))))

(def smart-js-colon
  (insert ":")
  (ensure-space)
  (insert ",")
  (backward-char))

(def pad-comma
  (insert ",")
  (ensure-space))

(def pad-equals
  (if (nth 3 (syntax-ppss))
      (insert "=")
    (cond ((looking-back "=[[:space:]]")
           (delete-char -1))
          ((looking-back "[^#/|!]")
           (ensure-space)))
    (insert "=")
    (ensure-space)))

(def pad-pipes
  (ensure-space)
  (insert "||")
  (backward-char))

(def open-brackets-newline-and-indent
  (ensure-space)
  (insert "{\n\n}")
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(def pad-brackets
  (ensure-space)
  (insert "{  }")
  (backward-char 2))

(def hemacs-todo
  (insert "- [ ] "))

(def log-statement
  (cond ((member major-mode '(js-mode js2-mode))
         (insert "console.log()")
         (backward-char))
        ((eq major-mode 'coffee-mode)
         (insert "console.log "))
        ((eq major-mode 'ruby-mode)
         (insert "ap "))))

(def find-user-init-file-other-window
  (find-file-other-window user-init-file))

(defun hemacs-save-hook ()
  (unless (member major-mode '(markdown-mode gfm-mode sql-mode))
    (delete-trailing-whitespace))
  (when (region-active-p)
    (deactivate-mark t))
  (when (fboundp 'mc/keyboard-quit)
    (mc/keyboard-quit)))

(defun process-output-scrolling ()
  (setq truncate-lines nil)
  (set (make-local-variable 'scroll-margin) 0))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun try-expand-dabbrev-matching-buffers (old)
  (let ((matching-buffers (--filter
                           (eq major-mode (with-current-buffer it major-mode))
                           (buffer-list))))
    (flet ((buffer-list () matching-buffers))
      (try-expand-dabbrev-all-buffers old))))

(defun try-expand-dabbrev-other-buffers (old)
  (let ((matching-buffers (--reject
                           (eq major-mode (with-current-buffer it major-mode))
                           (buffer-list))))
    (flet ((buffer-list () matching-buffers))
      (try-expand-dabbrev-all-buffers old))))

(def create-scratch-buffer
  (let ((current-major-mode major-mode)
        (buf (generate-new-buffer "*scratch*")))
    (switch-to-buffer buf)
    (funcall current-major-mode)))

(def find-or-create-projectile-restclient-buffer
  (let ((buffer-name (concat "*" (projectile-project-name) " restclient*")))
    (switch-to-buffer
     (or (get-buffer buffer-name)
         (generate-new-buffer buffer-name)))
    (restclient-mode)))

(def toggle-split-window
  (if (eq last-command 'toggle-split-window)
      (progn
        (jump-to-register :toggle-split-window)
        (setq this-command 'toggle-unsplit-window))
    (window-configuration-to-register :toggle-split-window)
    (switch-to-buffer-other-window nil)))

(defun maybe-indent-afterwards (&optional _)
  (and (not current-prefix-arg)
       (not (member major-mode indent-sensitive-modes))
       (or (-any? 'derived-mode-p progish-modes))
       (let ((mark-even-if-inactive transient-mark-mode))
         (indent-region (region-beginning) (region-end) nil))))

(defun turn-on-comint-history (history-file)
  (setq comint-input-ring-file-name history-file)
  (comint-read-input-ring 'silent))

(defun improve-npm-process-output (output)
  (replace-regexp-in-string "\\[[0-9]+[GK]" "" output))

(defun show-image-dimensions-in-mode-line ()
  (let* ((image-dimensions (image-size (image-get-display-property) :pixels))
         (width (car image-dimensions))
         (height (cdr image-dimensions)))
    (setq mode-line-buffer-identification
          (format "%s %dx%d" (propertized-buffer-identification "%12b") width height))))

(defun hemacs-imenu-elisp-expressions ()
  (--each '(("packages" "^\\s-*(\\(use-package\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)
            (nil "^(def \\(.+\\)$" 1)
            ("sections" "^;;;;; \\(.+\\)$" 1))
    (add-to-list 'imenu-generic-expression it)))

(defun css-imenu-generic-expression ()
  (setq imenu-generic-expression '((nil "^\\([^\s-].*+\\(?:,\n.*\\)*\\)\\s-{$" 1))))

(def describe-thing-in-popup
  (let* ((thing (symbol-at-point))
         (help-xref-following t)
         (description (save-window-excursion
                        (with-temp-buffer
                          (help-mode)
                          (help-xref-interned thing)
                          (buffer-string)))))
    (popup-tip description
               :point (point)
               :around t
               :height 20
               :scroll-bar t
               :margin t)))

(defun hemacs-kill-buffer-query ()
  (if (not (member (buffer-name) '("*Messages*")))
      t
    (bury-buffer)
    nil))

(defun alert-after-finish-in-background (buf str)
  (unless (get-buffer-window buf 'visible)
    (alert str :buffer buf)))

(def toggle-transparency
  (if (member (frame-parameter nil 'alpha) '(nil 100))
      (set-frame-parameter nil 'alpha 67)
    (set-frame-parameter nil 'alpha 100)))

(def company-kill-ring
  (company-begin-with
   (mapcar #'substring-no-properties kill-ring))
  (company-filter-candidates))

(def company-only-emoji
  (insert ":")
  (run-at-time nil nil
               (lambda () (company-begin-backend 'company-emoji))))

(def recentf-find-file
  (let ((file (completing-read "Choose recent file: "
                               (-map 'abbreviate-file-name recentf-list)
                               nil t)))
    (when file
      (find-file file))))

(defun pop-to-process-list-buffer ()
  (pop-to-buffer "*Process List*"))

(defun backward-delete-subword (orig-fun &rest args)
  (flet ((kill-region (beg end) (delete-region beg end)))
    (apply orig-fun args)))

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

(defun find-file-maybe-make-directories (filename &optional wildcards)
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir :make-parents)))))

(defun save-buffers-kill-emacs-no-process-query (orig-fun &rest args)
  (flet ((process-list ()))
    (apply orig-fun args)))

(defun hippie-expand-case-sensitive (orig-fun &rest args)
  (let ((case-fold-search nil))
    (apply orig-fun args)))

(defun magit-process-alert-after-finish-in-background (orig-fun &rest args)
  (let* ((process (nth 0 args))
         (event (nth 1 args))
         (buf (process-get process 'command-buf))
         (buff-name (buffer-name buf)))
    (when (and buff-name (stringp event) (s-match "magit" buff-name) (s-match "finished" event))
      (alert-after-finish-in-background buf (concat (capitalize (process-name process)) " finished")))
    (apply orig-fun (list process event))))

(defun projectile-relevant-known-git-projects ()
  (mapcar
   (lambda (dir)
     (substring dir 0 -1))
   (cl-remove-if-not
    (lambda (project)
      (unless (file-remote-p project)
        (file-directory-p (concat project "/.git/"))))
    (projectile-relevant-known-projects))))

(defun hippie-expand-ruby-symbols (orig-fun &rest args)
  (if (eq major-mode 'ruby-mode)
      (let ((table (make-syntax-table ruby-mode-syntax-table)))
        (modify-syntax-entry ?: "." table)
        (with-syntax-table table (apply orig-fun args)))
    (apply orig-fun args)))

(defun with-region-or-point-to-eol (beg end &optional _)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (point) (line-end-position)))))

(defun with-region-or-line (beg end &optional _)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(defun with-region-or-buffer (beg end &optional _)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (point-min) (point-max)))))

(defun js2-log-arguments ()
  (interactive)
  (save-excursion
    (when (and (beginning-of-defun) (search-forward "function") (search-forward "("))
      (let ((args (buffer-substring-no-properties
                   (point)
                   (progn (backward-char 1) (forward-sexp 1) (1- (point))))))
        (search-forward "{")
        (insert "\nconsole.log({"
                (mapconcat (lambda (arg) (format "%s: %s" (s-trim arg) (s-trim arg)))
                           (split-string args ", " t) ", ")
                "});")
        (call-interactively 'indent-for-tab-command)))))

(def upgrade-packages
  (package-refresh-contents)
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (condition-case nil
        (package-menu-execute t)
      (error
       (package-menu-execute)))))

(defun refresh-themed-packages ()
  (when (fboundp 'powerline-reset)
    (powerline-reset))
  (when (fboundp 'highlight-tail-reload)
    (highlight-tail-reload)))

(defun refresh-themed-packages-when-idle (&optional no-confirm no-enable)
  (run-with-idle-timer 1 nil #'refresh-themed-packages))

(provide 'hemacs)
