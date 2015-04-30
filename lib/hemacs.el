;;; hemacs --- an emacs configuration -*- lexical-binding: t; -*-

(defmacro def (name &rest body)
  (declare (indent 1) (debug t))
  `(defun ,name (&optional arg)
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
  `(--each ,modes
     (add-λ (intern (format "%s-hook" it))
       ,@body)))

(defmacro each-mode-map (modes &rest body)
  (declare (indent 1) (debug t))
  `(--each ,modes
     (let ((mode-map (symbol-value (intern (format "%s-map" it)))))
       ,@body)))

(defmacro with-region-or-line (func &optional point-to-eol)
  (declare (indent 1) (debug t))
  `(progn
     (defadvice ,func (before with-region-or-line activate compile)
       (interactive
        (cond (mark-active
               (list (region-beginning) (region-end)))
              (,point-to-eol
               (list (point) (line-end-position)))
              ((list (line-beginning-position) (line-beginning-position 2))))))
     (defadvice ,func (after pulse-line-or-region activate compile)
       (unless mark-active
         (if ,point-to-eol
             (pulse-momentary-highlight-region (point) (line-end-position))
           (pulse-line-hook-function))))))

(defmacro with-region-or-buffer (func)
  (declare (indent 1) (debug t))
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(defmacro make-beautify-defun (type)
  (declare (indent 1) (debug t))
  (let ((defun-name (intern (format "beautify-%s" type))))
    `(progn
       (defun ,defun-name (beg end)
         (interactive "r")
         (shell-command-on-region beg end ,(format "js-beautify --%s -f - -s 2 -m 1" type)
                                  (current-buffer) 'replace))
       (bind-key "s-b" ',defun-name ,(intern (format "%s-mode-map" type))))))

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
       (when (member this-command ,commands)
         (funcall ,func)))))

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
     ((vc-backend filename) (vc-rename-file filename new-name))
     (:else
      (rename-file filename new-name :force-overwrite)
      (set-visited-file-name new-name :no-query :along-with-file)))))

(def open-package
  (let* ((packages (mapcar 'symbol-name (mapcar 'car package-alist)))
         (package (completing-read "Open package: " packages nil t)))
    (find-library package)))

(def delete-file-and-buffer
  (let ((filename (buffer-file-name)))
    (cond
     ((not filename) (kill-buffer))
     ((vc-backend filename) (vc-delete-file filename))
     (:else
      (delete-file filename)
      (kill-buffer)))))

(def hemacs-delete
  (cond
   ((eq major-mode 'dired-mode)
    (dired-do-delete))
   ((or (derived-mode-p 'comint-mode)
        (eq major-mode 'inf-ruby-mode))
    (clear-shell))
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

(def clear-shell
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)
    (goto-char (point-max))))

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
  (when (not (looking-back " "))
    (insert " ")))

(def insert-arrow
  (ensure-space)
  (insert "-> "))

(def insert-fat-arrow
  (ensure-space)
  (insert "=> "))

(def smart-css-comma
  (insert ",")
  (smart-newline)
  (save-excursion
    (insert " ")))

(def smart-css-colon
  (let ((current-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (cond ((string-match "^\\(?:[^[:blank:]]+\\|[[:blank:]]+[[:word:]]*[#&.@,]+\\)" current-line)
           (insert ":"))
          ((looking-at "\;.*")
           (insert ": "))
          (:else
           (insert ": ;")
           (backward-char)))))

(def pad-comma
  (insert ", "))

(def pad-equals
  (cond ((looking-back "=[[:space:]]")
         (delete-char -1))
        ((looking-back "[^#/]")
         (ensure-space)))
  (insert "= "))

(def pad-pipes
  (ensure-space)
  (insert "||")
  (backward-char))

(def smart-ruby-colon
  (when (looking-back ":[[:space:]]")
    (delete-char -1))
  (if (looking-back "[[:word:]]")
      (insert ": ")
    (insert ":")))

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
  (cond ((member major-mode '(js-mode coffee-mode))
         (insert "console.log()")
         (backward-char))
        ((member major-mode lispy-modes)
         (insert "alert "))
        ((eq major-mode 'ruby-mode)
         (insert "Rails.logger.info "))
        (:else
         (insert "log"))))

(def find-user-init-file-other-window
  (find-file-other-window user-init-file))

(defun hemacs-writing-hook ()
  (visual-line-mode)
  (flyspell-mode)
  (key-chord-define-local (kbd "SPC SPC") (λ (insert ". "))))

(defun hemacs-save-hook ()
  (unless (member major-mode '(markdown-mode gfm-mode sql-mode))
    (delete-trailing-whitespace))
  (when (region-active-p)
    (deactivate-mark t))
  (when (fboundp 'mc/keyboard-quit)
    (mc/keyboard-quit)))

(defun hemacs-shellish-hook ()
  (setq truncate-lines nil)
  (set (make-local-variable 'scroll-margin) 0)
  (text-scale-decrease 1))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun try-expand-dabbrev-matching-buffers (old)
  (let ((matching-buffers (--filter
                           (eq major-mode (with-current-buffer it major-mode))
                           (buffer-list))))
    (noflet ((buffer-list () matching-buffers))
      (try-expand-dabbrev-all-buffers old))))

(defun try-expand-dabbrev-other-buffers (old)
  (let ((matching-buffers (--reject
                           (eq major-mode (with-current-buffer it major-mode))
                           (buffer-list))))
    (noflet ((buffer-list () matching-buffers))
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

(--each '(yank yank-pop clipboard-yank)
  (eval `(defadvice ,it (after indent-region activate compile)
           (and (not current-prefix-arg)
                (not (member major-mode indent-sensitive-modes))
                (or (-any? 'derived-mode-p progish-modes))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(defun eval-after-init (form)
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(defun turn-on-comint-history (history-file)
  (setq comint-input-ring-file-name history-file)
  (comint-read-input-ring 'silent))

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

(def recentf-ido-find-file-other-window
  (let ((file (ido-completing-read "Choose recent file: "
                                   (-map 'abbreviate-file-name recentf-list)
                                   nil t)))
    (when file
      (find-file-other-window file))))

(defun pop-to-process-list-buffer ()
  (pop-to-buffer "*Process List*"))

(defun backward-delete-subword (orig-fun &rest args)
  (noflet ((kill-region (beg end) (delete-region beg end)))
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
  (noflet ((process-list ()))
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

(defun package-install-never-select (orig-fun &rest args)
  (apply orig-fun (list (nth 0 args) t)))

(def update-packages
  (package-refresh-contents)
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (package-menu-execute t)))

(defun projectile-relevant-known-git-projects ()
  (mapcar
   (lambda (dir)
     (substring dir 0 -1))
   (cl-remove-if-not
    (lambda (project)
      (unless (file-remote-p project)
        (file-directory-p (concat project "/.git/"))))
    (projectile-relevant-known-projects))))

(provide 'hemacs)
