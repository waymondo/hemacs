(defmacro defn (name &rest body)
  (declare (indent 1))
  `(defun ,name (&optional arg)
     ,(if (stringp (car body)) (car body))
     (interactive "p")
     ,@(if (stringp (car body)) (cdr `,body) body)))

(defmacro λ (&rest body)
  (declare (indent 1))
  `(lambda ()
     (interactive)
     ,@body))

(defmacro add-λ (hook &rest body)
  (declare (indent 1))
  `(add-hook ,hook (lambda () ,@body)))

(defmacro hook-modes (modes &rest body)
  (declare (indent 1))
  `(--each ,modes
     (add-λ (intern (format "%s-hook" it))
       ,@body)))

(defmacro with-region-or-line (func)
  `(defadvice ,func (before with-region-or-line activate compile)
     (interactive
      (if mark-active (list (region-beginning) (region-end))
        (list (line-beginning-position) (line-beginning-position 2))))))

(defun dwim-at-point ()
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

(defn open-finder
  (shell-command (concat "open " (shell-quote-argument default-directory))))

(defn duplicate-dwim
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

(defn subword-backward-delete
  (let ((beg (point)))
    (subword-backward arg)
    (delete-region (point) beg)))

(defn rename-file-and-buffer
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

(defn delete-file-and-buffer
  (let ((filename (buffer-file-name)))
    (cond
     ((not filename) (kill-buffer))
     ((vc-backend filename) (vc-delete-file filename))
     (:else
      (delete-file filename)
      (kill-buffer)))))

(defn back-to-indentation-or-beginning
  (if (or (looking-back "^\s*")
          (eq last-command 'back-to-indentation-or-beginning))
      (beginning-of-line)
    (back-to-indentation)))

(defn kill-region-and-god-local-mode
  (when (region-active-p) (call-interactively 'kill-region))
  (god-local-mode -1))

(defn eol-then-newline
  (move-end-of-line nil)
  (cond ((eq major-mode 'coffee-mode)
         (coffee-newline-and-indent))
        (t (newline-and-indent))))

(defn html-smarter-newline
  (move-end-of-line nil)
  (smart-newline)
  (sgml-close-tag)
  (move-beginning-of-line nil)
  (smart-newline))

(defn coffee-smarter-newline
  (move-end-of-line nil)
  (insert " ->")
  (coffee-newline-and-indent))

(defn move-line-up
  (transpose-lines 1)
  (forward-line -2)
  (unless (member major-mode indent-sensitive-modes)
    (indent-according-to-mode)))

(defn move-line-down
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (unless (member major-mode indent-sensitive-modes)
    (indent-according-to-mode)))

(defn toggle-split-window
  (if (eq last-command 'toggle-split-window)
      (progn
        (jump-to-register :toggle-split-window)
        (setq this-command 'toggle-unsplit-window))
    (window-configuration-to-register :toggle-split-window)
    (switch-to-buffer-other-window nil)))

(defn tab-dwim
  (if (minibufferp)
      (hippie-expand nil)
    (if mark-active
        (indent-region (region-beginning) (region-end))
      (if (looking-at "\\_>")
          (hippie-expand nil)
        (indent-for-tab-command)))))

(defn ido-go-home
  (cond
   ((looking-back "~/") (insert "code/"))
   ((looking-back "/") (insert "~/"))
   (:else (call-interactively 'self-insert-command))))

(defn shift-right
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) tab-width))))

(defn shift-left
  (shift-right (* -1 (or arg 1))))

(defn sudo-edit
  (find-alternate-file (concat "/sudo::" (buffer-file-name))))

(defn byte-compile-current-buffer
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(defn google-dwim
  (browse-url
   (concat
    "http://www.google.com/search?q="
    (if (region-active-p)
        (buffer-substring (region-beginning) (region-end))
      (read-string "Query: " (dwim-at-point))))))

(defn projectile-switch-project-vc
  (let ((projectile-switch-project-action 'projectile-vc))
    (call-interactively 'projectile-switch-project)))

(defn clear-shell
  (let ((old-max comint-buffer-maximum-size))
    (setq comint-buffer-maximum-size 0)
    (comint-truncate-buffer)
    (setq comint-buffer-maximum-size old-max)
    (goto-char (point-max))))

(defn magit-kill-file-on-line
  (magit-visit-item)
  (delete-file-and-buffer)
  (magit-refresh))

(defn magit-just-amend
  (save-window-excursion
    (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
    (magit-refresh)))

(defun magit-pull-request-for-issue-number (prompt)
  (interactive "sIssue number for pull request: ")
  (save-window-excursion
    (shell-command (concat "hub pull-request -i " prompt))
    (magit-refresh)))

(defn insert-fat-arrow
  (insert " => "))

(defn insert-arrow
  (insert " -> "))

(defn open-brackets-newline-and-indent
  (insert " {\n\n}")
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defn pad-brackets
  (insert "{  }")
  (backward-char 2))

(defn pad-colon
  (if (not (member major-mode '(css-mode less-css-mode)))
      (insert ": ")
    (if (looking-at "\;.*")
          (insert ": ")
        (insert ": ;")
        (backward-char))))

(defn log-statement
  (cond ((member major-mode '(js-mode coffee-mode))
         (insert "console.log()")
         (backward-char))
        ((member major-mode '(ruby-mode))
         (insert "Rails.logger.info "))))

(defn find-user-init-file-other-window
  (find-file-other-window user-init-file))

(defun hemacs-writing-hook ()
  (visual-line-mode)
  (flyspell-mode))

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

(defadvice eval-region (after maybe-deactivate-mark activate compile)
  (if (region-active-p) (deactivate-mark t)))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate compile)
  (noflet ((process-list ())) ad-do-it))

(defn create-scratch-buffer
  (let ((current-major-mode major-mode)
        (buf (generate-new-buffer "*scratch*")))
    (switch-to-buffer buf)
    (funcall current-major-mode)))

(--each '(yank yank-pop clipboard-yank)
  (eval `(defadvice ,it (after indent-region activate compile)
           (and (not current-prefix-arg)
                (not (member major-mode indent-sensitive-modes))
                (or (-any? 'derived-mode-p progish-modes))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(defun set-clean-margins (&optional arg)
  "Set window-body-width to ARG or 90 with relative margins."
  (interactive "P")
  (let* ((width (if arg (prefix-numeric-value arg) 90))
         (margin (/ (- (window-total-width) width) 2)))
    (set-window-margins (selected-window) margin margin)))

(defun eval-after-init (form)
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(defun replace-region-or-symbol-at-point-with (fn)
  (unless (use-region-p) (er/mark-symbol))
  (let* ((beg (region-beginning))
         (end (region-end))
         (contents (buffer-substring beg end)))
    (delete-region beg end)
    (insert (funcall fn contents))))

(defun incs (s &optional num)
  (let* ((inc (or num 1))
         (new-number (number-to-string (+ inc (string-to-number s))))
         (zero-padded? (s-starts-with? "0" s)))
    (if zero-padded?
        (s-pad-left (length s) "0" new-number)
      new-number)))

(defun increment-number-at-point (arg)
  (interactive "p")
  (unless (or (looking-at "[0-9]")
              (looking-back "[0-9]"))
    (error "No number to change at point"))
  (save-excursion
    (while (looking-back "[0-9]")
      (forward-char -1))
    (re-search-forward "[0-9]+" nil)
    (replace-match (incs (match-string 0) arg) nil nil)))

(defun decrement-number-at-point (arg)
  (interactive "p")
  (increment-number-at-point (- arg)))

(defun recompile-elc-on-save ()
  (add-hook
   'after-save-hook
   (lambda ()
     (when (string-prefix-p user-emacs-directory (file-truename buffer-file-name))
       (emacs-lisp-byte-compile)))
   nil
   t))

(defun beautify-css (beg end)
  (interactive "r")
  (shell-command-on-region beg end "js-beautify --css -f - -s 2 --no-preserve-newlines" (current-buffer) 'replace))

(defun turn-on-comint-history (history-file)
  (setq comint-input-ring-file-name history-file)
  (comint-read-input-ring 'silent))

(defvar ace-jump-zapping nil)

(add-λ 'ace-jump-mode-end-hook
  (unless (not ace-jump-zapping)
    (call-interactively 'delete-region)
    (deactivate-mark))
  (setq ace-jump-zapping nil))

(defn ace-jump-zap-up-to-char
  (setq ace-jump-zapping t)
  (call-interactively 'set-mark-command)
  (call-interactively 'ace-jump-char-mode)
  (define-key overriding-local-map [t]
    (λ (setq ace-jump-zapping nil))))
