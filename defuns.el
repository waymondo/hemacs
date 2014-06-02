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

(defn goto-line-with-feedback
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defn subword-backward-delete
  (let ((beg (point)))
    (subword-backward arg)
    (delete-region (point) beg)))

(defn rename-current-file-or-buffer
  (if (not (buffer-file-name))
      (call-interactively 'rename-buffer)
    (let ((file (buffer-file-name)))
      (with-temp-buffer
        (set-buffer (dired-noselect file))
        (dired-do-rename)
        (kill-buffer nil))))
  nil)

(defn delete-current-buffer-file
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defn back-to-indentation-or-beginning
  (if (or (looking-back "^\s*")
          (eq last-command 'back-to-indentation-or-beginning))
      (beginning-of-line)
    (back-to-indentation)))

(defn kill-region-and-god-local-mode
  (when (region-active-p) (call-interactively 'kill-region))
  (god-local-mode -1))

(defn smart-open-line
  (move-end-of-line nil)
  (cond ((eq major-mode 'coffee-mode) (coffee-newline-and-indent))
        (t (newline-and-indent))))

(defn newline-dwim
  (let ((break-open-pair (or (and (looking-back "{" 1) (looking-at "}"))
                             (and (looking-back ">" 1) (looking-at "<"))
                             (and (looking-back "(" 1) (looking-at ")"))
                             (and (looking-back "\\[" 1) (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
    (indent-for-tab-command)))

(defn smart-open-line-above
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defn move-line-up
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defn move-line-down
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defn toggle-split-window
  (if (eq last-command 'toggle-split-window)
      (progn
        (jump-to-register :toggle-split-window)
        (setq this-command 'toggle-unsplit-window))
    (window-configuration-to-register :toggle-split-window)
    (switch-to-buffer-other-window nil)))

(defn tab-dwim
  (interactive)
  (if (minibufferp)
      (hippie-expand nil)
    (if mark-active
        (indent-region (region-beginning) (region-end))
      (if (looking-at "\\_>")
          (hippie-expand nil)
        (indent-for-tab-command)))))

(defn google-search
  (browse-url
   (concat "http://www.google.com/search?q=" (dwim-at-point))))

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

(defn eval-region-and-maybe-deactivate-mark
  (eval-region (region-beginning)
               (region-end))
  (if (region-active-p) (deactivate-mark t))
  (minibuffer-message "Lisp evaluated"))

(defn byte-compile-current-buffer
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(defn ruby-smart-newline-end-defun
  (let ((whites
         (save-excursion
           (back-to-indentation)
           (current-column))))
    (end-of-line)
    (save-excursion
      (newline)
      (indent-line-to (+ whites ruby-indent-level))
      (newline)
      (indent-line-to whites)
      (insert "end"))
    (next-line)))

(defn projectile-switch-project-vc
  (let ((projectile-switch-project-action 'projectile-vc))
    (call-interactively 'projectile-switch-project)))

(defn clear-shell
  (let ((old-max comint-buffer-maximum-size))
    (setq comint-buffer-maximum-size 0)
    (comint-truncate-buffer)
    (setq comint-buffer-maximum-size old-max)
    (end-of-buffer)))

(defn magit-kill-file-on-line
  (interactive)
  (magit-visit-item)
  (delete-current-buffer-file)
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
  (previous-line)
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
    (flet ((buffer-list () matching-buffers))
      (try-expand-dabbrev-all-buffers old))))

(defun try-expand-dabbrev-other-buffers (old)
  (let ((matching-buffers (--remove
                           (eq major-mode (with-current-buffer it major-mode))
                           (buffer-list))))
    (flet ((buffer-list () matching-buffers))
      (try-expand-dabbrev-all-buffers old))))

(defadvice kill-line (before check-position activate)
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1))))

(defadvice hippie-expand (around hippie-expand-case-fold activate)
  (let ((case-fold-search nil))
    ad-do-it))

(defun space-chord-define-global (key command)
  (define-key (current-global-map)
    (vector 'key-chord ? (if (stringp key) (aref key 0) key)) command))

(defvar yankee-do-modes '(css-mode less-css-mode sgml-mode))
(defvar yankee-no-modes '(coffee-mode slim-mode c-mode objc-mode))

(dolist (command '(yank yank-pop clipboard-yank))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (not (member major-mode yankee-no-modes))
                (or (derived-mode-p 'prog-mode)
                    (member major-mode yankee-do-modes))
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

(defun golden-ratio-inhibit-popwin-config ()
  (let ((buffer (current-buffer)))
    (if (popwin:match-config buffer) t)))
