;;; hemacs --- an emacs configuration

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

(def delete-file-and-buffer
  (let ((filename (buffer-file-name)))
    (when filename
      (system-move-file-to-trash filename))
    (kill-buffer)))

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

(defun ensure-space ()
  (unless (looking-back " " nil)
    (insert " ")))

(def insert-arrow
  (ensure-space)
  (insert "-> "))

(def insert-fat-arrow
  (ensure-space)
  (insert "=> "))

(def smart-ruby-colon
  (if (looking-back "[[:word:]]" nil)
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
    (cond ((looking-back "=[[:space:]]" nil)
           (delete-char -1))
          ((looking-back "[^#/|!<>]" nil)
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
  (unless (looking-back "(" nil)
    (ensure-space))
  (insert "{  }")
  (backward-char 2))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(def create-scratch-buffer
  (let ((current-major-mode major-mode)
        (buf (generate-new-buffer "*scratch*")))
    (switch-to-buffer buf)
    (funcall current-major-mode)))

(def toggle-transparency
  (if (member (frame-parameter nil 'alpha) '(nil 100))
      (set-frame-parameter nil 'alpha 67)
    (set-frame-parameter nil 'alpha 100)))

(defun refresh-themed-packages ()
  (when (fboundp 'powerline-reset)
    (powerline-reset))
  (when (fboundp 'highlight-tail-reload)
    (highlight-tail-reload)))

(defun refresh-themed-packages-when-idle (&optional no-confirm no-enable)
  (run-with-idle-timer 1 nil #'refresh-themed-packages))

(provide 'hemacs)
