;; -*- lexical-binding: t -*-

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

(defmacro after (feature &rest forms)
  (declare (indent 1) (debug t))
  `(,(if (or (not (bound-and-true-p byte-compile-current-file))
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         #'progn
       #'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(defmacro use-feature (name &rest args)
  (declare (indent 1))
  `(use-package ,name
     :straight nil
     ,@args))

(defun ensure-space (direction)
  (let* ((char-fn
          (cond
           ((eq direction :before)
            #'char-before)
           ((eq direction :after)
            #'char-after)))
         (char-result (funcall char-fn)))
    (unless (and (not (eq char-result nil)) (string-match-p " " (char-to-string char-result)))
      (insert " "))
    (when (and (eq char-fn #'char-after) (looking-at " "))
      (forward-char))))

(def self-with-space
  (call-interactively #'self-insert-command)
  (ensure-space :after))

(def pad-equals
  (if (nth 3 (syntax-ppss))
      (call-interactively #'self-insert-command)
    (cond ((looking-back "=[[:space:]]" nil)
           (delete-char -1))
          ((looking-back "[^#/|!<>+~]" nil)
           (ensure-space :before)))
    (self-with-space)))

(def open-brackets-newline-and-indent
  (let ((inhibit-message t)
        (text
         (when (region-active-p)
           (buffer-substring-no-properties (region-beginning) (region-end)))))
    (when (region-active-p)
      (delete-region (region-beginning) (region-end)))
    (unless (looking-back (rx (or "(" "[")) nil)
      (ensure-space :before))
    (insert (concat "{\n" text "\n}"))
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))

(def pad-brackets
  (unless (looking-back (rx (or "(" "[")) nil)
    (ensure-space :before))
  (insert "{  }")
  (backward-char 2))

(def insert-arrow
  (ensure-space :before)
  (insert "->")
  (ensure-space :after))

(def insert-fat-arrow
  (ensure-space :before)
  (insert "=>")
  (ensure-space :after))

(def pad-pipes
  (ensure-space :before)
  (insert "||")
  (backward-char))

(defun delete-region-instead-of-kill-region (f &rest args)
  (cl-letf (((symbol-function 'kill-region) #'delete-region))
    (apply f args)))

(defun inhibit-message-in-minibuffer (f &rest args)
  (let ((inhibit-message (minibufferp)))
    (apply f args)))

(def text-smaller-no-truncation
  (setq truncate-lines nil)
  (set (make-local-variable 'scroll-margin) 0)
  (text-scale-set -0.25))

(defun set-or-update-alist-value-by-key (alist key value)
  (let ((current-cell (assq key (symbol-value alist))))
    (if current-cell
        (setcdr current-cell value)
      (add-to-list alist `(,key . ,value)))))
