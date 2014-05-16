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

(defmacro allow-line-as-region-for-function (orig-function)
  `(defun ,(intern (concat (symbol-name orig-function) "-or-line"))
       ()
     ,(format "Like `%s', but acts on the current line if mark is not active."
              orig-function)
     (interactive)
     (if mark-active
         (call-interactively (function ,orig-function))
       (save-excursion
         (beginning-of-line)
         (set-mark (point))
         (end-of-line)
         (call-interactively (function ,orig-function))))))
