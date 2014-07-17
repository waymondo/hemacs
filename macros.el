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

(defmacro with-region-or-line (func)
  `(defadvice ,func (before with-region-or-line activate compile)
     (interactive
      (if mark-active (list (region-beginning) (region-end))
        (list (line-beginning-position) (line-beginning-position 2))))))
