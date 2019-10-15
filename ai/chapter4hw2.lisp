(defvar *state* nil)

(defvar *ops* nil)

(defstruct op
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun remove-* (subtract all)
  (loop for x in all
        if (not (member x subtract))
        collect x))

(defun member-* (x xs)
  (let ((not 0))
    (loop for c in xs
          until (equalp x c)
          do (incf not))
    (< not (length xs))))
