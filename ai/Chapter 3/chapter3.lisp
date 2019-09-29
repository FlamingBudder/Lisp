
(defun length1 (list)
  (let ((len 0))
    (dolist (element list len)
      (incf len))))


(defstruct student name division id (home-phone nil))

(defvar doc (make-student :name 'Docmo :division 'A321 :id '666321))
(defvar self (make-student :name 'Alec :division '141 :id '44428741 :home-phone '773-534-7500))

(defvar namediv
  (let ((n (student-name doc))
        (d (student-division doc)))
    (list n d)))

