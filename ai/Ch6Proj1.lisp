(defstruct rule name inputs funct)

(defparameter *rules*
  (pairlis '(gerbil dog cat + *)
           (list (make-rule :name 'gerbil
                            :inputs '(str str)
                            :funct (lambda (name activity)
                                     `(The gerbil ,name loves to ,activity)))
                 (make-rule :name 'dog
                            :inputs '(str (opt num))
                            :funct (lambda (name &optional (barks 0))
                                     (append `(,name loves to bark.)
                                             (loop repeat barks
                                                   collect 'Bark!))))
                 (make-rule :name 'cat
                            :inputs '(str)
                            :funct (lambda (name)
                                     (append `(The cat ,name loves to)
                                             (elt '(purr meow raawr)
                                                  (random 3)))))
                 (make-rule :name '+
                            :inputs '(num num)
                            :funct (lambda (x y)
                                     (+ x y)))
                 (make-rule :name '*
                            :inputs '(num num)
                            :funct (lambda (x y)
                                     (* x y))))))

(defparameter *rules*
  )

(defparameter typematch
  (pairlis '(str num)
           '(symbolp numberp)))

(defun match-type (input type)
  (funcall (cdr (assoc type typematch)) input))

(defun checktype (input types)
  (every #'match-type
         input
         types))

(defun runcase (input rule)
  (if (checktype input (rule-inputs rule))
      (apply (rule-funct rule)
             input)))
