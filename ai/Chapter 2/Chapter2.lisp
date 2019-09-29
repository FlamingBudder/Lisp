(defparameter *simple-grammar*
  '((sentence (noun-phrase verb-phrase))
    (noun-phrase (Article Noun))
    (verb-phrase (Verb noun-phrase))
    (Article the a)
    (Noun man ball woman table)
    (Verb hit took saw liked))
  "A grammar for a trivial subset of English.")
(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially , thi s i s
*simple-grammar*, but we can switch to other grammars.")

(defun mappend (f list)
  (apply #'append (mapcar f list)))


(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))
(defun rule-rhs (rule)
  "The right-hand side of a rule."
  (rest rule))
(defun rewrites (category)
  "Return a list of the possible rewrites for thi s category."
  (rule-rhs (assoc category *grammar*)))

(defun random-elt (list)
  (elt list (random (length list))))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun generate2 (phrase)
  "Generate a random sentence or phrase"
  (if (listp phrase)
      (mappend #'generate phrase)
      (let ((choices (rewrites phrase)))
        (if (null choices)
           (list phrase)
           (generate (random-elt choices))))))
