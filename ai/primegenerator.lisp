(defparameter primes '(2))
(defparameter checking 3)

;; find if n is prime
(defun isprime (n)
  (cond ((= n checking)
         (loop for p in primes
               never (= (mod n p)
                        0)))
        ((< n checking)
         (member n primes))
        ((> n checking)
         (progn (genprimes n)
                (isprime n)))))

;; generates primes up to n, returns whether n is prime
(defun genprimes (n)
  (cond ((<= n checking)
         primes)
        (t (if (isprime checking)
               (progn (setf primes (append primes (list checking)))
                      (incf checking)
                      (genprimes n))
               (progn (incf checking)
                      (genprimes n))))))
