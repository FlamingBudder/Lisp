
(defstruct (city (:type list)) name long lat)

(defparameter *cities*
  '((Atlanta 84.23 33.45) (Los-Angeles 118.15 34.03)
    (Boston 71.05 42.21) (Memphis 90.03 35.09)
    (Chicago 87.37 41.50) (New-York 73.58 40.47)
    (Denver 105.00 39.45) (Oklahoma-City 97.28 35.26)
    (Eugene 123.05 44.03) (Pittsburgh 79.57 40.27)
    (Flagstaff 111.41 35.13) (Quebec 71.11 46.49)
    (Grand-Jet 108.37 39.05) (Reno 119.49 39.30)
    (Houston 105.00 34.00) (San-Francisco 122.26 37.47)
    (Indianapolis 86.10 39.46) (Tampa 82.27 27.57)
    (Jacksonville 81.40 30.22) (Victoria 123.21 48.25)
    (Kansas-City 94.35 39.06) (Wilmington 77.57 34.14)))

(defun city (name)
  (assoc name *cities*))

(defun deg->radians (deg)
  (* (+ (truncate deg) (* (rem deg 1) 100/60)) pi 1/180))

(defun radconvert (city)
  (list (city-name city)
        (deg->radians (city-long city))
        (deg->radians (city-lat city))))

(defun air-distance (city1 city2)
  (let* ((c1 (radconvert (city city1)))
         (c2 (radconvert (city city2)))
         (lat1 (city-lat c1))
         (long1 (city-long c1))
         (lat2 (city-lat c2))
         (long2 (city-long c2))
         (dlat (- lat2 lat1))
         (dlong (- long2 long1))
         (a (+ (* (cos lat1) (cos lat2) (expt (sin (/ dlong 2)) 2))
               (expt (sin (/ dlat 2)) 2)))
         (c (* 2 (atan (sqrt a)
                       (sqrt (- 1 a))))))
    (* c 6371)))

(defun neighbors (city)
  (remove-if-not #'(lambda (c)
                     (and (not (eq (first c) city))
                          (< (air-distance (first c) city) 1000.0)))
                 *cities*))

(defun tree-search (states goal-p successors combiner)
  (cond ((null states) nil)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search
            (funcall combiner
                     (funcall successors (first states))
                     (rest states))
            goal-p successors combiner))))

(defun depth-first-search (start goal-p successors)
  (tree-search (list start) goal-p successors #'append))

(defun binary-tree (x) (list (* 2 x) (+ 1 (* 2 x))))
(defun make-finite (f l) #'(lambda (x) (if (< x l)
                                           (apply f (list x))
                                           nil)))

(defun is (value) #'(lambda (x) (eql x value)))
