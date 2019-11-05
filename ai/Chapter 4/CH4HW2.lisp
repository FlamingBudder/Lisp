(defvar *state* nil)

(defstruct op
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun remove-* (subtract all)
  (loop for x in all
        if (not (member-* x subtract))
        collect x))

(defun member-* (x xs)
  (let ((not 0))
    (loop for c in xs
          until (equalp x c)
          do (incf not))
    (if (< not (length xs))
        xs)))

(defun all-achieved (wanted got)
  (let ((num 0))
    (loop for x in wanted
          until (not (member-* x got))
          do (incf num))
    (if (= num (length wanted))
        got)))

(defun apply-op (state op1)
  (append (remove-* (op-del-list op1) state)
          (op-add-list op1)))

(defun rev-apply (state op1)
  (append (op-preconds op1)
          (remove-* (op-add-list op1) state)
          (op-del-list op1)))

(defun appropriate-p (op1 goal-list)
  (all-achieved goal-list (op-add-list op1)))

(defun find-appropriate-1 (ops goal)
  (loop for x in ops
        if (appropriate-p x (list goal))
        collect x))

(defun find-appropriate (goal-list)
  (let ((appropriate '()))
    (loop for x in goal-list
          do (setf appropriate
                   (nodupeappend (find-appropriate-1 *ops* x)
                                 appropriate)))
    appropriate))

(defun nodupeappend (add list)
  (append list (loop for x in add
                     if (not (member-* x list))
                     collect x)))

(defun achieve-all-h (goal orig opstack)
  (let ((need (remove-* orig goal)))
    (if (= (length need) 0)
        opstack
        (let ((appropriate (find-appropriate need))
              (done nil))
          (loop for op in appropriate
                until done
                do (setf done
                         (achieve-all-h (rev-apply goal op)
                                        orig
                                        (append opstack (list op)))))
          done))))

(defun achieve-all (orig goal)
  (let ((stack (achieve-all-h goal orig '())))
    (loop for op in (reverse stack)
          do (print (op-action op)))
    (if (> (length stack) 0)
        'solved)))

(defparameter *ops*
  *dessert-ops*)

(defparameter *cliff-ops*
  (list
   (make-op :action 'jump-off-a-cliff
            :preconds '(on-cliff)
            :add-list '(off-cliff dead)
            :del-list '(on-cliff))
   (make-op :action 'jump-on-cushion
            :preconds '(on-cliff cushion-placed)
            :add-list '(off-cliff landed-safely)
            :del-list '(on-cliff))
   (make-op :action 'place-cushion
            :preconds '()
            :add-list '(cushion-placed)
            :del-list '())))

(defparameter *school-ops*
  (list
   (make-op :action 'drive-son-to-school
            :preconds '(son-at-home car-works)
            :add-list '(son-at-school)
            :del-list '(son-at-home))
   (make-op :action 'shop-installs-battery
            :preconds '(car-needs-battery shop-knows-problem shop-has-money)
            :add-list '(car-works))
   (make-op :action 'tell-shop-problem
            :preconds '(in-communication-with-shop)
            :add-list '(shop-knows-problem))
   (make-op :action 'telephone-shop
            :preconds '(know-phone-number)
            :add-list '(in-communication-with-shop))
   (make-op :action 'look-up-number
            :preconds '(have-phone-book)
            :add-list '(know-phone-number))
   (make-op :action 'give-shop-money
            :preconds '(have-money)
            :add-list '(shop-has-money)
            :del-list '(have-money))))

(defparameter *dessert-ops*
  (list
   (make-op :action 'free-ice-cream
            :preconds '(ate-cake)
            :add-list '(have-ice-cream)
            :del-list '(free-ice-cream))
   (make-op :action 'eat-ice-cream
            :preconds '(have-ice-cream)
            :add-list '(ate-dessert)
            :del-list '(have-ice-cream))
   (make-op :action 'eat-cake
            :preconds '(have-cake)
            :add-list '(ate-cake ate-dessert)
            :del-list '(have-cake))
   (make-op :action 'buy-ice-cream
            :add-list '(have-ice-cream))
   (make-op :action 'buy-cake
            :add-list '(have-cake))
   (make-op :action 'buy-cake
            :add-list '(have-cake))))

