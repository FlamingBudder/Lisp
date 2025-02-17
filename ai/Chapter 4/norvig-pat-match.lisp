
;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; TAKEN FROM File eliza1.lisp: Basic version of the Eliza program

;;; The basic are in auxfns.lisp; look for "PATTERN MATCHING FACILITY"

;; New version of pat-match with segment variables

(declaim (optimize (safety 3) (debug 3) (speed 0)))

(defconstant MATCH-FAIL nil)
(defvar MATCH-EMPTY '((T . T)))
(defvar NO-BINDINGS MATCH-EMPTY)

(defun make-binding (var val) (cons var val))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings NO-BINDINGS)
            nil
            bindings)))


(defun starts-with (list x)
  "Is x a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t MATCH-FAIL))))

(defun pat-match (pattern input &optional (bindings NO-BINDINGS))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings MATCH-FAIL) MATCH-FAIL)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)                ; ***
         (segment-match pattern input bindings))    ; ***
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input)
                               bindings)))
        (t MATCH-FAIL)))

(defun segment-pattern-p (pattern)
  "Is this a segment matching pattern: ((?*/?+ var) . pat), if so return "
  (if (and (consp pattern)
           (or (starts-with (first pattern) '?*)
               (starts-with (first pattern) '?+)))
      (first (first pattern))))

(defun determine-start (x)
  "Determine where to start seg-match based on ?* or ?+"
  (cond ((eq '?* x) 0)
        ((eq '?+ x) 1)))

;;; ==============================

(defun segment-match (pattern input bindings &optional (start (determine-start (segment-pattern-p pattern))))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        ;; We assume that pat starts with a constant
        ;; In other words, a pattern can't have 2 consecutive vars
        (let ((pos (position (first pat) input
                             :start start :test #'equal)))
          (if (null pos)
              MATCH-FAIL
              (let ((b2 (pat-match
                          pat (subseq input pos)
                          (match-variable var (subseq input 0 pos)
                                          bindings))))
                ;; If this match MATCH-FAILed, try another longer one
                (if (eq b2 MATCH-FAIL)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred"
  (let* ((var (first var-and-pred))
         (pred (second var-and-pred))
         (new-bindings (pat-match var input bindings)))
    (if (or (eq new-bindings match-fail)
            (not (funcall pred input)))
        match-fail
        new-bindings)))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input"
  (loop for pat in patterns
        thereis (pat-match pat input bindings)))
