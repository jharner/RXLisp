(setf *operators* (list "(" ")" "+" "-" "*" "/" "^" ))
(setf *functions* (list "abs" "acos" "asin" "atan" "cos" "exp" "expt" "log"
                        "log-gamma" "sin" "sqrt" "tan"))

;;;;
;;;;  Stat Expression Proto
;;;;


(defproto numeric-expression-proto '(formula variables parameters)
  () numeric-variable-proto)

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth numeric-expression-proto :formula
  (&optional (formula nil set-formula))
  (if set-formula (setf (slot-value 'formula) formula))
  (slot-value 'formula))

(defmeth numeric-expression-proto :variables
  (&optional (variables nil set-variables))
  (if set-variables (setf (slot-value 'variables) variables))
  (slot-value 'variables))

(defmeth numeric-expression-proto :parameters
  (&optional (parameters nil set-parameters))
  (if set-parameters (setf (slot-value 'parameters) parameters))
  (slot-value 'parameters))

;;;
;;; Other Methods
;;;

(defmeth numeric-expression-proto :evaluate (formula) nil)

;;;;
;;;;  Linear Expression Proto
;;;;

(defproto linear-expression-proto '(terms intercept)
  () numeric-expression-proto)

(defmeth linear-expression-proto :isnew (text-formula)
  (setf (slot-value 'intercept) t)
  (setf (slot-value 'formula)
        (with-input-from-string (s text-formula) (read s)))
  (let* ((terms (cdr (slot-value 'formula))))
    (setf (slot-value 'terms) terms)))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth linear-expression-proto :terms (&optional (terms nil set-terms))
  (if set-terms (setf (slot-value 'terms) terms))
  (slot-value 'terms))

(defmeth linear-expression-proto :intercept
  (&optional (intercept nil set-intercept))
  (if set-intercept (setf (slot-value 'intercept) intercept))
  (slot-value 'intercept))

;;;
;;; Other Methods
;;;

(defmeth linear-expression-proto :model-matrix (terms n)
  (let ((x nil)
        (x-names nil))
    (dolist (term terms)
            (cond
              ((numeric-variable-p term)
               (setf x (cons (send (eval term) :values) x))
               (setf x-names (cons term x-names)))
              ((categoric-variable-p term)
               (setf x (cons (send (eval term) :values) x))
               (setf x-names (cons term x-names)))
              ((intercept-p term)
               (setf x (cons (repeat 1 n) x))
               (setf x-names (cons term x-names)))
              (t nil)))
    (let ((p (length x)))
      (list (reverse terms) (apply #'bind-columns (reverse x))))))

;;;;
;;;; Recognizers
;;;;

(defun intercept-p (intercept)
  (equal intercept '1))


;;;;
;;;; Transform Dialog
;;;;

(defproto transform-dialog-proto '(dataset) nil modal-dialog-proto
  "Expression Builder")

(defmeth transform-dialog-proto :isnew (dataset)
  (let* ((screen-size (screen-size))
         (screen-width (car screen-size))
         (screen-height (cadr screen-size))
         (var-count (+ (send dataset :num-var) 1))
         (variables (send dataset :variables))
         (new-var-name (send text-item-proto :new "Var Name"
                         :location '(5 5)))
         (new-var (send text-item-proto :new
                      (concatenate 'string "Var" (format nil "~s" var-count))
                      :editable t :location '(75 5) :size '(200 15)))
         (exp-name (send text-item-proto :new "Expr"
                              :location '(5 30)))
         (exp (send text-item-proto :new "" :editable t
                         :location '(75 30) :size '(368 80)))
         (var-name (send text-item-proto :new "Variables"
                            :location '(5 120)))
         (var (send list-item-proto
                       :new (map-elements
                             #'(lambda (x)
                                 (format nil "~s" (send (eval x) :name)))
                             variables)
                       :location '(5 140) :size '(150 130)))
         (operator-name (send text-item-proto :new "Operators"
                              :location '(165 120)))
         (operator (send list-item-proto
                         :new *operators*
                         :location '(165 140) :size '(120 130)))
         (func-name (send text-item-proto :new "Functions"
                                :location '(295 120)))
         (func (send list-item-proto
                       :new *functions*
                       :location '(295 140) :size '(150 130)))
         (help (send button-item-proto :new "Help"
                       :location '(5 280) :size '(70 20)))
         (cancel (send button-item-proto :new "Cancel"
                       :location '(290 280) :size '(70 20)))
         (run (send button-item-proto :new "Create"
                   :location '(370 280) :size '(70 20))))
    (send var :action
          #'(lambda (double)
              (if double
                  (let* ((vars (map-elements
                                #'(lambda (x) (send (eval x) :name))
                                variables))
                         (index (send var :selection))
                         (item (format nil "~s" (select vars index)))
                         (exp-text (send exp :text))
                         (new-text (concatenate 'string exp-text " " item)))
                    (send exp :text new-text)))))
    (send operator :action
          #'(lambda (double)
              (if double
                  (let* ((operators *operators*)
                         (index (send operator :selection))
                         (item (select operators index))
                         (exp-text (send exp :text))
                         (new-text (concatenate 'string exp-text " " item)))
                    (send exp :text new-text)))))
    (send func :action
          #'(lambda (double)
              (if double
                  (let* ((functions *functions*)
                         (index (send func :selection))
                         (item (select functions index))
                         (exp-text (send exp :text))
                         (new-text (concatenate 'string exp-text " " item)))
                    (send exp :text new-text)))))
    (send cancel :action
          #'(lambda () (send self :remove)))
    (send run :action #'(lambda ()
                         (let* ((exp-text (send exp :text))
                                (values (send dataset :evaluate exp-text))
                                (name (send new-var :text))
                                (var (send numeric-variable-proto :new values
                                           :name name :active t 
                                           :expression exp-text)))
                           (send dataset :add-variable var))))
    (call-next-method (list new-var-name new-var exp-name exp
                                    var-name var operator-name operator
                                    func-name func help cancel run)
                      :title "Transform Builder"
                      :location (list (/ (- screen-width 450) 2)
                                      (/ (- screen-height 375) 2))
                      :size '(450 305)
                      :default-button run)))



