(provide "statobj")

;;;;
;;;;Includes a generic variable prototype, prototypes for numerical and
;;;;    categorical variables, dialog views for variables, and variable menu
;;;;    protos
;;;;

;;;; 
;;;; Variable Prototype: generic variable prototype with slots for name, type,
;;;;     role, values dependents, flag for specifying transformed variables
;;;;

(defproto variable-proto '(name description values active owner dependents 
                                type dist role position)
  () compound-data-proto)

;;;
;;; Constructor Function: makes a variable; the type can be inferred or
;;;         specified
;;;

(defun make-variable (values &key (name nil) (active t) (type nil) (role nil)
                             (center 0) (scale 1))
  (let* ((values (cond
              ((symbolp values) (eval values))
              ((listp values) values)
              (t (error "The variable name must be a symbol or list"))))
         (type (if type type
                   (let* ((type 'N)
                          (categories nil))
                     (dolist (val values)
                             (if (and (symbolp val) (not (equalp val 'NA)))
                                 (setf categories (cons val categories))))
                     (if categories (setf type 'C))
                     type)))
         (var (cond
                ((equalp type 'N)
                 (send numeric-variable-proto :new values
                       :name name :active active :type type :role role
                       :center center :scale scale))
                ((equalp type 'C)
                 (send categoric-variable-proto :new values
                       :name name :active active :type type :role role)))))
    var))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth variable-proto :name (&optional (name nil set-name))
  (if set-name (setf (slot-value 'name) name))
  (slot-value 'name))

(defmeth variable-proto :description (&optional (desc nil set-desc))
  (if set-desc (setf (slot-value 'description) desc))
  (slot-value 'description))

(defmeth variable-proto :values (&optional (values nil set-values))
  (if set-values (setf (slot-value 'values) values))
  (slot-value 'values))

(defmeth variable-proto :active (&optional (active t set-active))
  (if set-active (setf (slot-value 'active) active))
  (slot-value 'active))

(defmeth variable-proto :owner (&optional (owner nil set-owner))
  (if set-owner (setf (slot-value 'owner) owner))
  (slot-value 'owner))

(defmeth variable-proto :type (&optional (type nil set-type))
  (if set-type (setf (slot-value 'type) type))
  (slot-value 'type))

(defmeth variable-proto :dist (&optional (dist nil set-dist))
  (if set-dist (setf (slot-value 'dist) dist))
  (slot-value 'dist))

(defmeth variable-proto :role (&optional (role nil set-role))
  (if set-role (setf (slot-value 'role) role))
  (slot-value 'role))

(defmeth variable-proto :dependents
  (&optional (dependents nil set-dependents))
  (if set-dependents (setf (slot-value 'dependents) dependents))
  (slot-value 'dependents))

;;;
;;; Other Methods
;;;

(defmeth variable-proto :set-type (new-type)
  (if (not (equalp (send self :type) new-type))
      (let* ((values (send self :values))
             (parent (car (send self :parents)))
             (new-parent nil))
        (case new-type
          ('N (let* ((value-types (send self :value-types values))
                     (categories (caddr value-types)))
                (if categories
                    (message-dialog "Not all values are numeric or NA")
                    (setf new-parent (car (send numeric-variable-proto
                                                :precedence-list))))))
          ('C (let* ((unique-values (send self :make-unique-values))
                     (p-unique (/ (length unique-values) (length values)))
                     (s (format nil "~,1f of the values are unique.~
                                     ~%Do you wish to proceed?" p-unique))
                     (ok (ok-or-cancel-dialog s)))
                (if ok (setf new-parent (car (send categoric-variable-proto
                                                   :precedence-list)))))))
        (if new-parent
            (progn
             (send self :reparent new-parent parent)
             (if (equal new-type 'C)
                 (progn (send self :type 'C)
                        (send self :add-slot 'formal-levels)
                        (send self :add-slot 'actual-levels)
                        (send self :add-slot 'ordered-levels)
                        (send self :add-slot 'contrast-matrix)
                        (send self :make-levels))
                 (progn (send self :type 'N)
                        (send self :add-slot 'cut-values)
                        (send self :add-slot 'unique-values)))))
        new-parent)))

(defmeth variable-proto :value-types (values)
  (let ((numbers nil)
        (num-na 0)
        (categories nil))
    (dolist (val values)
            (cond
              ((numberp val) (setf numbers (cons val numbers)))
              ((equalp val 'NA) (setf num-na (+ num-na 1)))
              (t (setf categories (cons val categories)))))
    (list (reverse numbers) num-na (reverse categories))))

(defmeth variable-proto :make-unique-values ()
  (let ((values (send self :values))
        (unique-values nil))
    (dolist (val values)
            (if (not (member val unique-values))
                (setf unique-values (cons val unique-values))))
    (reverse unique-values)))

;;;
;;; Compound Data Methods
;;;

(defmeth variable-proto :data-length () (length (slot-value 'values)))

(defmeth variable-proto :data-seq () (slot-value 'values))

(defmeth variable-proto :make-data (values)
  (let ((values (coerce values 'list)))
    (send self :new values)))

(defmeth variable-proto :print (&optional (stream t))
  (call-next-method)
  (terpri)
  (format stream "~a" (slot-value 'values)))

;;;;
;;;; Numeric Variable Prototype: a prototype for numerical variables;
;;;;     inherits from generic proto
;;;;

(defproto numeric-variable-proto
  '(center scale cut-values unique-values shingles num-shingles
           frac-overlap current-shingle) () variable-proto)

(defmeth numeric-variable-proto :isnew (values &rest args)
  (send self :values values)
  (send self :frac-overlap .5)
  (send self :num-shingles 0)
  (let ((unique-values (send self :make-unique-values)))
    (if (< (cadr unique-values) .5)
        (send self :unique-values (car unique-values))))
  (apply #'call-next-method args))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth numeric-variable-proto :current-shingle
  (&optional (current-shing nil set-current-shing))
  (if set-current-shing (setf (slot-value 'current-shingle) current-shing))
  (slot-value 'current-shingle))

(defmeth numeric-variable-proto :num-shingles (&optional num-shingles)
  (if num-shingles (setf (slot-value 'num-shingles) num-shingles))
  (slot-value 'num-shingles))

(defmeth numeric-variable-proto :center (&optional (center nil set-center))
  (if set-center (setf (slot-value 'center) center))
  (slot-value 'center))

(defmeth numeric-variable-proto :scale (&optional (scale nil set-scale))
  (if set-scale (setf (slot-value 'scale) scale))
  (slot-value 'scale))

(defmeth numeric-variable-proto :cut-values
  (&optional (cut-values nil set-cuts))
  (if set-cuts (setf (slot-value 'cut-values) cut-values))
  (slot-value 'cut-values))

(defmeth numeric-variable-proto :unique-values
  (&optional (unique-values nil set-unique-values))
  (if set-unique-values (setf (slot-value 'unique-values) unique-values))
  (slot-value 'unique-values))

(defmeth numeric-variable-proto :shingles
  (&optional (shingles nil set-shingles))
  (if set-shingles (setf (slot-value 'shingles) shingles))
  (slot-value 'shingles))

(defmeth numeric-variable-proto :frac-overlap
  (&optional (frac-overlap nil set-frac-overlap))
  (if set-frac-overlap (setf (slot-value 'frac-overlap) frac-overlap))
  (slot-value 'frac-overlap))

;;;
;;; Other Methods
;;;

(defmeth numeric-variable-proto :make-cuts () nil)

; Makes shingles for co-plots added 4/7/95.
; The number of shingles is the only parameter taken by the method.
; The default number of shingles is 6.
; The fraction of overlap between the shingles is also needed. It is a slot.
; The default fraction of overlap is .5, it is set in the isnew method.
; For each shingle an upper and lower bound must be calculated.
; The upper and lower bounds for the shingles are stored in a k by 2 matrix.
; The 'if' statements make sure the lower/upper bounds do not go below the  
;      variable's maximum and minimum values.
; The method returns the shingle-matrix
; The algorithm is taken from William Cleveland's book, "Visualizing Data"
;   pp.134-135   

(defmeth numeric-variable-proto :make-shingles (numb-shingles)
  (let* ((sorted-values (sort-data (send self :values)))
         (num-shingles (send self :num-shingles
                             (if (<= numb-shingles 0) 6 numb-shingles)))
         (num-values (send self :data-length))
         (overlap (send self :frac-overlap))
         (num-per-shingle
          (round (/ num-values (+ (* num-shingles (- 1 overlap)) overlap))))
         (shingle-matrix
          (make-array (list num-shingles 2) :initial-element 0)))
    (dotimes (i num-shingles shingle-matrix)
             (setf lower (round (* i (- 1 overlap) num-per-shingle)))
             (setf lower (if (or (< lower 0) (= i 0)) 0 lower))
             (setf (select shingle-matrix i 0) (select sorted-values lower))
             (setf upper
                   (round (- (+ num-per-shingle
                                (* i (- 1 overlap) num-per-shingle)) 1)))
             (setf upper
                   (if (or (>= upper num-values)
                           (= i (- num-shingles 1))) (- num-values 1) upper))
             (setf (select shingle-matrix i 1)
                   (select sorted-values upper)))))

(defmeth numeric-variable-proto :make-unique-values ()
  (let ((unique-values (call-next-method)))
    (sort-data unique-values)))

;;;;
;;;; Categoric Variable Prototype: inherits from generic variable-proto; use
;;;;     for variables with values in a finite number of categories
;;;;

(defproto categoric-variable-proto
  '(actual-levels formal-levels ordered-levels contrast-matrix 
                  current-shingle shingles)
  () variable-proto)

(defmeth categoric-variable-proto :isnew (values &rest args)
  (send self :values values)
  (send self :make-levels)
  (send self :make-default-contrast-matrix)
  (apply #'call-next-method args))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth categoric-variable-proto :current-shingle
  (&optional (current-shing nil set-current-shing))
  (if set-current-shing (setf (slot-value 'current-shingle) current-shing))
  (slot-value 'current-shingle))

(defmeth categoric-variable-proto :shingles
  (&optional (shingles nil set-shingles))
  (if set-shingles (setf (slot-value 'actual-levels) shingles))
  (slot-value 'actual-levels))

(defmeth categoric-variable-proto :values (&optional (values nil set-values))
  (if set-values (setf (slot-value 'values) values))
  (slot-value 'values))

(defmeth categoric-variable-proto :actual-levels
  (&optional (levels nil set-levels))
  (if set-levels (setf (slot-value 'actual-levels) levels))
  (slot-value 'actual-levels))

(defmeth categoric-variable-proto :formal-levels
  (&optional (levels nil set-levels))
  (if set-levels (setf (slot-value 'formal-levels) levels))
  (slot-value 'formal-levels))

(defmeth categoric-variable-proto :ordered-levels
  (&optional (levels nil set-levels))
  (if set-levels (setf (slot-value 'ordered-levels) levels))
  (slot-value 'ordered-levels))

(defmeth categoric-variable-proto :contrast-matrix
  (&optional (matrix nil set-matrix))
  (if set-matrix (setf (slot-value 'matrix) matrix))
  (slot-value 'matrix))

;;;
;;; Other Methods
;;;

(defmeth categoric-variable-proto :make-levels ()
  (let ((values (send self :values))
        (levels nil))
    (dolist (val values)
            (if (not (member val levels))
                (setf levels (cons val levels))))
    (setf levels (map-elements #'(lambda (x) (format nil "~s" x)) levels))
    (setf levels (map-elements
                  #'(lambda (x) (with-input-from-string (s x) (read s)))
                  (sort levels #'string<)))
    (send self :formal-levels (iseq (length levels)))
    (send self :actual-levels levels)))

(defmeth categoric-variable-proto :make-formal-values ()
  (let ((values (send self :values))
        (actual-levels (send self :actual-levels))
        (formal-levels (send self :formal-levels))
        (formal-values nil))
    (dolist (val values)
            (setf formal-values
                  (cons (elt formal-levels (position val actual-levels))
                        formal-values)))
    (reverse formal-values)))

(defmeth categoric-variable-proto :make-shingles (numb-shingles)
 (send self :shingles))


(defmeth categoric-variable-proto :make-ordered-levels () nil)

(defmeth categoric-variable-proto :make-default-contrast-matrix () nil)

(defmeth categoric-variable-proto :make-helmert-contrast () nil)

(defmeth categoric-variable-proto :make-sum-contrast () nil)

(defmeth categoric-variable-proto :make-treatment-contrast () nil)

(defmeth categoric-variable-proto :make-poly-contrast () nil)

;;;;
;;;; Recognizers
;;;;

(defun numeric-variable-p (variable)
  (kind-of-p (eval variable) numeric-variable-proto))

(defun categoric-variable-p (variable)
  (kind-of-p (eval variable) categoric-variable-proto))
