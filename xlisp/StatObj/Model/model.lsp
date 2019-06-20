(provide "model")

(setf *current-model* nil)
(setf *current-model-branch* nil)
(setf *model-protos* nil)

;;;;
;;;;
;;;;
;;;; Model Frame  Prototype
;;;;
;;;;

(defproto model-frame-proto '(owner model-tree dependent-variable)
  '(instances) dataset-proto "Model Frame")

(defmeth model-frame-proto :new (dataset &rest args)
  (let ((model-data (apply #'call-next-method dataset args)))
    (setf (slot-value 'instances)
          (cons model-data (slot-value 'instances)))
    model-data))

(defmeth model-frame-proto :isnew (dataset &rest args)
 (let*  ((variable-lists (send dataset :variables-by-role))
         (x-variables (second variable-lists))
         (y-variable (third variable-lists))
         (str nil))
   (setf (slot-value 'owner) dataset)
   (setf (slot-value 'variables)
         (concatenate 'list x-variables y-variable))
   (setf (slot-value 'dependent-variable) (car y-variable))
   (dolist (x-var (reverse x-variables))
           (setf str
                 (concatenate 'string (format nil " ~s"
                                              (send x-var :name)) str)))
   (setf (slot-value 'name)
         (concatenate 'string
                      (format nil "Model: ~s ~~"
                              (send (first y-variable) :name)) str))
   (setf (slot-value 'mask) (copy-list (send dataset :mask)))
   (setf (slot-value 'symbol) (send dataset :slot-value 'symbol))
   (setf (slot-value 'color) (send dataset :slot-value 'color))
   (setf (slot-value 'state) (send dataset :slot-value 'state))
   (setf (slot-value 'model-tree)
         (send model-tree-proto :new self))))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth model-frame-proto :owner (&optional (owner nil set-owner))
  (if set-owner (setf (slot-value 'owner) owner))
  (slot-value 'owner))

(defmeth model-frame-proto :model-tree
  (&optional (model-tree nil set-model-tree))
  (if set-model-tree (setf (slot-value 'model-tree) model-tree))
  (slot-value 'model-tree))

(defmeth model-frame-proto :dependent-variable ()
  (slot-value 'dependent-variable))

(defmeth model-frame-proto :x-names () nil)

(defmeth model-frame-proto :instances
  (&optional (instances nil set-instances))
  (if set-instances (setf (slot-value 'instances) instances))
  (slot-value 'instances))

;;;
;;; Constructor Functions
;;;

(defun make-model (dataset data-browser)
  (let* ((model (send model-frame-proto :new dataset)))
    (send *data-tree* :register-dataset dataset model)
    (send *data-tree* :register-dataset-view dataset
          (display-model-browser (send model :model-tree)))))

;;;;;
;;;;; Model-frame-menu-proto
;;;;;

(defproto model-frame-menu-proto '(model-frame) ()
  menu-proto "Model-frame Menu")

(defmeth model-frame-menu-proto :isnew (master-view)
  (let* ((model-frame (send master-view :dataset))
         (model-tree (send model-frame :model-tree)))
    (call-next-method "Model Frame")
    (send self :append-items
          (send menu-item-proto :new "Messages")
          (send dash-item-proto :new)
          (send menu-item-proto :new "Show Model Browser"
                :action
                #'(lambda () (send master-view :register-view
                                   (display-model-browser model-tree))))
          (send menu-item-proto :new "Remove Model Browser"
                :action
                #'(lambda () (send master-view
                                   :delete-view model-browser-proto)))
          (send dash-item-proto :new)
          (send menu-item-proto :new "Delete Model" 
                :action #'(lambda () 
                            (send master-view :delete-model))))))

;;;;
;;;;
;;;; Model Tree Prototype
;;;;
;;;;

(defproto model-tree-proto  '(owner tree name y-coord model-count found-it)
  () *object* "Model Tree")

(defmeth model-tree-proto :isnew (data-frame)
  (setf (slot-value 'owner) data-frame)
  (setf (slot-value 'name) (send data-frame :name))
  (setf (slot-value 'y-coord) 0)
  (setf (slot-value 'model-count) 0)
  (setf (slot-value 'tree)
        (list model-proto 
              (list linear-model-proto glinear-model-proto)
              (list nonlinear-model-proto gnonlinear-model-proto))))

;;;
;;;  Slot Accessors and Mutators
;;;

(defmeth model-tree-proto :owner ()
  (slot-value 'owner))
 
(defmeth model-tree-proto :tree (&optional (tree nil set-tree))
  (if set-tree (setf (slot-value 'tree) tree))
  (slot-value 'tree))

(defmeth model-tree-proto :name ()
  (slot-value 'name))

(defmeth model-tree-proto :y-coord (&optional (y-coord nil set-y-coord))
  (if set-y-coord (setf (slot-value 'y-coord) y-coord))
  (slot-value 'y-coord))

(defmeth model-tree-proto :model-count (&optional model-count)
  (if model-count (setf (slot-value 'model-count) model-count))
  (slot-value 'model-count))

;;;
;;;  Other Methods
;;;

(defmeth model-tree-proto :add-model (model)
  (setf (slot-value 'found-it) nil)
  (let ((new-tree (send self :add-element (send self :tree) model)))
   (when (slot-value 'found-it) 
         (setf (slot-value 'tree) new-tree)
         (setf (slot-value 'y-coord) 0)
         (send self :position-elements))))
    
(defmeth model-tree-proto :add-element (tree model)
  (let ((parent (car (send model :parents))))
    (cond 
      ((null tree) nil)
      ((atom tree) (if (eql tree parent)
                      (progn 
                         (setf (slot-value 'found-it) t)
                         (list tree model))
                       tree))
      (t  
       (let ((node (car tree))
             (children (cdr tree)))
         (setf children (if (eql node parent)
                            (progn
                             (setf (slot-value 'found-it) t) 
                             (reverse (cons model (reverse children))))
                            
                            (if (member node (send parent :precedence-list))
                                (mapcar
                                 #'(lambda (branch)
                                     (send self :add-element branch model))
                                 children)
                                children)))
         (cons node children))))))

(defmeth model-tree-proto :remove-model (model)
  (setf (slot-value 'found-it) nil)
  (let ((new-tree (send self :remove-element (send self :tree) model)))
    (when (slot-value 'found-it) 
          (setf (slot-value 'tree) new-tree)
          (setf (slot-value 'y-coord) 0)
          (send self :position-elements)))
  (slot-value 'found-it))

(defmeth model-tree-proto :remove-element (tree model)
  (cond 
    ((null tree) nil)
    ((atom tree) (unless (eql tree model) tree)) 
    (t  
     (let ((node (car tree))
           (children (cdr tree))
           (new-children nil))
       (if (member model children) 
           (progn
            (setf (slot-value 'found-it) t)
            (if (= (length tree) 2) (car (remove model tree))
                (remove model tree))) 
           (progn
            (setf new-children
                  (if (member node (send model :precedence-list))
                      (mapcar #'(lambda (branch)
                                  (send self :remove-element branch model)) 
                              (remove-if
                               #'(lambda (x) (if (atom x) (eql model x) 
                                                 (eql (car x) model)))
                               children))
                      children))
            (unless (= (length children) (length new-children)) 
                    (setf (slot-value 'found-it) t))
            (cons node new-children)))))))

(defmeth model-tree-proto :position-elements
  (&optional current-branch x-coord)
  (let* ((tree (if current-branch current-branch (slot-value 'tree)))
         (x (if x-coord x-coord 0)))
    (cond
      ((atom tree) (send tree :x-coord x)
       (send tree :y-coord (slot-value 'y-coord)))
      (t (let ((node (car tree))
               (children (cdr tree)))
           (send node :x-coord x)
           (send node :y-coord (slot-value 'y-coord))
           (map-elements
            #'(lambda (branch x)
                (send self :position-elements branch x)
                (setf (slot-value 'y-coord)
                      (+ (slot-value 'y-coord) 1)))
            children (+ x 1))
           (setf (slot-value 'y-coord) (- (slot-value 'y-coord) 1)))))))

;;;;
;;;;
;;;; Model Prototype
;;;;
;;;;

(defproto model-proto '(owner dep x-names mean-fcn var-fcn
                              betas standard-errors error-df type
                              x-coord y-coord)
  () dataset-proto "Model")

(send model-proto :name "Model Proto")

(defmeth model-proto :isnew (model-frame &key name dep mean-fcn var-fcn)
  (setf (slot-value 'owner) model-frame)
  (setf (slot-value 'name) (if name name (slot-value 'proto-name)))
  (setf (slot-value 'type) 'M)
  (setf (slot-value 'variables) (send model-frame :variables))
  (setf (slot-value 'mask) (copy-list (send model-frame :mask)))
  (setf (slot-value 'symbol) (send model-frame :slot-value 'symbol))
  (setf (slot-value 'color) (send model-frame :slot-value 'color))
  (setf (slot-value 'state) (send model-frame :slot-value 'state))
  (if dep (setf (slot-value 'dep) dep))
  (if mean-fcn (setf (slot-value 'mean-fcn) mean-fcn))
  (if var-fcn (setf (slot-value 'var-fcn) var-fcn)))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth model-proto :dep ()
  (slot-value 'dep))

(defmeth model-proto :x-names ()
  (slot-value 'x-names))

(defmeth model-proto :mean-fcn ()
  (slot-value 'mean-fcn))

(defmeth model-proto :var-fcn ()
  (slot-value 'var-fcn))

(defmeth model-proto :type ()
  (slot-value 'type))

(defmeth model-proto :betas (&optional (betas nil set-betas))
  (if  set-betas (setf (slot-value 'betas) betas))
  (slot-value 'betas))

(defmeth model-proto :standard-errors
  (&optional (standard-errors nil set-standard-errors))
  (if set-standard-errors
      (setf (slot-value 'standard-errors) standard-errors))
  (slot-value 'standard-errors))

(defmeth model-proto :error-df (&optional (error-df nil set-error-df))
  (if  set-error-df (setf (slot-value 'error-df) error-df))
  (slot-value 'error-df))

(defmeth model-proto :x-coord (&optional x-coord)
  (if x-coord (setf (slot-value 'x-coord) x-coord))
  (slot-value 'x-coord))

(defmeth model-proto :y-coord (&optional y)
  (if y (setf (slot-value 'y-coord) y))
  (slot-value 'y-coord))

;;;
;;; Other Methods
;;;

(defmeth model-proto :model-state () nil)

(defmeth model-proto :compute () nil)

(defmeth model-proto :y-name () nil)

(defmeth model-proto :x-names () nil)
 
(defmeth model-proto :dependent-variable ()
  (send (slot-value 'owner) :dependent-variable))

(defmeth model-proto :make-header ()
  (let* ((dep (slot-value 'dep))
         (mean-fcn (slot-value 'mean-fcn))
         (var-fcn (slot-value 'var-fcn))
         (error-df (slot-value 'error-df))
         (dep-text (format nil "~%The dependent variable is: ~90a" dep))
         (mean-fcn-text (format nil "~%The mean function is: ~150a" mean-fcn))
         (var-fcn-text (format nil
                               "The variance function is: ~150a~%" var-fcn))
         (error-df-text (format nil "~%The error df is: ~5d~%" error-df)))
    (list dep-text mean-fcn-text var-fcn-text error-df-text)))

(defmeth model-proto :make-matrix-list ()
  (let* ((variable-names (slot-value 'x-names))
         (t-table-names (list "Betas" "StdErrs" "t-values" "p-values"))
         (betas (slot-value 'betas))
         (standard-errors (slot-value 'standard-errors))
         (t-values (if betas (/ betas standard-errors) nil))
         (error-df (slot-value 'error-df))
         (p-values (if betas
                       (* 2 (- 1 (t-cdf (abs t-values) error-df)))
                       nil))
         (t-table nil))
    (if betas 
        (setf t-table (send matrix-proto :new "t-table"
                            (bind-columns betas standard-errors
                                          t-values p-values)
                            :row-labels variable-names
                            :column-labels t-table-names)))
    (list t-table)))

;;;;
;;;;
;;;; Linear Model Proto
;;;;
;;;;

(defproto linear-model-proto '(linear-predictor)
  () model-proto "Linear Model")

(send linear-model-proto :name "Linear Model")

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth linear-model-proto :linear-predictor
  (&optional (linear-predictor nil set-linear-predictor))
  (if set-linear-predictor
      (setf (slot-value 'linear-predictor) linear-predictor))
  (slot-value 'linear-predictor))

;;;
;;; Other Methods
;;;

(defmeth linear-model-proto :compute ()
  (let ((linear-predictor
         (send linear-expression-proto :new (slot-value 'mean-fcn))))
    (setf (slot-value 'linear-predictor) linear-predictor)
    (let* ((y-name (with-input-from-string (s (slot-value 'dep)) (read s)))
           (y (send (eval y-name) :values))
           (n (length y))
           (terms (send linear-predictor :terms))
           (x (send linear-predictor :model-matrix terms n))
           (x-names (first x))
           (udv (sv-decomp (second x)))
           (d (diagonal (second udv)))
           (betas (matmult (third udv) (inverse d) (transpose (first udv)) y))
           (fitted (matmult (second x) betas))
           (residuals (- y fitted))
           (error-df (- n (length betas)))
           (error-var (/ (inner-product residuals residuals) error-df))
           (standard-errors (sqrt (diagonal
                                   (matmult error-var (third udv)
                                            (diagonal (^ (second udv) -2))
                                            (transpose (third udv))))))
           (fitted-variable
            (make-variable fitted
                           :name (with-input-from-string
                                  (s (format nil "Fitted")) (read s))
                           :role 'X))
           (residual-variable
            (make-variable residuals
                           :name (with-input-from-string
                                  (s (format nil "Residuals")) (read s))
                           :role 'Y)))
      (setf (slot-value 'x-names) x-names)
      (setf (slot-value 'betas) betas)
      (setf (slot-value 'standard-errors) standard-errors)
      (setf (slot-value 'error-df) error-df)
      (let ((x-variables nil))
        (dolist (var x-names)
                (if (not (eql var '1))
                    (setf x-variables (cons (eval var) x-variables))))
        (setf (slot-value 'variables)
              (concatenate 'list x-variables
                            (cons (eval y-name)
                                  (cons fitted-variable
                                        (cons residual-variable nil)))))))))

;;;;
;;;;
;;;; Generalized Linear Model Proto
;;;;
;;;;

(defproto glinear-model-proto () () linear-model-proto
  "Generalized Linear Model")

(send glinear-model-proto :name "GenLin Model")

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth glinear-model-proto :link (&optional (link nil set-link))
  (if set-link (setf (slot-value 'link) link))
  (slot-value 'link))

;;;
;;; Other Methods
;;;

;;;;
;;;;
;;;; Nonlinear Model Proto
;;;;
;;;;

(defproto nonlinear-model-proto () () model-proto
  "Noninear Model")

(send nonlinear-model-proto :name "Nonlinear Model")

;;;
;;; Slot Accessors and Mutators
;;;

;;;
;;; Other Methods
;;;

;;;;
;;;;
;;;; Generalized Nonlinear Model Proto
;;;;
;;;;

(defproto gnonlinear-model-proto () () nonlinear-model-proto
  "Generalized Nonlinear Model")

(send gnonlinear-model-proto :name "GenNonlin Model")

;;;
;;; Slot Accessors and Mutators
;;;

;;;
;;; Other Methods
;;;

(setf *model-protos* (list model-proto linear-model-proto glinear-model-proto 
                           nonlinear-model-proto gnonlinear-model-proto)) 

