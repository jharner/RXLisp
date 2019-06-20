;;;;
;;;; Principal Components Prototype: does ordinary principal
;;;;     component analysis and builds dataset for biplots                           ;;;;

(defproto princomp-proto '(no-of-components y-udv y-column-weights )
  '(instances) multivariate-data-proto "Principal Components")

(defmeth princomp-proto :new (dataset &rest args)
  (let ((princomp-data (apply #'call-next-method dataset args)))
    (setf (slot-value 'instances)
          (cons princomp-data (slot-value 'instances)))
    princomp-data))

;;;
;;;Constructor Function
;;;
      
(defun make-princomp (dataset data-browser &key name the-type robust
                              tolerance tuning-value iterations)
  (let* ((princomp nil)
         (x-variables (send dataset :variables-of-role 'X))) 
    (if x-variables
        (let* ((count (+ (length (send redundancy-proto
                                       :slot-value 'instances)) 1))
               (name (if name name  
                         (concatenate 'string "RDA "
                                      (format nil "~s" count)))))
          (setf princomp (send redundancy-proto :new dataset :name name
                               :the-type the-type :robust robust
                               :tolerance tolerance
                               :tuning-value tuning-value
                               :iterations iterations)))
        (let* ((count (+ (length (send princomp-proto
                                       :slot-value 'instances)) 1))
               (name (if name name  
                         (concatenate 'string "PCA "
                                      (format nil "~s" count)))))
          (setf princomp (send princomp-proto :new dataset :name name
                               :the-type the-type :robust robust
                               :tolerance tolerance
                               :tuning-value tuning-value
                               :iterations iterations))))
    (when princomp 
          (send *data-tree* :register-dataset dataset princomp))))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth princomp-proto :y-udv ()
  (slot-value 'y-udv))


(defmeth princomp-proto :y-column-weights ()
  (slot-value 'y-column-weights))


;;;
;;; Computes principal components and the corresponding eigenvalues and
;;;     canonical correlations between the components and variables
;;;

(defmeth princomp-proto :compute ()
  (let* ((ytilda (send self :preprocess))
         (row-weights (send self :find-row-weights ytilda))
         (y-column-weights (send self :find-column-weights ytilda))
         (ystar (matmult (diagonal  row-weights)
                         (matmult ytilda (diagonal y-column-weights))))      
         (y-udv (sv-decomp ystar)))
    (setf (slot-value 'y-udv) y-udv)))

(defmeth princomp-proto :add-new-variables ()
  (send self :make-variables (first (slot-value 'y-udv)) "PC" ))

(defmeth princomp-proto :find-row-weights (&optional y)
  (let ((n (send self :num-obs-unmasked)))
    (make-array n :initial-element 1)))

(defmeth princomp-proto :find-column-weights (&optional y)
  (let* ((n (send self :num-obs-unmasked))
         (num-y-vars (slot-value 'num-y-vars))
         (y-column-weights (case (slot-value 'the-type)
                             (corr (/ (* (sqrt (- n 1))
                                         (mapcar #'standard-deviation
                                                 (column-list y)))))
                             (cov (make-array num-y-vars :initial-element 1))
                             (custom (slot-value 'y-column-weights)))))
         
    (setf (slot-value 'y-column-weights) y-column-weights)))

(defmeth princomp-proto :preprocess ()
  (let* ((robust (slot-value 'robust))
         (centered-matrix (if robust 
                          (send self :robust-summaries )
                          (send self :numerical-summaries)))
         (num-y-vars (slot-value 'num-y-vars))
         (num-z-vars (slot-value 'num-z-vars))
         (num-obs (array-dimension centered-matrix 0))
         (i (iseq num-y-vars))
         (j (iseq num-y-vars (+ num-y-vars num-z-vars (- 1))))
         (l (iseq num-obs))
         (y-centered (select centered-matrix  l i))
         (z-centered (if (> num-z-vars 0) (select centered-matrix l j)))
         (ytilda (if (> num-z-vars 0) 
                     (second (project y-centered z-centered))
                     y-centered)))
    ytilda))

;;;
;;;  Creates a dataset for the principal component biplot,
;;;      taking the value of alpha as argument;the datapoints
;;;      representing the original variables are masked with nil
;;;      It also makes a label-variable for the biplot-dataset
;;;

(defmeth princomp-proto :make-biplot-dataset (alpha &key type)
  (let* ((y-udv (slot-value 'y-udv))
         (scores (first y-udv))
         (eigenvectors (third y-udv))
         (singular-values (second y-udv))
         (dataset (slot-value 'owner))
         (dataset-name (send dataset :name)) 
         (g-values (column-list 
                    (matmult scores (diagonal (^ singular-values alpha)))))
         (mask (slot-value 'mask))
         (label nil)
         (labels-var (dolist (var (send dataset :variables) label)
                             (if (eql (send (eval var) :role) 'L)
                                 (setf label var))))
         (labels (if labels-var (select (send (eval labels-var) :values)
                                        (which mask))
                     (iseq (length (first g-values)))))
         (h-values (column-list 
                    (matmult eigenvectors 
                            (diagonal (^ singular-values (- 1 alpha))))))
         (max-g (max g-values))
         (max-h (max h-values))
         (fact (/ max-g max-h))
         (h-values (map-elements #'* fact h-values))
         
         (names (send dataset :get-names-of-role 'Y))
         (varscores nil)
         (value-list (dotimes (i (length g-values) (reverse varscores))
                              (setf varscores
                                 (cons (concatenate 'list (select g-values i)
                                           (select h-values i)) varscores))))
         (vars nil)
         (i 0)
         (variable-list
          (dolist (var value-list (reverse vars))
                  (setf i (+ i 1))
                  (setf vars (cons
                              (let ((variable  
                                     (make-variable (coerce var 'list) 
                                     :name (format nil "~a ~d" type i))))
                                (send variable :role 'Y)
                                variable)
                                vars))))
         (label-var (make-variable (concatenate 'list labels names)
                                   :name "LABEL" :type 'C :role 'L))
         (variables (cons label-var variable-list))
         (princ-dataset 
          (send dataset-proto :new variables :name (send self :name)))
         (num-obs (length (first g-values)))
         (num-vars (length (first h-values)))
         (old-color (select (slot-value 'color)
                            (which (slot-value 'mask))))
         (color (concatenate 'list old-color (repeat 'black num-vars)))
         (old-state (select (slot-value 'state) 
                            (which (slot-value 'mask))))
         (state (concatenate 'list old-state (repeat 'invisible num-vars))) 
         (old-symbol (select (slot-value 'symbol)
                            (which (slot-value 'mask))))
         (symbol (concatenate 'list old-symbol (repeat 'cross num-vars))))
    (send princ-dataset :color color)
    (send princ-dataset :symbol symbol)
    (send princ-dataset :state state)
    (send princ-dataset :variables variable-list)
    (setf *biplo* princ-dataset)))

;;;;
;;;;  Princomp Report
;;;;

(defmeth princomp-proto :make-matrix-list ()
  (let* ((variable-names (send self :get-names-by-role))
         (num-y-vars (slot-value 'num-y-vars))
         (y-names (third variable-names))
         (j (iseq num-y-vars )) 
         (means (select (slot-value 'means) j))
         (standard-deviations (select (slot-value 'standard-deviations) j))
         (covariance (select (slot-value 'covariance) j j))
         (correlation (select (slot-value 'correlation) j j))
         (cov-corr (make-array (list (array-dimension correlation 0)
                                     (array-dimension correlation 0))))
         (pc-names nil)
         (pc-variances (^ (second (slot-value 'y-udv)) 2))
         (pc-coefficients (third (slot-value 'y-udv))))
    (dotimes (i (length y-names))
             (setf pc-names
                   (cons (format nil "PC ~d" (+ i 1)) pc-names)))
    (dotimes (i (array-dimension correlation 0))
            (dotimes (j (array-dimension correlation 0))
                    (setf (aref cov-corr i j)
                          (if (> j i)  (aref correlation i j)
                             (aref covariance i j)))))
    (let* ((mean-mat (send matrix-proto :new
                          "Means" (coerce means 'vector)
                          :column-labels y-names))
           (std-mat (send matrix-proto
                         :new "Standard Deviations"
                         (coerce standard-deviations 'vector)
                         :column-labels y-names))
           (cov-corr-mat (send matrix-proto
                              :new "Covariance/Correlation Matrix"
                              cov-corr :row-labels y-names
                              :column-labels y-names))
           (pc-names (reverse pc-names))
           (pc-var (send matrix-proto :new "PrinComp Variances"
                        (coerce pc-variances 'vector)
                        :column-labels pc-names))
           (pc-coeff (send matrix-proto :new "PrinComp Coefficients"
                          pc-coefficients
                          :row-labels y-names
                           :column-labels pc-names)))
      (list mean-mat std-mat cov-corr-mat pc-var pc-coeff))))

(defmeth princomp-proto :make-stat-menu (browser)
  (let* ((princomp (send browser :dataset))
         (icon-view (send browser :icon-view))
         (menu (send stat-menu-proto :new browser)))
    (send menu :append-items
          (send dash-item-proto :new)
          (send menu-item-proto :new "Show Report"
                :action #'(lambda ()
                            (let ((report (send report-proto :new princomp)))
                              (send icon-view :register-view report))))
          (send menu-item-proto :new "PCA Biplot"
                :action
                #'(lambda () 
                    (let ((num-obs-points (send princomp :num-obs-unmasked))
                          (alpha (get-value-dialog
                                        "Choose an alpha between 0 and 1:"
                                  :initial 1)))
                      (if (and alpha (and  (>= (first alpha) 0)
                                           (<= (first alpha) 1)))
                          (make-xyplot (send princomp :make-biplot-dataset
                                             (first alpha) :type "PRINC")
                                       icon-view num-obs-points)
                          (message-dialog
                                 "Incorrect value: alpha must be between
                                0 and 1"))))))
    menu))

(defmeth princomp-proto :make-stat-options-menu (browser)
  nil)
