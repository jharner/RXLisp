;;;;
;;;; Redundancy Analysis Prototype
;;;;


(defproto redundancy-proto '() '(instances) cancorr-proto
  "Redundancy Analysis")



(defmeth redundancy-proto :find-column-weightmatrix (cov-matrices)
  (let ((num-y-var (array-dimension (third cov-matrices) 0)))
    (diagonal (make-array num-y-var :initial-element 1))))

;;;;
;;;;   Canonical Correspondence Analysis Prototype
;;;;

(defproto cancorresp-proto '(y-column-weights) '(instances) cancorr-proto "Canonical Correspondence Analysis")

(defmeth cancorresp-proto :new (dataset &rest args)
  (let ((cancorresp-data (apply #'call-next-method dataset args)))
    (setf (slot-value 'instances)
          (cons cancorresp-data (slot-value 'instances)))
    cancorresp-data))

(defmeth cancorresp-proto :preprocess ()
  (let* ((x (send self :make-data-matrix-of-role 'X))
         (y (send self :make-data-matrix-of-role 'Y))
         (z (send self :make-data-matrix-of-role 'Z))
         (x (if z (second (project x z)) x))
         (y (if z (second (project y z)) y))
         (x-weights (send self :find-row-weights x))
         (mean-x (/ (matmult x-weights x) (sum x-weights)))
         (num-obs (array-dimension x 0))
         (j (make-array num-obs :initial-element 1))
         (centered-x (- x (outer-product j mean-x)))
         (syy (diagonal (send self :find-column-weights y)))
         (syx (matmult (transpose y ) centered-x))
         (sxx (matmult (transpose centered-x)
                       (matmult (diagonal (send self :find-row-weights y)) centered-x))))
    (setf (slot-value 'y-can-scores) y)
    (setf (slot-value 'x-can-scores) centered-x) 
    (setf (slot-value 'num-x-vars) (array-dimension x 1))
    (setf (slot-value 'num-y-vars) (array-dimension y 1))
    (setf (slot-value 'num-z-vars) (if z (array-dimension z 1) 0))
    (list sxx syx syy))) 

(defmeth cancorresp-proto :find-row-weights (y)
 (let ((rowsums (reduce #'+ (column-list y ))))
  (setf (slot-value 'row-weights) rowsums)))

(defmeth cancorresp-proto :find-column-weights (y)
  (let ((colsums (reduce #'+ (row-list y ))))
    (setf (slot-value 'y-column-weights) colsums)))  

;;;;
;;;;CanCorresp Report
;;;;


(defmeth cancorresp-proto :make-matrix-list ()
  (let* ((variable-names (send self :get-names-by-role))
         (x-names (second variable-names))
         (y-names (third variable-names))
         (no-of-pairs (min (length x-names) (length y-names)))
         (eigenvalues (^ (second (slot-value 'udv)) 2))
         (x-can-coeff (slot-value 'x-can-coeff))
         (y-can-coeff (slot-value 'y-can-coeff))
         (x-can-names nil)
         (y-can-names nil)
         (corresp-names nil)
         (x-components-names
          (dotimes (i no-of-pairs (reverse x-can-names))
                   (setf x-can-names
                         (cons (format nil "X-Can ~d" (+ i 1)) x-can-names)))) 
         (y-components-names
          (dotimes (i no-of-pairs (reverse y-can-names))
                   (setf y-can-names
                         (cons (format nil "Y-Can ~d" (+ i 1)) y-can-names))))
         (eigenvalue-names
          (dotimes (i no-of-pairs (reverse corresp-names))
                   (setf corresp-names
                         (cons (format nil "Corresp ~d" (+ i 1))
                               corresp-names)))) 
         (corresp-val (send matrix-proto :new "Eigenvalues"
                            (coerce eigenvalues 'vector)
                            :column-labels eigenvalue-names))
         (xcan-mat
          (send matrix-proto
                :new "X-Correspondence Variables" x-can-coeff
                :row-labels x-names :column-labels x-components-names))
         (ycan-mat
          (send matrix-proto
                :new "Y-Correspondence Variables" y-can-coeff
                :row-labels y-names :column-labels y-components-names)))
    (list corresp-val xcan-mat ycan-mat)))