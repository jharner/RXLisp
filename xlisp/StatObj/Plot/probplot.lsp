;;;;
;;;;  Definition of probplot-data-proto. It inherits from plot-data-proto 
;;;;      and adds the slot 'p'
;;;;

(defproto probplot-data-proto '(p)  '(instances) plot-data-proto
  "ProbPlot Model")

(defmeth probplot-data-proto :new (dataset &rest args)
  (let ((probplot-data (apply #'call-next-method dataset args)))
    (setf (slot-value 'instances)
          (cons probplot-data (slot-value 'instances)))
    probplot-data))

;;;;
;;;;  Initialization method for probplot-data-proto
;;;;

(defmeth probplot-data-proto :isnew (dataset)
  (let* ((y-variable (eval (first (send dataset :variables-of-role 'Y))))
         (values (send y-variable :values))
         (ranks (rank values))
         (nq (normal-quant (/ (+ ranks .5) (length values))))
         (scaled-values (/ (- values (min values))
                           (- (max values) (min values))))
         (scaled-variable
          (make-variable scaled-values
                         :name
                         (format nil "Scaled- ~a" (send y-variable :name))
                         :role 'Y))
         (norm-quant (make-variable nq
                                    :name (format nil "N-Quantiles")
                                    :role 'X)))
    (setf (slot-value 'name)
          (format nil "~a ProbPlot" (send y-variable :name)))
    (setf (slot-value 'p) 1)
    (setf (slot-value 'variables)
          (list y-variable scaled-variable norm-quant))
    (call-next-method dataset)))

;;;;
;;;;  Slot Accessors and Mutators
;;;;

(defmeth probplot-data-proto :p (&optional p)
  (if p (setf (slot-value 'p) p))
  (slot-value 'p))


;;;;
;;;;  Method to transform the variable
;;;;

(defmeth probplot-data-proto :bc-transform (p)
  (let* ((variables (slot-value 'variables))
         (values (send (first variables) :values))
         (y (if (< (abs p) .0001) (log values) (/ (^ values p) p)))
         (min (min y))
         (ranks (rank y))
         (nq (normal-quant (/ (+ ranks .5) (length y))))
         (scaled-y (/ (- y min) (- (max y) min))))
    (send (second variables) :values scaled-y )
    (send (third variables) :values nq )
    (setf (slot-value 'p) p)))

;;;;
;;;;  Definition of probplot-proto. It inherits from plot-proto and adds the ;;;;      slot p-overlay
;;;;

(defproto probplot-proto '(p-overlay) () plot-proto "Probability Plot")

;;;
;;;  Constucts probplots
;;;

(defun make-probplots (dataset data-browser)
 (let* ((probplot-data (send probplot-data-proto :new dataset))
        (probplot (probplot probplot-data)))
   (send data-browser :register-dataset-with-plot probplot-data probplot)))

(defun probplot (probplot-data)
  (let* ((y-variable (first (send probplot-data :variables)))
         (probplot (send probplot-proto :new 2 :scale-type 'variable
                         :title (send probplot-data :name)
                         :dataset probplot-data :size '(400 300)))
         (p-overlay (send p-slider-overlay-proto
                          :new "P" (list 310 8) (list 53 10) 20
                          (list -1.0 2.0) 2.0 -1.0 .1))
         (menu-name (send menu-overlay-proto :new 'probtype "ProbPlot"))
         (menu (send probplot-menu-proto :new probplot))
         (y-name (format nil "~a" (send y-variable :name)))
         (x-range (get-nice-range -3 3 10))
         (y-range (get-nice-range 0 1 10)))
    (plot-helper probplot "Normal Quantiles" y-name probplot-data)
    (send probplot :plot-menu menu)
    (send probplot :add-overlay menu-name)
    (send probplot :add-overlay p-overlay)
    (send probplot :range 0 (first x-range) (second x-range) :draw nil)
    (send probplot :range 1 (first y-range) (second y-range) :draw nil)
    (send probplot :x-axis t t (third x-range) :draw nil)
    (send probplot :y-axis t t (third y-range) :draw nil)
   probplot))

;;;;
;;;;  Slot accessors and mutators
;;;;

(defmeth probplot-proto :p-overlay (&optional p-overlay)
  (if p-overlay (setf (slot-value 'p-overlay) p-overlay))
  (slot-value 'p-overlay))

;;;;
;;;;  Method to redraw the content of probplot-proto
;;;;

(defmeth probplot-proto :redraw-content ()
  (let* ((dataset (slot-value 'dataset))
         (scaled-y (send (second (send dataset :variables)) :values))
         (nq (send (third (send dataset :variables)) :values)))
    (send self :clear-points :draw nil)
    (send self :add-points (list nq scaled-y) :draw nil))
  (call-next-method))

;;;;
;;;;  Definition of probplot-model-menu-proto: inherits from menu-proto        
;;;;

(defproto probplot-model-menu-proto '() nil menu-proto "Probplot Model Menu")


;;;;
;;;;  Intilization method for probplot-model-menu-proto
;;;;

(defmeth probplot-model-menu-proto :isnew (master-view)
  (call-next-method "Probplot-Model")
    (send self :append-items
          (send menu-item-proto :new "Messages")
          (send dash-item-proto :new)
          (send menu-item-proto :new "Show ProbPlot" 
                :action #'(lambda () 
                            (send master-view :show-view probplot-proto)))
          (send menu-item-proto :new "Hide ProbPlot" 
                :action #'(lambda ()
                            (send master-view :hide-view probplot-proto)))
          (send dash-item-proto :new)
          (send menu-item-proto :new "Delete Probplot Model" 
                :action #'(lambda () 
                            (send master-view :delete-model)))))


;;;;
;;;;  Definition of probplot-menu-proto: inherits from plot-menu-proto.
;;;;

(defproto probplot-menu-proto '() nil plot-menu-proto "Probplot Menu")


;;;;
;;;;  Intitialization mentod for probplot-menu-proto
;;;;

(defmeth probplot-menu-proto :isnew (master-view)
  (call-next-method master-view)
    (send self :append-items
          (send dash-item-proto :new)
          (send menu-item-proto :new "Draw Ref Lines"
              :action #'(lambda ()
                          nil))))
