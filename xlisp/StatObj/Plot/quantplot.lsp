;;;;
;;;;  Definition of quantplot-data-proto. It inherits from plot-data-proto                               ;;;;

(defproto quantplot-data-proto '() '(instances) plot-data-proto
  "QuantilePlot")

(defmeth quantplot-data-proto :new (dataset &rest args)
  (let ((quantplot-data (apply #'call-next-method dataset args)))
    (setf (slot-value 'instances)
          (cons quantplot-data (slot-value 'instances)))
    quantplot-data))

;;;;  Initialization method for quantileplot-data-proto

(defmeth quantplot-data-proto :isnew (dataset &rest args)
  (let* ((y-var (first args))
         (q (normal-quant (/ (+ (rank (send y-var :values)) .5)
                             (send dataset :num-obs))))
         (quantiles (make-variable q :name 'normal-quantiles :role 'X)))
    (setf (slot-value 'x-variables) quantiles)
    (setf (slot-value 'y-variables) y-var)
    (setf (slot-value 'name)
          (concatenate 'string
                       (format nil "QuantPlot: ")
                       (format nil "~s" (send y-var :name))))
    (call-method plot-data-proto :isnew dataset (list quantiles y-var))))

;;; Other Methods

(defmeth quantplot-data-proto :normal-quantiles ()
  (let* ((mask (send self :mask))
         (z-mask (send self :z-mask))
         (or-mask (map-elements #'vector-or mask z-mask))
         (not-mask (mapcar #'(lambda (val) (not val)) or-mask))
         (normal-quantiles (send (send self :x-variables) :values))
         (values (send (send self :y-variables) :values))
         (ranks (rank (select values (which not-mask))))
         (n (length (select values (which not-mask))))
         (q (normal-quant (/ (+ ranks .5) n))))
    (setf (select normal-quantiles (which not-mask)) q)
    (send (send self :x-variables) :values normal-quantiles)))

(defmeth quantplot-data-proto :compute-quantiles (conditioning)
  (let* ((values (send self :get-variable-values 'Y conditioning))
         (quartiles (quantile values '(0.25 0.5 0.75)))
         (iqr (- (third quartiles) (first quartiles)))
         (min (min values))
         (max (max values))
         (range (- max min)))
    (list min (first quartiles) (second quartiles) (third quartiles)
          max iqr range)))

;;;;
;;;;  Definition of quantileplot-proto. It inherits from scatplot-proto                               ;;;;

(defproto quantplot-proto '(q-visible q-lines) ()
  scatplot-proto "Quantile Plot")

(defmeth quantplot-proto :isnew (&rest args)
  (let* ((quant-plot (apply #'call-next-method args))
         (overlays (slot-value 'overlays))
         (menu (send quantplot-menu-proto :new quant-plot))
         (quantiles (send quantile-overlay-proto :new :lines 8)))
    (dolist (overlay overlays)
            (if (kind-of-p overlay menu-overlay-proto)
                (send overlay :string "QuantPlot")))
    (send quant-plot :add-overlay quantiles)
    (send quant-plot :plot-menu menu)
    (send quant-plot :summaries (list quantiles))
    quant-plot))

;;; Constucts quantplots for each y variable

(defun make-quantplot (y-var dataset)
  (let* ((quant-data (send quantplot-data-proto :new dataset y-var))
         (title (send quant-data :name))
         (quant-plot (send quantplot-proto :new 2 :title title
                           :dataset quant-data
                           :size '(350 275) :show nil))
         (z-variables (send dataset :z-variables))
         (n (send y-var :data-length))
         (x-max (ceiling (normal-quant (- 1 (/ 1 (* 2 n)))))))
    (send quant-data :y-variables y-var)
    (send quant-data :normal-quantiles)
    (if z-variables
        (send quant-data :compute-quantiles z-variables)
        (send quant-data :compute-quantiles nil))
    (send quant-plot :range 0 (- x-max) x-max :draw nil)
    (send quant-plot :x-axis t t (+ (* 2 x-max) 1) :draw nil)
    (when (not z-variables)
          (send quant-plot :needs-adjusting t)
          (send quant-plot :adjust-screen))
    (send quant-plot :redraw-bottom)
    (send *data-tree* :register-dataset-with-plot
          dataset quant-data quant-plot)
    (send quant-plot :show-window)
    quant-data))

;;;  Slot Accessors and Mutators

(defmeth quantplot-proto :q-visible (&optional (q-visible nil set-q))
  (if set-q (setf (slot-value 'q-visible) q-visible))
  (slot-value 'q-visible))

(defmeth quantplot-proto :q-lines (&optional (q-lines nil set-lines))
  (if set-lines (setf (slot-value 'q-lines) q-lines))
  (slot-value 'q-lines))

;;; Other Methods

(defmeth quantplot-proto :adjust-to-data ()
  (let* ((n (send self :num-points))
         (x-values (send self :point-coordinate 0 (iseq n)))
         (x-max (ceiling (normal-quant (- 1 (/ 1 (* 2 n))))))
         (y-values (send self :point-coordinate 1 (iseq n)))
         (unique-values (make-unique-values y-values))
         (min (min unique-values))
         (max (max unique-values))
         (num-intervals (ceiling (+ 1 (* (/ 3 (log 8)) (log n)))))
         (num-ticks (round (+ (- max min) 2)))
         (ticks (if (and (<= num-ticks 12) (< (/ num-ticks n) 0.3))
                    num-ticks num-intervals))
         (new-range (get-nice-range min max ticks)))
    (send self :range 0 (- x-max) x-max)
    (send self :range 1 (first new-range) (second new-range))
    (send self :x-axis t t (+ (* 2 x-max) 1))
    (send self :y-axis t t (third new-range))
    (send self :redraw)))

(defmeth quantplot-proto :redraw-lines ()
  (send self :clear-lines :draw nil)
  (if (send self :fit) (send self :robust-fit))
  (if (send self :q-lines) (send self :quant-lines))
  (send self :redraw-content))

(defmeth quantplot-proto :adjust-plot ()
  (let* ((dataset (slot-value 'dataset))
         (summaries (slot-value 'summaries)))
    (send dataset :normal-quantiles)
    (when (send (first summaries) :visible)
          (send dataset :compute-quantiles t))))

(defmeth quantplot-proto :robust-fit ()
  (let* ((dataset (slot-value 'dataset))
         (mask (send dataset :mask))
         (z-mask (send dataset :z-mask))
         (or-mask (map-elements #'vector-or mask z-mask))
         (not-mask (mapcar #'(lambda (val) (not val)) or-mask))
         (values (select (send (send dataset :y-variables) :values)
                         (which not-mask)))
         (x (select (send (send dataset :x-variables) :values)
                    (which not-mask)))
         (quartiles (quantile values '(0.25 0.5 0.75)))
         (iqr (- (third quartiles) (first quartiles)))
         (a (+ (first quartiles) (/ iqr 2)))
         (b (/ iqr 1.34898))
         (x-min (min x))
         (x-max (max x))
         (num-lines (send self :num-lines)))
    (send self :add-lines (list (list x-min x-max)
                                (list (+ a (* b x-min)) (+ a (* b x-max))))
          :draw nil)
    (send self :linestart-color (iseq num-lines (+ num-lines 1)) 'blue)))

(defmeth quantplot-proto :quant-lines ()
  (let* ((dataset (slot-value 'dataset))
         (mask (send dataset :mask))
         (z-mask (send dataset :z-mask))
         (or-mask (map-elements #'vector-or mask z-mask))
         (not-mask (mapcar #'(lambda (val) (not val)) or-mask))
         (values (select (send (send dataset :y-variables) :values)
                         (which not-mask)))
         (quartiles (quantile values '(0.25 0.5 0.75)))
         (iqr (- (third quartiles) (first quartiles)))
         (upper-inner-hinge (+ (third quartiles) (* 1.5 iqr)))
         (upper-outer-hinge (+ (third quartiles) (* 3 iqr)))
         (lower-inner-hinge (- (first quartiles) (* 1.5 iqr)))
         (lower-outer-hinge (- (first quartiles) (* 3 iqr)))
         (q-lines (combine lower-outer-hinge lower-inner-hinge quartiles
                           upper-inner-hinge upper-outer-hinge))
         (x-range (send self :range 0))
         (num-lines (send self :num-lines))
         (i 0))
    (dolist (line q-lines)
            (when (<= (min values) line (max values))
                  (send self :add-lines
                        (list (list (first x-range) (second x-range))
                              (list line line)) :draw nil)
                  (if (or (<= i 1) (>= i 5))
                      (send self :linestart-color
                            (iseq num-lines (+ num-lines 1)) 'red))
                  (setf num-lines (+ num-lines 2)))
            (setf i (+ i 1)))))

;;; Mark menu items

(defmeth quantplot-proto :set-menu ()
  (let* ((plot-menu (send self :plot-menu))
         (items (send plot-menu :items)))
    (send (select items 6) :mark (send self :fit))
    (send (select items 7) :mark (send self :q-lines))))

;;;;
;;;;  Definition of quantileplot-menu-proto: inhertis from plot-meu-proto
;;;;

(defproto quantplot-menu-proto '() nil plot-menu-proto
  "QuantPlot Menu")

;;;;  Initialization method for quantplot-menu-proto

(defmeth quantplot-menu-proto :isnew (plot)
  (let* ((fit (send menu-item-proto :new "Robust Fit"
                 :action #'(lambda ()
                        (send plot :fit (not (send plot :fit)))
                        (send plot :redraw-lines)
                        (send plot :set-menu))))
         (q-lines (send menu-item-proto :new "Quantile Lines"
                 :action #'(lambda ()
                        (send plot :q-lines (not (send plot :q-lines)))
                        (send plot :redraw-lines)
                        (send plot :set-menu))))
         (hide-item (send menu-item-proto :new "Hide Plot"
                          :action #'(lambda () (send plot :hide-window)))))
    (call-next-method plot)
    (send self :append-items (send dash-item-proto :new) fit
         q-lines (send dash-item-proto :new) hide-item)))

