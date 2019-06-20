(defproto hist-data-proto '(null-value ci-level)
  '(instances) plot-data-proto "Histogram Model")

(defmeth hist-data-proto :new (dataset &rest args)
  (let ((hist-data (apply #'call-next-method dataset args)))
    (setf (slot-value 'instances)
          (cons hist-data (slot-value 'instances)))
    hist-data))

(defmeth hist-data-proto :isnew (dataset &rest args)
  (setf (slot-value 'name)
        (concatenate 'string
                     (format nil "Hist: ")
                     (format nil  "~s"
                             (send (first args) :name))))
  (setf (slot-value 'y-variables) (first args))
  (setf (slot-value 'null-value)
        (mean (send (first args) :values)))
  (setf (slot-value 'ci-level) 0.95)
  (call-next-method dataset args))

;;; Slot Accessors and Mutators

(defmeth hist-data-proto :null-value (&optional null-value)
  (if null-value (setf (slot-value 'null-value) null-value))
  (slot-value 'null-value))

(defmeth hist-data-proto :ci-level (&optional ci-level)
  (if ci-level (setf (slot-value 'ci-level) ci-level))
  (slot-value 'ci-level))

;;; Other Methods

(defmeth hist-data-proto :compute-moments (conditioning)
  (let* ((values (send self :get-variable-values 'Y conditioning))
         (n (length values))
         (mean (mean values))
         (std-dev (standard-deviation values))
         (std-err (/ std-dev (sqrt n)))
         (median (median values)))
    (list n mean std-dev std-err (/ (* 3 (- mean median)) std-dev))))

(defmeth hist-data-proto :compute-test (conditioning)
  (let* ((moments (send self :compute-moments conditioning))
         (n (first moments))
         (t-value (/ (- (second moments) (slot-value 'null-value))
                     (/ (third moments) (sqrt n))))
         (p (t-cdf t-value (- n 1))))
    (list t-value (* 2 (min (- 1 p) p)) (- 1 p) p)))

(defmeth hist-data-proto :compute-ci (conditioning)
  (let* ((moments (send self :compute-moments conditioning))
         (n (first moments))
         (ci-level (slot-value 'ci-level))
         (t-tab (t-quant (- 1 (/ (- 1 ci-level) 2)) (- n 1)))
         (lower-limit (- (second moments)
                         (* t-tab (/ (third moments) (sqrt n)))))
         (upper-limit (+ (second moments)
                         (* t-tab (/ (third moments) (sqrt n))))))
    (list lower-limit upper-limit)))

;;;;
;;;; Definition of hist-proto: inherits from plot proto
;;;;

(defproto hist-proto '(normal kernel kernel-width ci-menu)
  () (list plot-proto histogram-proto) "Histogram")

(defmeth hist-proto :isnew (&rest args)
  (let* ((hist (apply #'call-next-method args))
         (hist-data (slot-value 'dataset))
         (overlays (slot-value 'overlays))
         (y-var (send hist-data :y-variables))
         (y-values (send y-var :values))
         (y-name (format nil "~a" (send y-var :name)))
         (y-range (get-nice-range (min y-values) (max y-values) 10))
         (menu (send histogram-menu-proto :new hist))
         (ci-menu (send ci-menu-proto :new hist))
         (moments (send moment-overlay-proto :new :lines 6))
         (test (send test-overlay-proto :new :lines 6))
         (ci (send ci-overlay-proto :new :lines 4))
         (animate (send animate-button-overlay-proto :new 14)))
    (dolist (overlay overlays)
            (if (kind-of-p overlay menu-overlay-proto)
                (send overlay :string "Histogram"))
            (if (kind-of-p overlay label-button-overlay-proto)
                (send hist :delete-overlay overlay))
            (if (kind-of-p overlay symbol-button-overlay-proto)
                (send hist :delete-overlay overlay))
            (if (kind-of-p overlay color-button-overlay-proto)
                (send overlay :position 11)))
    (send hist :add-overlay moments)
    (send hist :add-overlay test)
    (send hist :add-overlay ci)
    (send hist :add-overlay animate)
    (send hist :variable-label '(0) (list y-name))
    (send hist :plot-menu menu)
    (send hist :ci-menu ci-menu)
    (send hist :summaries (list moments test ci))
    (send hist :range 0 (first y-range) (second y-range) :draw nil)
    (send hist :x-axis t t (third y-range) :draw nil)
    (send hist :kernel-width (/ (- (second y-range) (first y-range))
                                (send hist :num-bins)))
    (send hist :add-points y-values :draw nil)
    (send hist :adjust-to-data)
    hist))

;;;
;;; Constucts a histogram for each y variable
;;;

(defun make-histogram (y-var dataset)
  (let* ((hist-data (send hist-data-proto :new dataset y-var))
         (title (send hist-data :name))
         (histogram (send hist-proto :new 1
                          :title title :dataset hist-data
                          :location (list 20 42)
                          :size '(350 275) :show nil)))
    (send histogram :redraw-bottom)
    (send *data-tree* :register-dataset-with-plot
          dataset hist-data histogram)
    (send histogram :show-window)
    hist-data))

;;; Slot Accessors and Mutators

(defmeth hist-proto :normal (&optional (normal nil set-normal))
  (if set-normal (setf (slot-value 'normal) normal))
  (slot-value 'normal))

(defmeth hist-proto :kernel (&optional (kernel nil set-kernel))
  (if set-kernel (setf (slot-value 'kernel) kernel))
  (slot-value 'kernel))

(defmeth hist-proto :kernel-width (&optional width)
  (when width
        (setf (slot-value 'kernel-width) width)
        (send self :redraw-lines))
  (slot-value 'kernel-width))

(defmeth hist-proto :test-menu (&optional (test-menu nil set-test-menu))
  (if set-test-menu (setf (slot-value 'test-menu) test-menu))
  (slot-value 'test-menu))

(defmeth hist-proto :ci-menu (&optional (ci-menu nil set-ci-menu))
  (if set-ci-menu (setf (slot-value 'ci-menu) ci-menu))
  (slot-value 'ci-menu))

;;; Other Methods

(defmeth hist-proto :adjust-to-data ()
  (let* ((n (send self :num-points))
         (values (send self :point-coordinate 0 (iseq n)))
         (unique-values (send self :make-unique-values values))
         (min (min unique-values))
         (max (max unique-values))
         (num-bins (ceiling (+ 1 (* (/ 3 (log 8)) (log n)))))
         (ticks (if (<= (length unique-values) 12)
                    (+ (length unique-values) 1)
                    (+ num-bins 1)))
         (new-range (get-nice-range min max ticks)))
    (send self :range 0 (first new-range) (second new-range))
    (send self :num-bins (- (third new-range) 1))
    (send self :x-axis t t (third new-range))))

(defmeth hist-proto :make-unique-values (values)
  (let* ((unique-values nil))
    (dolist (val values)
            (if (not (member val unique-values))
                (setf unique-values (cons val unique-values))))
    (reverse unique-values)))

(defmeth hist-proto :redraw-lines ()
  (let ((summaries (slot-value 'summaries)))
    (send self :clear-lines :draw nil)
    (if (send self :normal) (send self :normal-density))
    (if (send self :kernel) (send self :kernel-density))
    (if (send (second summaries) :visible) (send self :t-bar))
    (if (send (third summaries) :visible) (send self :ci-band))
    (send self :redraw-content)))

(defmeth hist-proto :animate (direction)
  (pause 10)
  (let ((num-bins (send self :num-bins))
        (range (send self :range 0)))
    (if direction
        (if (< num-bins 30) (send self :num-bins (+ num-bins 1) :draw nil))
        (if (> num-bins 2) (send self :num-bins (- num-bins 1) :draw nil)))
    (if (send self :kernel)
        (send self :kernel-width (/ (- (second range) (first range))
                                    (send self :num-bins))))
    (send self :redraw-content)))

(defmeth hist-proto :t-bar ()
  (let* ((dataset (send self :dataset))
         (null-value (send dataset :null-value))
         (num-points (send self :num-points))
         (values (send self :point-coordinate 0 (iseq num-points)))
         (s (standard-deviation values))
         (y-max (/ (normal-dens 0) s)))
    (send self :add-lines
          (list (list null-value null-value) (list 0 y-max)))
    (send self :linestart-width
          (iseq (- (send self :num-lines) 2) (- (send self :num-lines) 1)) 2)
    (send self :linestart-color
          (iseq (- (send self :num-lines) 2)
                (- (send self :num-lines) 1)) 'green)))

(defmeth hist-proto :ci-band ()
  (let* ((dataset (send self :dataset))
         (z-vars (send dataset :z-variables))
         (ci (send dataset :compute-ci z-vars))
         (position (- (second (send self :content-origin)) 10))
         (height (second (send self :canvas-to-real 0 position)))
         (lower-limit (first ci))
         (upper-limit (second ci))
         (upper-canvas-limit
          (- (first (send self :real-to-canvas upper-limit 0)) 10))
         (upper-limit
          (first (send self :canvas-to-real upper-canvas-limit 0)))
         (mean (/ (+ lower-limit upper-limit) 2)))
    (send self :add-lines
          (list (list lower-limit upper-limit) (list height height)))
    (send self :linestart-width
          (iseq (- (send self :num-lines) 2) (- (send self :num-lines) 1)) 10)
    (send self :linestart-color
          (iseq (- (send self :num-lines) 2)
                (- (send self :num-lines) 1)) 'red)))

(defmeth hist-proto :normal-density ()
  (let* ((dataset (slot-value 'dataset))
         (mask (send dataset :mask))
         (z-mask (send dataset :z-mask))
         (or-mask (map-elements #'vector-or mask z-mask))
         (not-mask (mapcar #'(lambda (val) (not val)) or-mask))
         (var (send dataset :y-variables))
         (values (select (send var :values) (which not-mask)))
         (num-points (length values))
         (num-lines (send self :num-lines))
         (range (send self :range 0))
         (color (send self :draw-color))
         (x-bar (mean values))
         (s (standard-deviation values))
         (x (rseq (- x-bar (* 3 s)) (+ x-bar (* 3 s)) 30))
         (y (/ (normal-dens (/ (- x x-bar) s)) s))
         (y-max (/ (normal-dens 0) s))
         (y-inflection (/ (normal-dens 1) s)))
    (send self :add-lines (list x y) :draw nil)
    (send self :add-lines (list (list x-bar x-bar) (list 0 y-max)) :draw nil)
    (send self :linestart-color (iseq num-lines (+ num-lines 30)) 'red)
    (when (> (- x-bar s) (first range))
          (send self :add-lines (list (list (- x-bar s) (- x-bar s))
                                      (list 0 y-inflection)) :draw nil)
          (send self :linestart-color
                (iseq (- (send self :num-lines) 2)
                      (- (send self :num-lines) 1)) 'red))
    (when (< (+ x-bar s) (second range))
          (send self :add-lines (list (list (+ x-bar s) (+ x-bar s))
                                      (list 0 y-inflection)) :draw nil)
          (send self :linestart-color
                (iseq (- (send self :num-lines) 2)
                      (- (send self :num-lines) 1)) 'red))))

(defmeth hist-proto :kernel-density ()
  (let* ((dataset (slot-value 'dataset))
         (mask (send dataset :mask))
         (z-mask (send dataset :z-mask))
         (or-mask (map-elements #'vector-or mask z-mask))
         (not-mask (mapcar #'(lambda (val) (not val)) or-mask))
         (y-var (send dataset :y-variables))
         (y-values (select (send y-var :values) (which not-mask)))
         (num-points (length y-values))
         (width (send self :kernel-width))
         (num-lines (send self :num-lines)))
    (send self :add-lines (kernel-dens y-values :width width :type 'b))
    (send self :linestart-color (iseq num-lines (+ num-lines 29))
          'blue)))

;; Histogram controls

;; Mark menu items

(defmeth hist-proto :set-menu ()
  (let* ((plot-menu (send self :plot-menu))
         (items (send plot-menu :items)))
    (send (select items 6) :mark (send self :normal))
    (send (select items 7) :mark (send self :kernel))))

;;
;; Definition of histogram-menu-proto: inherits from plot-menu-proto
;;

(defproto histogram-menu-proto '() nil plot-menu-proto "Histogram Menu")

(defmeth histogram-menu-proto :isnew (plot)
  (let* ((dataset (send plot :dataset))
         (y-var (send dataset :y-variables))
         (normal-density (send menu-item-proto :new "Normal Density"
                               :action #'(lambda ()
                      (send plot :normal (not (send plot :normal)))
                      (send plot :redraw-lines)
                      (send plot :set-menu))))
         (kernel-density (send menu-item-proto :new "Kernel Density"
                               :action #'(lambda ()
                      (send plot :kernel (not (send plot :kernel)))
                      (send plot :redraw-lines)
                      (send plot :set-menu))))
         (normal-plot (send menu-item-proto :new "normalPlot"
                               :action #'(lambda ()
                                           (make-quantplot y-var dataset))))
         (hide-item (send menu-item-proto :new "Hide Plot"
                          :action #'(lambda () (send plot :hide-window)))))
    (call-next-method plot)
    (send self :append-items (send dash-item-proto :new) normal-density
          kernel-density (send dash-item-proto :new) normal-plot
          (send dash-item-proto :new)hide-item)))

;;;
;;;  ci-menu-proto:
;;;      inherits from menu-proto and adds the slot 'plot'.
;;;

(defproto ci-menu-proto '(plot) nil menu-proto "CI")

;;;
;;;  Initilization method for ci-menu-proto
;;;

(defmeth ci-menu-proto :isnew (plot)
  (setf (slot-value 'plot) plot)
  (let* ((dataset (send plot :dataset))
         (ci90 (send menu-item-proto :new "90%"
                     :action #'(lambda ()
                                 (send dataset :ci-level 0.90)
                                 (send plot :redraw-overlays)
                                 (send plot :redraw-content))))
         (ci95 (send menu-item-proto :new "95%"
                     :action #'(lambda ()
                                 (send dataset :ci-level 0.95)
                                 (send plot :redraw-overlays)
                                 (send plot :redraw-content))))
         (ci99 (send menu-item-proto :new "99%"
                     :action #'(lambda ()
                                 (send dataset :ci-level 0.99)
                                 (send plot :redraw-overlays)
                                 (send plot :redraw-content))))
         (other (send menu-item-proto :new "Other ..."
                      :action #'(lambda ()
                      (let ((ci-level (first (get-value-dialog
                             "Enter a confidence level between 0.5 and 1"))))
                        (if (< 0.5 ci-level 1)
                            (progn
                             (send dataset :ci-level ci-level)
                             (send plot :redraw-overlays)
                             (send plot :redraw-content))
                            (message-dialog "Confidence level is not
                                             between 0.5 and 1")))))))
    (call-next-method "CI")
    (send self
          :append-items ci90 ci95 ci99 other)))

