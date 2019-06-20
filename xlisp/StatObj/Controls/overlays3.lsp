(defproto slider-overlay-proto
  '(label position dimensions value range max min delta click) nil
  graph-overlay-proto)

(defmeth slider-overlay-proto :isnew
  (label position dimensions value range max min delta)
  (send self :label label)
  (send self :position position)
  (send self :dimensions dimensions)
  (send self :value value)
  (send self :range range)
  (send self :max max)
  (send self :min min)
  (send self :delta delta)
  (send self :click 0))

(defmeth slider-overlay-proto :label (&optional set-label)
  (if set-label (setf (slot-value 'label) set-label))
  (slot-value 'label))

(defmeth slider-overlay-proto :position (&optional set-position)
  (if set-position (setf (slot-value 'position) set-position))
  (slot-value 'position))

(defmeth slider-overlay-proto :dimensions (&optional set-dimensions)
  (if set-dimensions (setf (slot-value 'dimensions) set-dimensions))
  (slot-value 'dimensions))

(defmeth slider-overlay-proto :range (&optional set-range)
  (if set-range (setf (slot-value 'range) set-range))
  (slot-value 'range))

(defmeth slider-overlay-proto :click (&optional set-click)
  (if set-click (setf (slot-value 'click) set-click))
  (slot-value 'click))

(defmeth slider-overlay-proto :value (&optional set-value)
  (if set-value (setf (slot-value 'value) set-value))
  (slot-value 'value))

(defmeth slider-overlay-proto :max (&optional set-max)
  (if set-max (setf (slot-value 'max) set-max))
  (slot-value 'max))

(defmeth slider-overlay-proto :min (&optional set-min)
  (if set-min (setf (slot-value 'min) set-min))
  (slot-value 'min))

(defmeth slider-overlay-proto :delta (&optional set-delta)
  (if set-delta (setf (slot-value 'delta) set-delta))
  (slot-value 'delta))


(defmeth slider-overlay-proto :redraw ()
  (let* ((plot (send self :graph)))
    (send self :draw-frame plot)
    (send self :draw-indicator plot)
    (send self :draw-label plot)
    (send self :draw-value plot)))


(defmeth slider-overlay-proto :draw-frame (plot)
  (let* ((x-pos (first (send self :position)))
         (y-pos (second (send self :position)))
         (width (first (send self :dimensions)))
         (height (second (send self :dimensions))))
    (send plot :erase-rect x-pos y-pos width height)
    (send plot :frame-rect x-pos y-pos width height)))

(defmeth slider-overlay-proto :draw-label (plot)
  (let* ((x-pos (- (first (send self :position)) 9))
         (y-pos (+ 8 (second (send self :position)))))
    (send plot :draw-string (send self :label) x-pos y-pos)))

(defmeth slider-overlay-proto :draw-value (plot)
  (let* ((x-pos (+ (first (send self :position))
                   (first (send self :dimensions)) 2))
         (y-pos (+ 8 (second (send self :position)))))
    (send plot
          :draw-string
          (format nil "~,2f" (* (send self :value)
                                (send self :delta))) x-pos y-pos)))

(defmeth slider-overlay-proto :erase-value (plot)
  (let* ((x-pos (+ (first (send self :position))
                   (first (send self :dimensions)) 2))
         (y-pos (+ 8 (second (send self :position)))))
    (send plot :erase-rect x-pos (- y-pos 10) 30 10)))


(defmeth slider-overlay-proto :slider-to-graph-coord (slider-coord)
  (let* ((max (send self :max))
         (min (send self :min))
         (left (first (send self :position)))
         (right (+ left (first (send self :dimensions))))
         (slope (/ (+ (- left right) 4) (- min max)))
         (intercept (- (+ left 2) (* slope min))))
    (floor (+ (* slope slider-coord) intercept (- 2)))))

(defmeth slider-overlay-proto :graph-to-slider-coord (graph-coord)
  (let* ((max (send self :max))
         (min (send self :min))
         (left (first (send self :position)))
         (right (+ left (first (send self :dimensions))))
         (slope (/ (+ (- left right) 4) (- min max)))
         (intercept (- (+ left 2) (* slope min))))
    (/ (+ graph-coord 2 (- intercept)) slope)))

(defmeth slider-overlay-proto :draw-indicator (plot)
  (let* ((indicator-right (send self :indicator-right))
         (indicator-left (- indicator-right 4))
         (indicator-width 5)
         (indicator-height (+ (second (send self :dimensions)) 6))
         (indicator-top (- (second (send self :position)) 3)))
    (send plot :paint-rect indicator-left indicator-top
          indicator-width indicator-height)))

(defmeth slider-overlay-proto :erase-indicator (plot)
  (let* ((indicator-right (send self :indicator-right))
         (indicator-left (- indicator-right 4))
         (indicator-width 5)
         (indicator-height (+ (second (send self :dimensions)) 6))
         (indicator-top (- (second (send self :position)) 3)))
    (send plot :erase-rect indicator-left indicator-top
          indicator-width indicator-height)))

(defmeth slider-overlay-proto :do-click ( x y mod1 mod2)
  (when (send self :in-slider x y)
        (send self :handle-click x y mod1 mod2) t))

(defmeth slider-overlay-proto :handle-click (x y mod1 mod2)
  (cond ((send self :in-indicator x y)
         (send self :handle-indicator-click x y mod1 mod2))
        ((send self :right-of-indicator x y)
         (send self :handle-right-click x y mod1 mod2))
        ((send self :left-of-indicator x y)
         (send self :handle-left-click x y mod1 mod2))))

(defmeth slider-overlay-proto :in-slider ( x y )
  (let* ((x-pos (first (send self :position)))
         (y-pos (second (send self :position)))
         (width (first (send self :dimensions)))
         (height (second (send self :dimensions))))
    (and (> x x-pos) (> y y-pos)
         (< x (+ x-pos width)) (< y (+ y-pos height)))))

(defmeth slider-overlay-proto :indicator-right ()
  (- (+ (first (send self :position)) (* (send self :value) 5)) 1))

(defmeth slider-overlay-proto :in-indicator (x y)
  (let* ((indicator-right (send self :indicator-right))
         (indicator-left (- indicator-right 4))
         (indicator-height (+ (second (send self :dimensions)) 6))
         (indicator-top (- (second (send self :position)) 3)))
    (and (> x indicator-left) (> y indicator-top)
         (< x indicator-right) (< y (+ indicator-top indicator-height)))))

(defmeth slider-overlay-proto :right-of-indicator (x y)
  (let* ((indicator-right (send self :indicator-right))
         (indicator-top (- (second (send self :position)) 3))
         (indicator-height (+ (second (send self :dimensions)) 6))
         (slider-right (+ (first (send self :position))
                          (first (send self :dimensions)))))
    (and (> x indicator-right) (> y indicator-top)
         (< x slider-right) (< y (+ indicator-top indicator-height)))))

(defmeth slider-overlay-proto :left-of-indicator (x y)
  (let* ((indicator-right (send self :indicator-right))
         (indicator-left (- indicator-right 4))
         (indicator-height (+ (second (send self :dimensions)) 6))
         (indicator-top (- (second (send self :position)) 3))
         (slider-left (first (send self :position))))
    (and (> x slider-left) (> y indicator-top) (< x indicator-left)
         (< y (+ indicator-top indicator-height)))))

(defmeth slider-overlay-proto :handle-right-click (x y mod1 mod2)
  (let* ((plot (send self :graph)))
    (send plot :while-button-down
          #'(lambda (x y) (pause 5)
              (if (send self :right-of-indicator x y)
                  (send self :move-indicator-right x y mod1 mod2 1))) nil)))

(defmeth slider-overlay-proto :handle-left-click (x y mod1 mod2)
  (let* ((plot (send self :graph)))
    (send plot :while-button-down
          #'(lambda (x y) (pause 5)
              (if (send self :left-of-indicator x y)
                  (send self :move-indicator-left x y mod1 mod2 1))) nil)))

(defmeth slider-overlay-proto :handle-indicator-click (x y mod1 mod2)
(let* ((prev-click (slot-value 'click))
       (current-click (get-internal-real-time)))
  (send self :click current-click)
  (if (<= (- current-click prev-click) (double-click-time))
      (send self :show-parameters-dialog)
      (cond
        ((and (not mod1) (not mod2)) 
         (send self :handle-no-mod-indicator x)) 
        ((and mod1 (not mod2)) 
         (send self :handle-mod1-indicator))
        ((and (not mod1) mod2) 
         (send self :handle-mod2-indicator))
        ((and mod1 mod2) 
         (send self :handle-mod1-mod2-indicator))))))

(defmeth slider-overlay-proto :show-parameters-dialog ()
  (let* ((plot (send self :graph))
         (dataset (send plot :dataset))
         (x-var (send (send dataset :x-variables) :values))
         (x-max (max x-var))
         (x-min (min x-var))
         (x-range (- x-max x-min))
         (iterations (select (send plot :smooth-parameters) 1))
         (cutoff (* (/ (select (send plot :smooth-parameters) 2)
                       x-range) 100))
         (iterations (format nil "~d" iterations))
         (cutoff (format nil "~,2f" cutoff)) 
         (iterations-static-text (send text-item-proto :new "Iterations"))
         (cutoff-static-text (send text-item-proto :new "Cutoff %"))
         (name-static-text (send text-item-proto :new "Lowess Parameters"))
         (blank-static-text (send text-item-proto :new "" ))
         (cancel-button (send modal-button-proto :new "Cancel"
                              :action #'cancel-smoothing-values))
         (ok-button (send modal-button-proto :new "OK"
                          :action #'collect-smoothing-values))
         (default-button (send modal-button-proto :new "Default"
                               :action #'set-default-smoothing-values)))
    (setf iterations-edit-text (send edit-text-item-proto
                                     :new iterations :text-length 1))
    (setf cutoff-edit-text (send edit-text-item-proto
                                 :new cutoff :text-length 2))
    (setf items-list
          (list
           (list
            (list name-static-text iterations-static-text cutoff-static-text)
            (list blank-static-text iterations-edit-text cutoff-edit-text) 
            (list default-button cancel-button ok-button))))
    (setf smooth-plot plot)
    (setf smoothing-dialog (send modal-dialog-proto
                                 :new items-list :default-button ok-button))
    (do ((reply 3 (send smoothing-dialog :modal-dialog nil)))
        ((not (= reply 3)) (send plot :remove-smoothing-line)
         (send plot :add-smoothing-line)))))

(defun collect-smoothing-values ()
  (let* ((iterations (with-input-from-string
                      (s (send iterations-edit-text :text)) (read s)))
         (cutoff (/ (with-input-from-string
                     (s (send cutoff-edit-text :text)) (read s)) 100))
         (dataset (send smooth-plot :dataset))
         (x-var (send (send dataset :x-variables) :values))
         (x-max (max x-var))
         (x-min (min x-var))
         (x-range (- x-max x-min)))
    (setf (select (send smooth-plot :smooth-parameters) 1) iterations)
    (setf (select (send smooth-plot :smooth-parameters) 2) (* cutoff x-range))
    (send smoothing-dialog :remove)
    (send smoothing-dialog :modal-dialog-return 1)))

(defun cancel-smoothing-values ()
  (send smoothing-dialog :remove)
  (send smoothing-dialog :modal-dialog-return 2))

(defun set-default-smoothing-values ()
  (send iterations-edit-text :text "2")
  (send cutoff-edit-text :text "2")
  (send smoothing-dialog :modal-dialog-return 3))


(defmeth slider-overlay-proto :handle-no-mod-indicator (x-start)
(let* ((plot (send self :graph))
       (prev-x x-start))
  (send plot :while-button-down 
        #'(lambda (x y)
            (cond 
              ((and (< prev-x x) (send self :right-of-indicator x y)
                    (send self :in-slider x y)) 
               (send self :move-indicator-right x y nil nil 0))
              ((and (> prev-x x) (send self :left-of-indicator x y)
                    (send self :in-slider x y))
               (send self :move-indicator-left x y nil nil 0)))
            (send self :draw-indicator plot)
            (send self :draw-value plot)
            (setf prev-x x)))
  (send plot :remove-smoothing-line)
  (send plot :add-smoothing-line)))
  
 
(defmeth slider-overlay-proto :move-indicator-right (x y mod1 mod2 update)
  (let* ((plot (send self :graph)))
    (cond
      ((and (not mod1) (not mod2)) 
       (send self :handle-no-mod-right)) 
      ((and mod1 (not mod2)) 
       (send self :handle-mod1-right))
      ((and (not mod1) mod2) 
       (send self :handle-mod2-right))
      ((and mod1 mod2) 
       (send self :handle-mod1-mod2-right)))
    (setf (select (send plot :smooth-parameters) 0)
          (* (send self :value) (send self :delta)))
    (cond 
      ((= update 1)
       (send plot :remove-smoothing-line)
       (send plot :add-smoothing-line)))))

(defmeth slider-overlay-proto :handle-no-mod-right ()
  (let* ((plot (send self :graph)))
    (send self :erase-indicator plot)
    (send self :erase-value plot) 
    (send self :value (+ (send self :value) 1))
    (send self :draw-frame plot)
    (send self :draw-indicator plot)
    (send self :draw-value plot)))

(defmeth slider-overlay-proto :move-indicator-left (x y mod1 mod2 update)
  (let* ((plot (send self :graph)))
    (cond 
      ((and (not mod1) (not mod2)) 
       (send self :handle-no-mod-left))
      ((and mod1 (not mod2)) 
       (send self :handle-mod1-left))
      ((and (not mod1) mod2) 
       (send self :handle-mod2-left))
      ((and mod1 mod2) 
       (send self :handle-mod1-mod2-left)))
    (setf (select (send plot :smooth-parameters) 0)
          (* (send self :value) (send self :delta)))
    (cond ((= update 1)
           (send plot :remove-smoothing-line)
           (send plot :add-smoothing-line)))))

(defmeth slider-overlay-proto :handle-no-mod-left ()
  (let* ((plot (send self :graph)))
    (send self :erase-indicator plot)
    (send self :erase-value plot) 
    (send self :value (- (send self :value) 1))
    (send self :draw-frame plot)
    (send self :draw-indicator plot)
    (send self :draw-value plot)))

(defproto kernel-overlay-proto '() () slider-overlay-proto)

(defmeth kernel-overlay-proto :indicator-right ()
  (ceiling (- (+ (first (send self :position))
                 (+ 5 (* 2.5 (- (send self :value) 1)))) 1)))


(defmeth kernel-overlay-proto :show-parameters-dialog ()
  (let* ((plot (send self :graph))
         (num-points (select (send plot :smooth-parameters) 1))
         (width (select (send plot :smooth-parameters) 2))
         (num-points (format nil "~d" num-points))
         (width (format nil "~,2f" width)) 
         (num-points-static-text (send text-item-proto :new "# Points"))
         (width-static-text (send text-item-proto :new "Method"))
         (name-static-text (send text-item-proto :new "Kernel Parameters"))
         (blank-static-text (send text-item-proto :new "" ))
         (cancel-button (send modal-button-proto
                              :new "Cancel" :action #'cancel-kernel-values))
         (ok-button (send modal-button-proto
                          :new "OK" :action #'collect-kernel-values))
         (default-button (send modal-button-proto :new "Default"
                               :action #'set-default-kernel-values)))
    (setf num-points-edit-text (send edit-text-item-proto
                                        :new num-points :text-length 1))
    (setf width-edit-text (send edit-text-item-proto
                                   :new width :text-length 2))
    (setf items-list (list
                      (list (list name-static-text
                                  num-points-static-text width-static-text)
                            (list blank-static-text
                                  num-points-edit-text width-edit-text) 
                            (list default-button cancel-button ok-button))))
    (setf smooth-plot plot)
    (setf smoothing-dialog (send modal-dialog-proto :new items-list
                                 :default-button ok-button))
    (do ((reply 3 (send smoothing-dialog :modal-dialog nil)))
        ((not (= reply 3)) (send plot :remove-smoothing-line)
         (send plot :add-smoothing-line)))))

(defun collect-kernel-values ()
  (let* ((num-points (with-input-from-string
                      (s (send num-points-edit-text :text)) (read s)))
         (method (with-input-from-string
                  (s (send width-edit-text :text)) (read s)))
         (dataset (send smooth-plot :dataset))
         (x-var (send (send dataset :x-variables) :values))
         (x-max (max x-var))
         (x-min (min x-var))
         (x-range (- x-max x-min)))
    (setf (select (send smooth-plot :smooth-parameters) 1) num-points)
    (setf (select (send smooth-plot :smooth-parameters) 2) method)
    (send smoothing-dialog :remove)
    (send smoothing-dialog :modal-dialog-return 1)))

(defun cancel-kernel-values ()
  (send smoothing-dialog :remove)
  (send smoothing-dialog :modal-dialog-return 2))

(defun set-default-kernel-values ()
  (let* ((dataset (send smooth-plot :dataset))
         (x-var (send (send dataset :x-variables) :values))
         (x-max (max x-var))
         (x-min (min x-var))
         (x-range (- x-max x-min)))
    (send num-points-edit-text :text "30")
    (send width-edit-text :text "G")
    (send smoothing-dialog :modal-dialog-return 3)))

(defproto p-slider-overlay-proto '() () slider-overlay-proto)

(defmeth p-slider-overlay-proto :indicator-right ()
  (ceiling (+ (first (send self :position))
              (+ (* (/ 45 29) (send self :value)) 5))))

(defmeth p-slider-overlay-proto :draw-value (plot)
  (let* ((x-pos (+ (first (send self :position))
                   (first (send self :dimensions)) 2))
         (y-pos (+ 8 (second (send self :position)))))
    (send plot :draw-string
          (format nil "~,2f" (- (* (send self :value)
                                   (send self :delta)) 1)) x-pos y-pos)))


(defmeth p-slider-overlay-proto :show-parameters-dialog ()
  nil)

(defmeth p-slider-overlay-proto :move-indicator-left (x y mod1 mod2 update)
  (let* ((plot (send self :graph)))
    (cond 
      ((and (not mod1) (not mod2)) 
       (send self :handle-no-mod-left))
      ((and mod1 (not mod2)) 
       (send self :handle-mod1-left))
      ((and (not mod1) mod2) 
       (send self :handle-mod2-left))
      ((and mod1 mod2) 
       (send self :handle-mod1-mod2-left)))
    (let ((p (/ (round (* (- (send (send plot :dataset) :p) .1) 10)) 10)))
      (if 'T
          (progn
           (send (send plot :dataset) :bc-transform p)
           (if (> update 0) (send plot :redraw)))))))

(defmeth p-slider-overlay-proto :handle-no-mod-indicator (x-start)
  (let* ((plot (send self :graph))
         (prev-x x-start))
    (send plot :while-button-down 
          #'(lambda (x y)
              (cond 
                ((and (< prev-x x) (send self :right-of-indicator x y)
                      (send self :in-slider x y)) 
                 (send self :move-indicator-right x y nil nil 0))
                ((and (> prev-x x) (send self :left-of-indicator x y)
                      (send self :in-slider x y))
                 (send self :move-indicator-left x y nil nil 0)))
              (send self :draw-indicator plot)
              (send self :draw-value plot)
              (setf prev-x x)))
    (send plot :redraw)))
  
 
(defmeth p-slider-overlay-proto :move-indicator-right (x y mod1 mod2 update)
  (let* ((plot (send self :graph)))
    (cond
      ((and (not mod1) (not mod2)) 
       (send self :handle-no-mod-right)) 
      ((and mod1 (not mod2)) 
       (send self :handle-mod1-right))
      ((and (not mod1) mod2) 
       (send self :handle-mod2-right))
      ((and mod1 mod2) 
       (send self :handle-mod1-mod2-right)))
    (let ((p (/ (round (* (+ (send (send plot :dataset) :p) .1) 10)) 10)))
      (if 'T
          (progn
           (send (send plot :dataset) :bc-transform p)
           (if (> update 0) (send plot :redraw)))))))



    

