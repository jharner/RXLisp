;;;;
;;;;  This file contains two prototypes: the data-table-proto,
;;;;    a view for datasets created from dataset-proto, and the
;;;;    data-table-subview-proto for subviews of the data browser,
;;;;    representing variables.
;;;;

;;;;                                                                    
;;;; Data Table Prototype: Provides a spreadsheet view of a dataset prototype                                                            ;;;;

(defproto data-table-proto '(subviews icon-view) '(instances)
  data-browser-proto)

(defmeth data-table-proto :new (dataset icon-view &rest args)
  (let ((data-table (apply #'call-next-method dataset icon-view args)))
    (setf (slot-value 'instances)
          (cons data-table (slot-value 'instances)))
    data-table))

(defmeth data-table-proto :isnew (dataset icon-view &rest args)
  (let ((variables (send dataset :variables))
        (browser nil)
        (subviews nil)
        (position 1))
    (dolist (var variables)
            (setf subviews
                  (cons (send data-table-subview-proto :new :variable var
                              :position position :data-table self)
                        subviews))
            (setf position (+ 1 position)))
    (setf (slot-value 'subviews) (reverse subviews))
    (apply #'call-next-method dataset icon-view browser args)))

;;;
;;; Constructor and Display Function
;;;

(defun display-data-table (dataset icon-view)
  (let* ((size (send *data-tree* :size))
         (location (send *data-tree* :location))
         (num-views (send (slot-value 'dataset) :num-var))
         (num-obs (send (slot-value 'dataset) :num-obs))
         (width (+ (* num-views 80) 80))
         (height (+ (* num-obs 16) 14))
         (h-position (send icon-view :h-position))
         (v-position (send icon-view :v-position))
         (start-horizontal (+ (car location) (car size) h-position))
         (start-vertical (- (+ (cadr location) v-position) 4))
         (table-browser (send data-table-proto :new dataset icon-view
                             :location (list start-horizontal start-vertical)
                             :size (list 320 (min 176 height))
                             :title (concatenate
                                     'string (format nil "~s Data"
                                                     (send dataset :name))))))
    (send table-browser :has-h-scroll width)
    (send table-browser :has-v-scroll height)
    (send table-browser :h-scroll-incs 80 400)
    (send table-browser :v-scroll-incs 16 80)
    (send table-browser :use-color t)
    (send table-browser :redraw)
    table-browser))

;;;
;;; Other Methods                   
;;;

(defmeth data-table-proto :redraw ()
  (let* ((dataset (slot-value 'dataset))
         (subviews (slot-value 'subviews))
         (view-rect (send self :view-rect))
         (x-delta (first view-rect))
         (y-delta (second view-rect))
         (first-obs (+ (floor (/ (- y-delta 16) 16)) 1))
         (last-obs (+ (floor (/ (- (fourth view-rect) 14) 16)) first-obs))
         (first-view (floor (/ (- x-delta 80) 80)))
         (num-views (send dataset :num-var))
         (obs (send dataset :num-obs))
         (width (+ x-delta (third view-rect)))
         (height (+ y-delta (fourth view-rect)))
         (label-variable (send dataset :label-variable))
         (values (if label-variable
                     (send (eval label-variable) :values)
                     (iseq 1 obs))))
    (send self :start-buffering)
    (send self :erase-rect 0 0 width height)
    (send self :draw-line
          (+ x-delta 12) (+ y-delta 3) (+ x-delta 18) (+ y-delta 3))
    (send self :draw-line
          (+ x-delta 12) (+ y-delta 6) (+ x-delta 18) (+ y-delta 6))
    (send self :draw-line
          (+ x-delta 12) (+ y-delta 9) (+ x-delta 15) (+ y-delta 9))
    (send self :draw-line
          (+ x-delta 32) (+ y-delta 3) (+ x-delta 38) (+ y-delta 3))
    (send self :draw-line
          (+ x-delta 32) (+ y-delta 9) (+ x-delta 38) (+ y-delta 9))
    (send self :draw-line
          (+ x-delta 62) (+ y-delta 3) (+ x-delta 68) (+ y-delta 3))
    (send self :draw-line
          (+ x-delta 62) (+ y-delta 6) (+ x-delta 68) (+ y-delta 6))
    (send self :draw-line
          (+ x-delta 62) (+ y-delta 9) (+ x-delta 68) (+ y-delta 9))
    (send self :line-width 2)
    (send self :draw-line 0 (+ y-delta 14) width (+ y-delta 14))
    (send self :draw-line (+ x-delta 80) 0 (+ x-delta 80) height)
    (send self :line-width 1)
    (dotimes (i obs nil)
             (if (<= first-obs i last-obs)
                 (progn
                  (let ((position (+ (* (+ i 1) 16) 16)))
                    (send self
                          :draw-text (format nil "~,3d" (select values i))
                          (+ x-delta 6) (- position 4) 0 0)
                    (send self :draw-line x-delta position
                          (+ x-delta 80) position)))))
    (dotimes (i num-views)
             (if (> i first-view) (send (select subviews i) :redraw)))
    (send self :buffer-to-screen 0 0 width height)))

;;;
;;; The main controller for the datatable: receives and processes
;;;     all the mouse clicks on thedata-table
;;;

(defmeth data-table-proto :do-click (x y m1 m2)
  (let* ((dataset (slot-value 'dataset))
         (view-rect (send self :view-rect))
         (x-delta (first view-rect))
         (y-delta (second view-rect))
         (width (+ x-delta (third view-rect)))
         (height (+ y-delta (fourth view-rect)))
         (data-tree-view (send (slot-value 'icon-view) :data-tree-view))
         (view-list (send data-tree-view :get-all-views dataset))
         (subviews (slot-value 'subviews)))
    (send data-tree-view :focus dataset)
    (send data-tree-view :redraw)
    (cond
      ((and (<= (+ x-delta 12) x (+ x-delta 18)) (<= y (+ y-delta 14)))
       (send (send dataset-menu-proto :new (slot-value 'icon-view))
             :popup (+ x-delta 12) y-delta  self))
      ((and (<= (+ x-delta 32) x (+ x-delta 38)) (<= y (+ y-delta 14)))
       (if (eq m2 t)
           (send (send stat-options-menu-proto :new self)
                     :popup (+ x-delta 32) y-delta self)
           (send (send stat-menu-proto :new self)
                     :popup (+ x-delta 32) y-delta self)))
      ((and (<= (+ x-delta 62) x (+ x-delta 68)) (<= y (+ y-delta 14)))
       (progn
        (let ((browser
               (dolist (view view-list)
                       (if  (kind-of-p view data-browser-proto)
                            (return view)))))
          (if browser
              (send browser :show-window)
              (send *data-tree* :register-dataset-view dataset
                    (display-data-browser dataset (slot-value 'icon-view)))))
        (send self :hide-window)))
      (t
       (dolist (subview subviews)
               (if (send subview :in-subview x y)
                   (send  subview :do-click x y m1 m2)))))
    (send self :redraw)))

;;;;                                                                       
;;;; Data Browser Subview Prototype: an iconic view for variables                                                           ;;;;

(defproto data-table-subview-proto '(data-table)
  nil data-browser-subview-proto)

;;;
;;; Other Methods                   
;;;

(defmeth data-table-subview-proto :in-subview (x y)
  (let ((x-delta (* (slot-value 'position) 80)))
    (and (<= x-delta x (+ x-delta 80)))))

(defmeth data-table-subview-proto :redraw ()
  (let* ((variable (slot-value 'variable))
         (data-table (slot-value 'data-table))
         (view-rect (send data-table :view-rect))
         (x-delta (* (slot-value 'position) 80))
         (y-delta (second view-rect))
         (width (+ x-delta (third view-rect)))
         (height (+ y-delta (fourth view-rect)))
         (first-obs (+ (floor (/ (- y-delta 16) 16)) 1))
         (last-obs (+ (floor (/ (- (fourth view-rect) 14) 16)) first-obs))
         (role (send (eval variable) :role))
         (type (send (eval variable) :type))
         (name (send (eval variable) :name))
         (obs (send (eval variable) :data-length))
         (values (send (eval variable) :values))
         (justified (if (eql type 'C) 0 2))
         (start (if (eql justified 2) 76 6)))
    (send data-table :draw-string (format nil "~s" name)
          (+ x-delta 32) (+ y-delta 11))
    (if (eql type 'C) (send data-table :line-type 'dashed))
    (if (not (or (eql role 'L) (eql role 'W)))
         (progn
          (send data-table :draw-line
                (+ x-delta 16) (+ y-delta 8) (+ x-delta 23) (+ y-delta 8))
          (send data-table :draw-line
                (+ x-delta 16) (+ y-delta 1) (+ x-delta 16) (+ y-delta 7))
          (send data-table :draw-line
                (+ x-delta 16) (+ y-delta 8) (+ x-delta 12) (+ y-delta 12))))
    (send data-table :line-width 2)
    (send data-table :draw-color 'red)
    (case role
       ('L (send data-table :frame-rect (+ x-delta 12) (+ y-delta 4) 10 7))
       ('X (send data-table :draw-line
                 (+ x-delta 16) (+ y-delta 8) (+ x-delta 22) (+ y-delta 8)))
       ('Y (send data-table :draw-line
                 (+ x-delta 16) (+ y-delta 1) (+ x-delta 16) (+ y-delta 7)))
       ('Z (send data-table :draw-line
                 (+ x-delta 15) (+ y-delta 8) (+ x-delta 11) (+ y-delta 12))))
    (send data-table :line-width 1)
    (send data-table :draw-color 'black)
    (send data-table :line-type 'solid)
    (send data-table :draw-line
          (+ x-delta 80) y-delta (+ x-delta 80) height)
    (send data-table :line-type 'dashed)
    (dotimes (i obs nil)
             (if (<= first-obs i last-obs)
                 (progn
                  (let ((position (+ (* (+ i 1) 16) 16)))
                    (send data-table
                          :draw-text (format nil "~,4g" (select values i))
                          (+ x-delta start) (- position 4) justified 0)
                    (send data-table :draw-line x-delta position
                          (+ x-delta 80) position)))))
    (send data-table :line-type 'solid)
    (send data-table :draw-line
          (+ x-delta 80) y-delta (+ x-delta 80) height)))

(defmeth data-table-subview-proto :do-click (x y m1 m2)
  (let* ((data-table (slot-value 'data-table))
         (view-rect (send data-table :view-rect))
         (x-delta (* (slot-value 'position) 80))
         (y-delta (second view-rect))
         (dataset (send data-table :dataset))
         (variable (slot-value 'variable))
         (type (send (eval variable) :type)))
    (if (<= (+ x-delta 10) x (+ x-delta 24))
        (if (eq m2 t)
            (send (send type-menu-proto :new self)
                  :popup (+ x-delta 10) y-delta data-table)
            (cond
              ((and (<= (+ x-delta 17) x (+ x-delta 23))
                    (<= (+ y-delta 7) y ( + y-delta 9)))
               (send (eval variable) :role 'X))
              ((and (<= (+ x-delta 15) x (+ x-delta 17))
                    (<= y-delta y (+ y-delta 6)))
               (send (eval variable) :role 'Y))
              ((and (<= (+ x-delta 10) x (+ x-delta 16))
                    (<= (+ y-delta 8) y (+ y-delta 12)))
               (send (eval variable) :role 'Z))
              (t (send (eval variable) :role nil)))))
    (if (<= (+ x-delta 26) x (+ x-delta 80))
        (message-dialog "Variable editing is under construction."))))
