(setf *types* (list 'log 'logit 'probit 'power 'reciprocal
                    '----------------
                    'normal 'poisson 'binomial 'gamma 'inv-Gaussian))

(setf *symbols* (list "(" ")" "+" "-" "*" "/" ":" "^" "**" "=" "~"
                      "beta" "mu" "eta" "theta" "alpha"
                      "abs" "sqrt" "acos" "asin" "atan" "cos" "sin" "tan"
                      "exp" "expt" "log" "log-gamma"))

;;;;
;;;;
;;;; Model Browser Prototype
;;;;
;;;;

(defproto model-browser-proto '(model-tree icon-list title)
  () (list graph-window-proto graph-mixin))

(defmeth model-browser-proto :isnew (model-tree &rest args)
  (setf (slot-value 'model-tree) model-tree)
  (send model-tree :y-coord 0)
  (send model-tree :position-elements)
  (send self :title "init")
  (apply #'call-next-method args))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth model-browser-proto :model-tree (&optional tree)
  (if tree (setf (slot-value 'model-tree) tree))
  (slot-value 'model-tree))

(defmeth model-browser-proto :title (&optional (title nil set-title))
  (if (equal title "init")
      (setf (slot-value 'title)
            (concatenate 'string
                         (format nil "~s" (send (send self :model-tree)
                                                :name)) " Browser")))
  (if (and set-title (not (equal title "init")))
      (setf (slot-value 'title) title))
  (slot-value 'title))

(defmeth model-browser-proto :icon-list (&optional (icon-list nil set-list))
  (if set-list (setf (slot-value 'icon-list) icon-list))
  (slot-value 'icon-list))

;;;
;;; Other Methods
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Builds icon list for the models in the tree...    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmeth model-browser-proto :build-icons (tree)
  (cond ((atom tree) 
         (setf (slot-value 'icon-list)
               (reverse (cons (send model-icon-proto
                                    :new :dataset tree :model-browser self)
                              (reverse (slot-value 'icon-list))))))
    (t (let* ((node (car tree))
              (children (cdr tree))
              (icon (send model-icon-proto
                          :new :dataset node :model-browser self)))
         (setf (slot-value 'icon-list)
               (reverse (cons icon (reverse (slot-value 'icon-list)))))
         (mapcar #'(lambda (branch)
                     (send self :build-icons branch)) children)))))

(defmeth model-browser-proto :add-icon (model)
  (setf (slot-value 'icon-list)
        (reverse (cons (send model-icon-proto
                             :new :dataset model :model-browser self)
                       (reverse (slot-value 'icon-list))))))


(defmeth model-browser-proto :redraw ()
  (let* ((model-tree (send self :model-tree))
         (width (send self :canvas-width))
         (height (send self :canvas-height)))
    (unless (slot-value 'icon-list) 
            (send self :build-icons (send model-tree :tree)))
    (send self :start-buffering)
    (send self :erase-rect 0 0 width height)
    (send self :draw-model-tree )
    (send self :buffer-to-screen)))

(defmeth model-browser-proto :draw-model-tree ()
  (let ((icon-list (slot-value 'icon-list)))
    (dolist (icon icon-list)
            (send icon :redraw)))) 

 (defmeth model-browser-proto :find-icon (x y)
   (let* ((icon-list (slot-value 'icon-list))
          (in-icon nil))
     (dolist (icon icon-list)
             (setf in-icon (send icon :in-icon x y))
             (if (car in-icon) (return (list icon (cadr in-icon)))))))

(defmeth model-browser-proto :do-click (x y m1 m2)
  (let* ((info (send self :find-icon x y)))
    (if info
        (let* ((model-icon (first info))
               (model (send model-icon :dataset))
               (model-menu (send model-menu-proto :new model-icon))
               (model-proto-menu (send model-proto-menu-proto
                                       :new model-icon)))
          (cond
            ((eq (second info) t)
             (display-data-browser model))
            (t (if (eql (send model :type) 'M)
                   (send model-menu :popup x y self)
                   (send model-proto-menu :popup x y self))))))
    (send self :redraw)))

;;;
;;; Constructor and Display Function
;;;

(defun display-model-browser (model-tree)
  (let* ((screen-size (screen-size))
         (screen-width (car screen-size))
         (screen-height (cadr screen-size))
         (model-browser (send model-browser-proto
                              :new  model-tree
                              :location (list (/ (- screen-width 475) 2)
                                              (/ (- screen-height 300) 2))
                              :size '(475 300) :go-away t)))
    (send model-browser :has-h-scroll 1200)
    (send model-browser :h-scroll-incs 50 225)
    (send model-browser :has-v-scroll 1200)
    (send model-browser :v-scroll-incs 50 150)
    (setf *current-model-browser* model-browser)))

;;;;;
;;;;;
;;;;; Model Icon Prototype
;;;;;
;;;;;

(defproto model-icon-proto '(model-browser) () graph-mixin "Model Icon")

(defmeth model-icon-proto :model-browser ()
  (slot-value 'model-browser))

(defmeth model-icon-proto :redraw ()
  (let* ((model (slot-value 'dataset))
         (model-browser (slot-value 'model-browser))
         (x (send model :x-coord))
         (y (send model :y-coord))
         (left (+ (* 150 x) 20))
         (top (+ (* 25 y) 20))
         (join-top (+ (* 25 y) 27))
         (type1 (send model :type))
         (type2 (if type1 type1 'P))
         (name (send model :name))
         (parent (car (send model :parents))))
    (send model-browser :frame-rect left top 120 15)
    (send model-browser :draw-line (+ left 14) top (+ left 14) (+ top 13))
    (send model-browser :draw-string
          (format nil "~s" type2) (+ left 5) (+ top 11))
    (send model-browser :draw-string name (+ left 18) (+ top 11))
    (if (not (eql model model-proto))
        (let* ((parent-x (send parent :x-coord))
               (parent-y (send parent :y-coord)))        
          (send model-browser :draw-line
                (+ (* 150 parent-x) 140)
                (+ (* 25 parent-y) 27) left (+ top 7))))))

(defmeth model-icon-proto :in-icon (x y)
  (let* ((model (send self :dataset))
         (model-x (send model :x-coord))
         (model-y (send model :y-coord))
         (left (+ (* 150 model-x) 20))
         (top (+ (* 25 model-y) 20)))
    (if (and (<= left x (+ left 120)) (<= top y (+ top 15)))
        (if (<= left x (+ left 14))
            (list t t)
            (list t nil))
        (list nil nil))))

;;;;;
;;;;;
;;;;;  Model Builder Dialog
;;;;;
;;;;;

(defproto model-builder-dialog-proto '(model-browser parent-model) nil
  modal-dialog-proto "Model Builder")

(defmeth model-builder-dialog-proto :isnew (browser parent-model)
  (let* ((screen-size (screen-size))
         (screen-width (car screen-size))
         (screen-height (cadr screen-size))
         (model-tree (send browser :model-tree))
         (model-frame (send model-tree :owner))
         (variables (send model-frame :variables))
         (variables-selected (if (eql (send parent-model :type) 'M)
                                 (send parent-model :variables)
                                 (list (send model-frame
                                             :dependent-variable))))
         (mask-list (if (eql (send parent-model :type) 'M)
                        (copy-list (send parent-model :mask))
                        (copy-list (send model-frame :mask))))
         (old-mean-fcn (if (eq (send parent-model :type) 'M)
                           (send parent-model :mean-fcn)
                           ""))
         (old-var-fcn (if (eq (send parent-model :type) 'M)
                          (send parent-model :var-fcn)
                          "1"))
         (old-var-names
          (cons "1" (map-elements
                     #'(lambda (x) (format nil "~s" (send (eval x) :name)))    
                     variables)))
         (model-count (+ (send model-tree :model-count) 1))
         (beta-count 1)
         (model-name (send text-item-proto :new "Model Name"
                         :location '(5 5)))
         (model (send text-item-proto :new
                      (concatenate 'string
                                   "Model " (format nil "~s" model-count))
                      :editable t :location '(100 5) :size '(130 15)))
         (dep-name (send text-item-proto :new "Dep Var"
                         :location '(5 30)))
         (dep (send text-item-proto
                    :new (format nil "~s" (send (eval (send model-frame
                                           :dependent-variable)) :name))
                    :editable t
                    :location '(75 35) :size '(365 15)))
         (mean-fcn-name (send text-item-proto :new "Mean Fcn"
                              :location '(5 55)))
         (mean-check (send toggle-item-proto :new ""
                           :value t :location '(5 75)))
         (mean-fcn (send text-item-proto :new old-mean-fcn :editable t
                         :location '(75 60) :size '(365 65)))
         (var-fcn-name (send text-item-proto :new "Var Fcn"
                             :location '(5 130)))
         (var-fcn (send text-item-proto :new old-var-fcn :editable t
                        :location '(75 135) :size '(365 35)))
         (var-name (send text-item-proto :new "Variables"
                            :location '(5 180)))
         (var (send list-item-proto
                       :new old-var-names
                       :location '(5 200) :size '(150 130)))
         (symbol-name (send text-item-proto :new "Symbols"
                              :location '(165 180)))
         (symbol (send list-item-proto
                         :new *symbols*
                         :location '(165 200) :size '(120 130)))
         (type-name (send text-item-proto :new "Types"
                                :location '(295 180)))
         (type (send list-item-proto
                       :new (map-elements
                             #'(lambda (x) (format nil "~s" x))
                             *types*)
                       :location '(295 200) :size '(150 130)))
         (help (send button-item-proto :new "Help"
                       :location '(5 347) :size '(70 20)))
         (suggest (send button-item-proto :new "Suggest"
                   :location '(85 347) :size '(70 20)))
         (mask (send button-item-proto :new "Mask"
                   :location '(165 347) :size '(70 20)))
         (cancel (send button-item-proto :new "Cancel"
                       :location '(290 347) :size '(70 20)))
         (run (send button-item-proto :new "Run"
                   :location '(370 347) :size '(70 20))))
    (send var :action
          #'(lambda (double)
              (if double
                  (let* ((vars
                          (cons '1 (map-elements
                                    #'(lambda (x) (send (eval x) :name))
                                    variables)))
                         (index (send var :selection))
                         (item (format nil "~s" (select vars index)))
                         (fcn (send mean-check :value))
                         (fcn-text (if fcn (send mean-fcn :text)
                                       (send var-fcn :text)))
                         (new-text (concatenate 'string fcn-text " " item)))
                    (if fcn (send mean-fcn :text new-text)
                        (send var-fcn :text new-text))
                    (if (and (> index 0)
                             (not (member (select variables (- index 1))
                                          variables-selected))) 
                        (setf variables-selected
                              (cons (select variables (- index 1))
                                    variables-selected)))))))
    (send symbol :action
          #'(lambda (double)
              (if double 
                  (let* ((symbols *symbols*)
                         (index (send symbol :selection))
                         (item (if (equal (select symbols index) "beta")
                                   (progn
                                    (setf beta-count (+ beta-count 1))
                                    (concatenate 'string
                                     (select symbols index)
                                     (format nil "~s" (- beta-count 1))))
                                   (select symbols index)))
                         (fcn (send mean-check :value))
                         (fcn-text (if fcn (send mean-fcn :text)
                                       (send var-fcn :text)))
                         (new-text (concatenate 'string fcn-text " " item)))
                    (if fcn (send mean-fcn :text new-text)
                        (send var-fcn :text new-text))))))
    (send type :action
          #'(lambda (double)
              (if double
                  (let* ((types *types*)
                         (index (send type :selection))
                         (item (format nil "~s" (select types index)))
                         (head (case index
                                 ((0 7) "((LINK (LOG MU)) (ETA ")
                                 ((1 8) "((LINK (LOGIT MU)) (ETA ")
                                 (2 "((LINK (PROBIT MU)) (ETA ")
                                 (3 "((LINK (^ MU ALPHA)) (ETA ")
                                 ((4 9) "((LINK (^ MU -1)) (ETA ")
                                 (10 "((LINK (^ MU -2)) (ETA ")
                                 (t "")))
                         (tail " ))")
                         (fcn (send mean-check :value))
                         (fcn-text (send mean-fcn :text))
                         (new-text (concatenate 'string head fcn-text tail))
                         (variance (case index
                                     ((0 1 2 3 4) "1")
                                     (7 "MU")
                                     (8 "(* MU (- 1 MU))")
                                     (9 "(^ MU 2)")
                                     (10 "(^ MU 3)")
                                     (t ""))))
                    (if fcn (send mean-fcn :text new-text))
                    (if (not (eq variance ""))
                        (send var-fcn :text variance))))))
    (send mask :action
          #'(lambda ()
              (let* ((model-name (send text-item-proto :new "Model"
                                       :locaiton '(5 5)))
                     (cancel2 (send button-item-proto :new "Cancel"
                                    :location '(290 330) :size '(70 20)))
                     (ok (send modal-button-proto :new "OK"
                                :location '(370 330) :size '(70 20)))
                     (mask-dialog (send modal-dialog-proto :new
                                        (list model-name
                                              cancel2 ok)
                                        :title "Masker"
                              :location (list (/ (- screen-width 450) 2)
                                              (/ (- screen-height 375) 2))
                              :size '(450 375)
                              :default-button ok)))
                (send ok :action #'(lambda () t))
                (unwind-protect (send mask-dialog :modal-dialog)
                                (send mask-dialog :remove)))))
    (send cancel :action
          #'(lambda () (send self :remove)))
    (send run :action
          #'(lambda ()
              (let* ((model-name (send model :text))
                     (dep (send dep :text))
                     (mean-fcn (send mean-fcn :text))
                     (var-fcn (send var-fcn :text))
                     (new-model (send  parent-model :new model-frame
                                       :name model-name :dep dep
                                       :mean-fcn mean-fcn :var-fcn var-fcn)))
                (send new-model :variables variables-selected)
                (send new-model :mask mask-list)
                (unless (eq (send parent-model :type) 'M)
                        (send new-model :symbol (send model-frame :symbol))
                        (send new-model :color (send model-frame :color))
                        (send new-model :state (send model-frame :state)))
                (send model-tree :add-model new-model)
                (send model-tree :model-count model-count)
                (send browser :add-icon new-model)
                (send browser :redraw)
                (setf *current-model* new-model)
                (send new-model :compute)
                (send report-proto :new new-model)
                (send self :remove))))
    (call-next-method (list dep-name dep model-name model
                            mean-fcn-name mean-check mean-fcn
                            var-fcn-name var-fcn var-name var
                            symbol-name symbol type-name type
                            help suggest mask cancel run)
                      :title "Model Builder"
                      :location (list (/ (- screen-width 450) 2)
                                      (/ (- screen-height 375) 2))
                      :size '(450 375)
                      :default-button run)))

;;;
;;; Model Menus and Items
;;;

(defproto model-menu-proto '() () menu-proto "Model")

(defmeth model-menu-proto :isnew (model-icon)
  (call-next-method "Model")
  (let* ((model (send model-icon :dataset))
         (model-browser (send model-icon :model-browser))
         (model-tree (send model-browser :model-tree)))
    (send self :append-items
          (send menu-item-proto :new "Messages")
          (send dash-item-proto :new)
          (send menu-item-proto :new "Create Model..."
                :action #'(lambda ()
                            (send model-builder-dialog-proto
                                  :new model-browser model)))
          (send dash-item-proto :new)
          (send menu-item-proto :new "Show Report"
                :action #'(lambda () (send report-proto :new model)))
          (send menu-item-proto :new "Append Report..."
                :action #'(lambda () nil))
          (send menu-item-proto :new "Show Notes"
                :action #'(lambda () nil))
          (send dash-item-proto :new)
          (send menu-item-proto :new "Plot Residuals"
                :action #'(lambda () nil))
          (send menu-item-proto :new "Plot ARES"
                :action #'(lambda () nil))
          (send menu-item-proto :new "Plot AVP"
                :action #'(lambda () nil))
          (send dash-item-proto :new)
          (send menu-item-proto :new "Close Report"
                :action #'(lambda () nil))
          (send menu-item-proto :new "Close Plots"
                :action #'(lambda () nil))
          (send menu-item-proto :new "Remove Model"
                :action #'(lambda () 
                            (when (send model-tree :remove-model model)
                                  (send model-browser :icon-list nil)
                                  (send model-browser :redraw)))))))

;;;
;;; Proto Model Menus and Items
;;;

(defproto model-proto-menu-proto '() () menu-proto "Model")

(defmeth model-proto-menu-proto :isnew (model-icon)
  (call-next-method "Model Proto")
  (let* ((model (send model-icon :dataset))
         (model-browser (send model-icon :model-browser))
         (model-tree (send model-browser :model-tree)))
    (send self :append-items
          (send menu-item-proto :new "Messages")
          (send dash-item-proto :new)
          (send menu-item-proto :new "Create Model..."
                :action #'(lambda ()
                            (send model-builder-dialog-proto
                                  :new model-browser model)))
          (send menu-item-proto :new "Analyze Models..."
                :action #'(lambda () nil)))))

;;;
;;; Model-Browser Menus and Items
;;;

(defproto model-browser-menu-proto '(model-browser) 
  () menu-proto "Model Browser Menu")

(defmeth model-browser-menu-proto :isnew (model-browser)
  (call-next-method "Model Browser Menu")
  (send self :append-items
      (send menu-item-proto :new "Messages")
      (send dash-item-proto :new)
      (send menu-item-proto :new "Show Notes"
            :action #'(lambda () nil))
      (send menu-item-proto :new "Show Script"
            :action #'(lambda () nil))
      (send dash-item-proto :new)
      (send menu-item-proto :new "Hide Model Browser"
            :action #'(lambda ()
                        (send model-browser :hide-window)))
      (send menu-item-proto :new "Remove Model Browser"
            :action #'(lambda () nil))))

