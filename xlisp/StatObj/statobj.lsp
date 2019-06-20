;(send *listener* :hide-window)

(defproto graph-mixin '(dataset) nil nil)

(defmeth graph-mixin :dataset (&optional dataset)
  (if dataset (setf (slot-value 'dataset) dataset))
  (slot-value 'dataset))

(load "StatObj/Structures/variable.lsp")
(load "StatObj/Structures/expression.lsp")
(load "StatObj/Structures/dataset.lsp")
(load "StatObj/Browsers/datatree.lsp")
(load "StatObj/Browsers/databrowser.lsp")
(load "StatObj/Browsers/datatable.lsp")
(load "StatObj/Browsers/datalist.lsp")
(load "StatObj/Controls/datamenus.lsp")
(load "StatObj/Controls/dialogs.lsp")
(load "StatObj/Controls/matrix.lsp")
(load "StatObj/Controls/overlays.lsp")
(load "StatObj/Controls/overlays2.lsp")
(load "StatObj/Controls/overlays3.lsp")
(load "StatObj/Controls/reports.lsp")
(load "StatObj/Controls/reports2.lsp")
(load "StatObj/Plot/plot.lsp")
(load "StatObj/Plot/histogram.lsp")
(load "StatObj/Plot/scatplot.lsp")
(load "StatObj/Plot/quantplot.lsp")
(load "StatObj/Plot/dotplot.lsp")

(defun load-calc ()
  (load "StatObj/Calculator/statcalc.lsp")
  (load "StatObj/Calculator/statcalc2.lsp")
  (load "StatObj/Calculator/statcalc3.lsp")
  (load "StatObj/Calculator/statcalc4.lsp"))

(defun load-mult ()
  (load "StatObj/Multivariate/multivariate.lsp")
  (load "StatObj/Multivariate/cancorr.lsp")
  (load "StatObj/Multivariate/princomp.lsp")
  (load "StatObj/Multivariate/corresp.lsp")
  (load "StatObj/Multivariate/redundancy.lsp")
  (load "StatObj/Multivariate/Controls/dialogs.lsp")
  (load "StatObj/Multivariate/Controls/overlays.lsp")
  (load "StatObj/Multivariate/Tour/xydata.lsp")
  (load "StatObj/Multivariate/Tour/xyplot.lsp"))

(defun load-model ()
  (load "StatObj/Model/model.lsp")
  (load "StatObj/Model/model2.lsp"))

;;; StatObj menu
#|
(setf *statobj-menu* (send menu-proto :new "StatObj"))

(send *statobj-menu* :install)

(setf calc-item
      (send menu-item-proto :new "Load Calculator"
            :action #'(lambda () (load-calc))))

(defmeth calc-item :update ()
  (send self :mark
        (if (find "statcalc" *modules* :test #'equal) t nil)))

(setf mult-item
      (send menu-item-proto :new "Load Multivariate"
            :action #'(lambda () (load-mult))))

(defmeth mult-item :update ()
  (send self :mark
        (if (find "multivariate" *modules* :test #'equal) t nil)))

(setf model-item
      (send menu-item-proto :new "Load Model"
            :action #'(lambda () (load-model))))

(defmeth model-item :update ()
  (send self :mark
        (if (find "model" *modules* :test #'equal) t nil)))

(setf data-tree-item
      (send menu-item-proto :new "Show Data Tree"
            :action #'(lambda () (send *data-tree* :show-window))))

(send *statobj-menu* :append-items data-tree-item (send dash-item-proto :new)
      calc-item mult-item model-item)
|#
;;;
;;; Global variables
;;;

(setf *Data-Tree* (display-data-tree))
;(send *Data-Tree* :hide-window)

(setf *descent* (send graph-window-proto :text-descent))
(setf *line-height* (+ (send graph-window-proto :text-ascent) *descent*))

;;; Datasets

(load "StatObj/Datasets/exercise.lsp")
(load "StatObj/Datasets/state-crime.lsp")
(load "StatObj/Datasets/animals.lsp")
(load "StatObj/Datasets/ethanol.lsp")
(load "StatObj/Datasets/air.lsp")
(load "StatObj/Datasets/cleansing.lsp")
(load "StatObj/Datasets/absorbtion.lsp")

;;; Functions

(defun vector-or (first second)
    (or first second))

(defun css (x)
  (- (inner-product x x) (/ (^ (sum x) 2) (length x))))

(defun ccp (x y)
  (- (inner-product x y) (/ (* (sum x) (sum y)) (length x))))
