;;;;;
;;;;;  Matrix Prototype
;;;;;      For display purposes only. It stores a matrix
;;;;;      or vector with row and/or column labels
;;;;;

(defproto matrix-proto '(name description dataset row-labels column-labels)
  nil nil "An enhanced matrix prototype")
  
(defmeth matrix-proto :isnew
  (name data &key column-labels row-labels description)
  (setf (slot-value 'name) name)
  (setf (slot-value 'description) description)
  (if (and (arrayp data) (<= (array-rank data) 2))
      (let* ((num-rows (if (vectorp data) 1 (array-dimension data 0)))
             (num-cols (if (vectorp data) (length data)
                           (array-dimension data 1))))
        (setf (slot-value 'dataset) data)
        (setf (slot-value 'row-labels) 
              (if (and row-labels (= (length row-labels) num-rows))
                  row-labels
                  (+ 1 (iseq num-rows))))
        (setf (slot-value 'column-labels)
              (if (and column-labels (= (length column-labels) num-cols))
                  column-labels
                  (+ 1 (iseq num-cols)))))
      (message-dialog "Data type is not a matrix")))

;;;
;;; Slot Accessors and Mutators
;;;

(defmeth matrix-proto :description (&optional (desc nil set-desc))
  (if set-desc (setf (slot-value 'description) desc))
  (slot-value 'description))

(defmeth matrix-proto :name (&optional (name nil set-name))
  (if set-name (setf (slot-value 'name) name))
  (slot-value 'name))

(defmeth matrix-proto :dataset (&optional (data nil set-data))
  (if set-data (setf (slot-value 'dataset) data))
  (slot-value 'dataset))

(defmeth matrix-proto :row-labels (&optional (row-labels nil set-row-names))
  (if set-row-names (setf (slot-value 'row-labels) row-labels))
  (slot-value 'row-labels))

(defmeth matrix-proto :column-labels (&optional (column-labels nil set-col-names))
  (if set-col-names (setf (slot-value 'column-labels) column-labels))
  (slot-value 'column-labels))

;;;;
;;;;  Report Prototype: for displaying results sub form of matrices/vectors
;;;;      in a text window
;;;;
#|
(defproto report-proto nil nil (list display-window-proto graph-mixin)
  "Report Prototype")

(defmeth report-proto :isnew (dataset)
  (setf (slot-value 'dataset) dataset)
  (call-next-method :title (format nil "~a-Report" (send dataset :name)))
  (let ((header (send dataset :make-header))
        (matrix-list (send dataset :make-matrix-list)))
    (dolist (item header)
            (send self :paste-string item))
    (dolist (matrix matrix-list)
            (let* ((name (send matrix :name))
                   (data (send matrix :dataset))
                   (row-labels (send matrix :row-labels))
                   (column-labels (send matrix :column-labels))
                   (description (send matrix :description))
                   (num-rows (if (eql (array-rank data) 2)
                                 (array-dimension data 0) 1))
                   (num-cols (if (eql (array-rank data) 2)
                                 (array-dimension data 1)
                                 (length data)))
                   (cycles (ceiling (/ num-cols 6))))
              (if ( and (= (array-rank data) 1)
                        (not (= (length column-labels) (length data))))
                  (setf column-labels row-labels))
              (send self :paste-string (format nil "~%")) 
              (send self :paste-string name)
              (if description (send self :paste-string description))
              (dotimes (k cycles)
                       (send self :paste-string
                             (format nil "~% ~17a" ""))
                       (dotimes (j (min 6 (- num-cols (* 6 k))))
                                (setf i (+ (* 6 k) j))
                                (send self :paste-string
                                      (format nil "~12a"
                                              (select column-labels i))))
                       (dotimes (i num-rows)
                                (send self :paste-string
                                      (if (= (array-rank data) 2)
                                          (format nil "~% ~12a "
                                                  (select row-labels i))
                                          (format nil "~% ~12a" "")))
                                (dotimes (m (min 6 (- num-cols (* 6 k))))
                                         (setf j (+ (* 6 k) m))
                                         (if (= (array-rank data) 2)
                                             (send self :paste-string
                                                   (format nil "~12,4g"
                                                           (aref data i j)))
                                             (send self :paste-string 
                                                   (format nil "~12,4g"
                                                         (select data j)))))))
              (send self :paste-string (format nil "~%"))))))

(defmeth report-proto :close ()
  (send self :delete-self)
  (call-next-method))
|#










