
(defun createReferenceManager ()

 (let* ((manager (send *object* :new)))
     (send manager :add-slot 'table (make-hash-table))
     (send manager :add-slot 'total 0)

     (defmeth manager :assign (value)
        "put the value into the list of exported objects managed by this container"
        (let ((total (+ 1 (send self :slot-value 'total))))
            (def (gethash (format nil "~d" total)  (send self :slot-value 'table)) value)
            (send self :slot-value 'total total))
     )

    (defmeth manager :exists (key)
      "check whether there is an object under the control of this manager identified by key"
      (let* ((ans 0)
           (f (lambda (x val) (if (string= key x) (setf ans 1)))))
           (maphash f (send self :slot-value 'table))
           ans)
    )

    (defmeth manager :get (key)
        "get the XLisp value in the manager associated with the given key"
        (let* ((ans nil)
               (f (lambda (x val)
                          (cond ((string= key x) (setq ans val)))
                  )))
               (maphash f (send self :slot-value 'table))
               ans)  
    )
    manager
    )
)

(def ReferenceManager (createReferenceManager))

; (print ReferenceManager)
; (print (variables))

