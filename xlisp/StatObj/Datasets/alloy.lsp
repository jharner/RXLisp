(def method (make-variable '(A A A A A A A A B B B B B B B B) :name 'Method))

(def %alloy (make-variable '(13.3 13.4 13.3 13.5 13.6 13.4 13.3 13.4 13.9 14.0
                                  13.9 13.9 13.9 13.9 13.8 13.7)
                           :name '%Alloy))

(def Alloy (make-dataset (list method %alloy) :dataset-name 'Alloy))