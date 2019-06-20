(def pH (make-variable '(6.5 6.9 7.8 8.4 8.8 9.2 6.7 6.9 7.5 7.9 8.7 9.2 6.5
                             7.0 7.2 7.6 8.7 9.2) :name 'pH :role 'X))

(def solids (make-variable '(292 329 352 378 392 410 198 227 277 297 364 375 167
                                 225 247 268 288 342) :name 'Solids :role 'Y))

(def polymer (make-variable '(P1 P1 P1 P1 P1 P1 P2 P2 P2 P2 P2 P2 P3 P3 P3
                                 P3 P3 P3) :name 'Polymer :role 'Z))

(def Cleansing (make-dataset (list pH solids polymer)
                             :dataset-name 'Cleansing))