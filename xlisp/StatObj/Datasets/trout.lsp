(def compound (make-variable
               '(Std Std Std Std Std Std Std Std Std Std New New New New New
                     New New New New New) :name 'compound))

(def weight (make-variable
             '(510 507 490 496 523 508 534 497 491 506 521 476 489 512 521
                   498 505 547 542 492) :name 'weight))

(def trout (make-dataset (list compound weight) :dataset-name 'trout))