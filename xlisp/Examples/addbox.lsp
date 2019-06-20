(defmeth spin-proto :add-box ()
  (let ((x (send self :visible-range 0))
        (y (send self :visible-range 1))
        (z (send self :visible-range 2)))
    (send self :add-lines (list (select x '(0 1 1 0 0))
                                (select y '(0 0 1 1 0))
                                (select z '(0 0 0 0 0)))
          :draw nil)
    (send self :add-lines (list (select x '(0 1 1 0 0))
                                (select y '(0 0 1 1 0))
                                (select z '(1 1 1 1 1)))
          :draw nil)
    (send self :add-lines (list (select x '(0 0))
                                (select y '(0 0))
                                (select z '(0 1)))
          :draw nil)
    (send self :add-lines (list (select x '(0 0))
                                (select y '(1 1))
                                (select z '(0 1)))
          :draw nil)
    (send self :add-lines (list (select x '(1 1))
                                (select y '(1 1))
                                (select z '(0 1)))
          :draw nil)
    (send self :add-lines (list (select x '(1 1))
                                (select y '(0 0))
                                (select z '(0 1))))))
