(def Temp (make-variable '(-0.8 -0.7 0.4 2.5 2.9 3.2 3.6 3.9 4.2 4.3 5.4 6 6 6 6.2 6.3 6.9 7 7.4 7.5 7.5 7.6 8 8.5 9.1 10.2 -0.7 0.8 1 1.4 1.5 1.6 2.3 2.5 2.5 3.1 3.9 4 4 4.2 4.3 4.6 4.7 4.9 4.9 4.9 5 5.3 6.2 7.1 7.2 7.5 8 8.7 8.8 9.7) :name 'Temp))

(def GasUsage (make-variable '(7.2 6.9 6.4 6 5.8 5.8 5.6 4.7 5.8 5.2 4.9 4.9 4.3 4.4 4.5 4.6 3.7 3.9 4.2 4 3.9 3.5 4 3.6 3.1 2.6 4.8 4.6 4.7 4 4.2 4.2 4.1 4 3.5 3.2 3.9 3.5 3.7 3.5 3.5 3.7 3.5 3.4 3.7 4 3.6 3.7 2.8 3 2.8 2.6 2.7 2.8 1.3 1.5) :name 'GasUsage))

(def Insulated (make-variable '(NO NO NO NO NO NO NO NO NO NO NO NO NO NO NO NO NO NO NO NO NO NO NO NO NO NO YES YES YES YES YES YES YES YES YES YES YES YES YES YES YES YES YES YES YES YES YES YES YES YES YES YES YES YES YES YES)
     :name 'Insulated :type 'C))

(def Insulate (make-dataset (list Temp GasUsage Insulated)
                            :dataset-name 'Insulate))