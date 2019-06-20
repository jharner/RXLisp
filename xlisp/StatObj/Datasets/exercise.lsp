(def weight (make-variable '(191 189 193 162 189 182 211 167 176 154 169 166 154 247 193 202 176 157 156 138) :name 'weight :role 'X))

(def waist (make-variable '(36 37 38 35 35 36 38 34 31 33 34 33 34 46 36 37 37 32 33 33) :name 'waist :role 'X))

(def pulse (make-variable '(50 52 58 62 46 56 56 60 74 56 50 52 64 50 46 62 54 52 54 68) :name 'pulse :role 'X))

(def chins (make-variable '(5 2 12 12 13 4 8 6 15 17 17 13 14 1 6 12 4 11 15 2) :name 'chins :role 'Y))

(def situps (make-variable '(162 110 101 105 155 101 101 125 200 251 120 210 215 50 70 210 60 230 225 110) :name 'situps :role 'Y))

(def jumps (make-variable '(60 60 101 37 58 42 38 40 40 250 38 115 105 50 31 120 25 80 73 43) :name 'jumps :role 'Y))

(def exercise (make-dataset (list weight waist pulse chins situps jumps)
                            :dataset-name 'exercise))