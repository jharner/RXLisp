(def id (make-variable '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40) :name 'id))

(def sex (make-variable '(M F F F F F M M F F F F F F F F M F F M M F F F F F F F M F F F F F M M M M M M) :name 'sex :type 'C))

(def height (make-variable '(183 163 152 157 157 165 173 180 164 160 166 157 168 167 156 155 178 169 171 175 169 168 165 166 164 163 161 157 181 163 157 169 177 174 183 181 182 171 184 179) :name 'height))

(def siblings (make-variable '(1 2 2 3 1 3 1 2 2 3 0 1 0 2 1 1 1 3 5 3 2 4 1 1 3 1 1 2 2 1 2 2 2 1 1 2 1 9 2 1) :name 'siblings))

(def distance (make-variable '(80 3 90 272 80 8 485 176 10 72 294 22 144 160 50 64 224 480 56 141 259 96 104 90 72 37 208 40 120 400 208 169 410 90 80 278 240 192 35 45) :name 'distance))

(def degree (make-variable '(2 1 1 2 2 2 2 2 2 1 2 1 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 1 2 2 2 2 2 1 1) :name 'degree :type 'C))

(def A-level (make-variable '(6 32 22 12 12 18 14 8 6 18 16 12 12 12 10 12 8 10 6 8 4 6 12 8 22 8 6 18 10 10 10 12 16 10 10 8 6 24 14 10) :name 'A-level))

(def Students (make-dataset (list id sex height siblings distance degree
                                  A-level)
                            :dataset-name 'Students))