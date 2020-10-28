(defn sizes-checker [args seq]
  (every? (partial == (count (first args))) seq))

(defn vectors-sizes-equals? [args] (sizes-checker args (mapv count args)))
(defn is-vector? [a] (and (every? number? a) (vector? a)))
(defn is-matrix? [a] (and (vector? a) (every? is-vector? a) (vectors-sizes-equals? a)))
(defn is-tensor? [a]
  (or
    (number? a)
    (every? number? a)
    (and (every? vector? a) (vectors-sizes-equals? a) (is-tensor? (apply concat [] a)))
    )
  )

(defn abstract-operation [check-function]
  (fn [operation]
    (letfn [(recursive-operation [& args]
              {:pre [(or (every? number? args) (vectors-sizes-equals? args))]}
              (if (every? number? args) (apply operation args) (apply mapv recursive-operation args)))]
      (fn [& args]
        {:pre [(every? check-function args)]}
        (apply recursive-operation args)))))

(def vector-operation (abstract-operation is-vector?))
(def matrix-operation (abstract-operation is-matrix?))
(def tensor-operation (abstract-operation is-tensor?))

(def v+ (vector-operation +))
(def v- (vector-operation -))
(def v* (vector-operation *))
(defn scalar [& args] {:pre [(every? is-vector? args) (vectors-sizes-equals? args)]}
  (apply + (apply v* args)))

(defn vect [& args]
  {:pre [(vectors-sizes-equals? args) (every? is-vector? args) (every? (partial == 3) (mapv count args))]}
  (letfn [(simple-vect [[x1, y1, z1], [x2, y2, z2]]
          (vector (- (* y1 z2) (* y2 z1)) (- (* x2 z1) (* x1 z2)) (- (* x1 y2) (* x2 y1))))]
    (reduce simple-vect args)))
(defn v*s [v & args]
  {:pre [(is-vector? v) (every? number? args)]}
  (let [prod (apply * args)]
    (mapv (partial * prod) v)
    )
  )
(def m+ (matrix-operation +))
(def m- (matrix-operation -))
(def m* (matrix-operation *))
(defn m*s [m & args] {:pre [(is-matrix? m) (every? number? args)]}
  (let [prod (apply * args)]
    (mapv #(v*s % prod) m)))
(defn m*v [m v] {:pre [(is-matrix? m) (is-vector? v) (= (count v) (count (first m)))]} (mapv (fn [x] (scalar x v)) m))

(defn transpose [m] {:pre [(is-matrix? m)]} (apply mapv vector m))

(defn m*m [& args]
  {:pre [(every? is-matrix? args)]}
  (letfn [(simple-prod [a, b] {:pre [(= (count b) (count (first a)))]} (mapv #(m*v (transpose b) %) a))]
  (reduce simple-prod args)))

(def t+ (tensor-operation +))
(def t- (tensor-operation -))
(def t* (tensor-operation *))
