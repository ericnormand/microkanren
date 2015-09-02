(ns micro-kanren.core)

(defn variable [x]
  (with-meta (list (symbol (str \V x)))
    {:variable? true}))

(defn variable? [v]
  (boolean (:variable? (meta v))))

(defn walk [u s]
  (if-let [pr (and (variable? u)
                   (get s u))]
    (recur pr s)
    u))

(def empty-state {:state {} :count 0})

(defn unify [u v s]
  (let [u (walk u s)
        v (walk v s)]
    (cond
      (= u v)
      s

      (variable? u)
      (assoc s u v)

      (variable? v)
      (assoc s v u)

      (and (sequential? u) (sequential? v))
      (cond
        (and (seq u) (seq v))
        (when-let [s' (unify (first u) (first v) s)]
          (unify (rest u) (rest v) s'))

        (and (empty? u) (empty? v))
        s)

      (and (map? u) (map? v))
      (let [k (first (keys u))]
        (when (contains? v k)
          (when-let [s' (unify (get u k) (get v k) s)]
            (unify (dissoc u k) (dissoc v k) s')))))))

(defn === [u v]
  (fn [sc]
    (when-let [s (unify u v (:state sc))]
      (list (assoc sc :state s)))))

(defn fresh [f]
  (fn [sc]
    ((f (variable (:count sc))) (update-in sc [:count] inc))))

(defn mplus [s1 s2]
  (lazy-seq
   (if (seq s1)
     (cons (first s1)
           (mplus s2 (rest s1)))
     s2)))

(defn disjunction [g1 g2]
  (fn [sc]
    (mplus (g1 sc)
           (g2 sc))))

(defn bind [s g]
  (lazy-seq
   (when (seq s)
     (mplus (g (first s))
            (bind (rest s) g)))))

(defn conjunction [g1 g2]
  (fn [sc]
    (bind (g1 sc) g2)))

(defmacro inv [g]
  `(fn [sc#]
     (lazy-seq (~g sc#))))

(defmacro disj+
  ([x]
   `(inv ~x))
  ([x & rst]
   `(inv (disjunction (inv ~x)
                      (disj+ ~@rst)))))

(defmacro conj+
  ([x]
   `(inv ~x))
  ([x & rst]
   `(inv (conjunction (inv ~x)
                      (conj+ ~@rst)))))

(defmacro fresh+
  [vars & body]
  (if-not (seq vars)
    `(conj+ ~@body)
    `(fresh (fn [var#]
              (let [~(first vars) var#
                    g# (fresh+ [~@(rest vars)] ~@body)]
                (fn [sc#]
                  (g# (assoc-in sc# [:state '~(first vars)] var#))))))))

(defn resol [x s]
  (cond
    (variable? x)
    (let [r (get s x)]
      (if (nil? r)
        x
        (recur r s)))

    (sequential? x)
    (map #(resol % s) x)

    (map? x)
    (into {} (for [[k v] x]
               [k (resol v s)]))

    :else
    x))

(defmacro run* [[q] & body]
  `(let [g# (fresh+ [~q] ~@body)]
     (map #(resol (with-meta '~q {:variable? true})
                  (:state %))
          (g# empty-state))))

(defmacro run [n [q] & body]
  `(take ~n (run* [~q] ~@body)))

(defn fives [x] (disj+ (=== x 5)
                       (fives x)))

(defn sixes [x]
  (disj+ (=== x 6)
         (sixes x)))

(def fives-and-sixes
  (fresh+ [x] (disj+ (fives x) (sixes x))))

(defn conso [a d q]
  (=== (cons a d) q))

(defn membero [v l]
  (fresh
    (fn [head]
      (fresh
        (fn [tail]
          (conjunction
           (conso head tail l)
           (disjunction
            (=== v head)
            (membero v tail))))))))

(defn assoco [m k v q]
  (=== (assoc m k v) q))

(comment

  (run* [q]
    (fresh+ [x]
            (=== 10 x)
            (=== {:x x} {:x q})))

  (run 5 [q]
    (conso 1 q [1 2 3]))

  (run 5 [q]
    (assoco q :x 10 {:x 10}))

  )
