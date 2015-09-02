(ns micro-kanren.core-async
  (:require [clojure.core.async :as async]))

(defmacro go [& body]
  `(async/go
     (try
       ~@body
       (catch Throwable t#
         (println t#)))))

(defn variable [x]
  (with-meta (list x)
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
      (when-let [s' (unify (first u) (first v) s)]
        (unify (rest u) (rest v) s'))

      (and (map? u) (map? v))
      (let [k (first (keys u))]
        (when (contains? v k)
          (when-let [s' (unify (get u k) (get v k) s)]
            (unify (dissoc u k) (dissoc v k) s')))))))

;; goal = state+count -> stream of states+counts

(defn === [u v]
  (fn [sc]
    (go
      (when-let [s (unify u v (:state sc))]
        (assoc sc :state s)))))

(defn fresh [f]
  (fn [sc]
    ((f (variable (:count sc))) (update-in sc [:count] inc))))

(defn mplus [s1 s2]
  (async/merge [s1 s2]))

(defn disj* [g1 g2]
  (fn [sc]
    (mplus (g1 sc) (g2 sc))))

(defmacro disj+
  ([x]
   x)
  ([x & rst]
   `(disj* ~x (disj+ ~@rst))))

(defn bind [s g]
  (let [c (async/chan)]
    (go
      (loop [chans #{s}]
        (when (seq chans)
          (let [[val chan] (async/alts! (vec chans))]
            (cond
              (nil? val)
              (recur (disj chans chan))

              (= s chan)
              (recur (conj chans (g val)))

              :else
              (do
                (async/>! c val)
                (recur chans))))))
      (async/close! c))
    c))

(defn conj* [g1 g2]
  (fn [sc]
    (let [s (g1 sc)]
      (bind s g2))))

(defmacro conj+
  ([x]
   x)
  ([x & rst]
   `(conj* ~x (conj+ ~@rst))))

(defmacro fresh+ [vars & body]
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

(defn run-timeout [to q goal]
  (map #(resol (with-meta q {:variable? true}) (:state %))
       (let [t (async/timeout to)
             c (goal empty-state)]
         (loop [acc []]
           (let [[val] (async/alts!! [c t])]
             (if (nil? val)
               acc
               (recur (conj acc val))))))))

(defmacro run [t [q] & body]
  `(run-timeout ~t '~q (fresh+ [~q] ~@body)))

(defn conso [f rst q]
  (=== (cons f rst) q))
