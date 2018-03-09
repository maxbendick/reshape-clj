(ns reshape-clj.core)

(defn paths
  "For a nested map, return the paths to each non-map."
  [pattern]
  (let [res (atom [])]
    (letfn [(paths-rec [pattern path]
              (doseq [k (keys pattern)]
                (let [v (k pattern)]
                  (if (map? v)
                    (paths-rec v (cons k path))
                    (swap! res conj {:key v
                                     :path (reverse (cons k path))})))))]
      (do
        (paths-rec pattern [])
        @res))))


(defn make-getter
  [paths]
  (fn [outter]
    (reduce
     (fn [partial-inner path]
       (let [k (:key path)
             p (:path path)
             v (get-in outter p)]
         (do
           (print partial-inner "... v:" v "\n")
           (assoc partial-inner k v))))
     {}
     paths)))


(defn make-setter
  [paths]
  (fn [outter inner]
    (reduce
     (fn [partial-outter path]
       (let [k (:key path)
             p (:path path)
             v (k inner)]
         (do
           (print partial-outter "... v:" v "\n")
           (assoc-in partial-outter p v))))
     {}
     paths)))


(defn lens [pattern]
  (let [ps (paths pattern)]
    {:get (make-getter ps)
     :set (make-setter ps)}))


(defn over [lens f old-outter]
  (let [old-inner ((:get lens) old-outter)
        new-inner (f old-inner)]
    ((:set lens) old-outter new-inner)))

(comment

  (paths {:a "aa" :b {:c :f}})

  (def getter (make-getter (paths {:a "aa" :b {:c :c}})))

  (getter {:a "a value!!!"
           :b {:c "!c value!!"}})

  (def setter (make-setter (paths {:a :a :b {:c :c}})))

  (setter
   {:a "a value!!!"
    :b {:c "!c value!!"}}
   {:a "new ahh"
    :c "new :ccc"})

  (def l (lens
          {:a :a
           :b {:c :c}}))

  ((:get l)
   {:a "a value!!!"
    :b {:c "!c value!!"}})

  ((:set l)
   {:a "a value!!!"
    :b {:c "!c value!!"}}
   {:a "new ahh"
    :c "new :ccc"})

  (defn updater [x] (assoc x :a 5))

  (over l updater
        {:a "a value!!!"
         :b {:c "!c value!!"}})
         
)