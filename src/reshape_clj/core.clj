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

(comment

  (paths {:a "aa" :b {:c :f}})

  (def getter (make-getter (paths {:a "aa" :b {:c :c}})))

  (getter {:a "a value!!!"
           :b {:c "!c value!!"}}))