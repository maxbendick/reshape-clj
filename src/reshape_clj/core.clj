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

(comment

(paths {:a "aa" :b {:c :f}})

)