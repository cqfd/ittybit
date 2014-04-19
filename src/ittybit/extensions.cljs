(ns ittybit.extensions)

(extend-type js/Buffer
  IEquiv
  (-equiv [this that]
    (and (= (.-length this) (.-length that))
         (loop [i 0]
           (or (= i (.-length this))
               (and (= (aget this i) (aget that i))
                    (recur (inc i))))))))
