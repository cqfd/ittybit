(ns ittybit.extensions)

(extend-type js/Buffer
  ISeq
  (-first [this]
    (when (> (.-length this) 0)
      (aget this 0)))
  (-rest [this]
    (when (> (.-length this) 0)
      (. this (slice 1))))
  ISeqable
  (-seq [this] this)
  IEquiv
  (-equiv [this that]
    (and (= (.-length this) (.-length that))
         (loop [i 0]
           (or (= i (.-length this))
               (and (= (aget this i) (aget that i))
                    (recur (inc i))))))))
