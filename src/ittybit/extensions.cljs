(ns ittybit.extensions)

(extend-type object
  ILookup
  (-lookup [o k] (aget o k))
  (-lookup [o k not-found] (or (aget o k) not-found)))

(extend-type array
  ILookup
  (-lookup [a i] (aget a i))
  (-lookup [a i not-found] (or (aget a i) not-found))
  ISeqable
  (-seq [this]
    (prim-seq this 0)))

(extend-type js/Buffer
  ISeq
  (-first [this]
    (when (> (.-length this) 0)
      (aget this 0)))
  (-rest [this]
    (when (> (.-length this) 0)
      (. this (slice 1))))
  ISeqable
  (-seq [this] (prim-seq this 0))
  IEquiv
  (-equiv [this that]
    (and (not (nil? that))
         (= (.-length this) (.-length that))
         (loop [i 0]
           (or (= i (.-length this))
               (and (= (aget this i) (aget that i))
                    (recur (inc i))))))))
