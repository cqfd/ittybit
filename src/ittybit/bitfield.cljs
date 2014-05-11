(ns ittybit.bitfield)

(deftype Bitfield [buf]
  ICollection
  (-conj [this i]
    (let [mask (bit-shift-right 2r10000000 (rem i 8))
          old-byte (aget buf (bit-shift-right i 3))
          new-byte (bit-or mask old-byte)
          buf' (js/Buffer. (.-length buf))]
      (. buf (copy buf'))
      (aset buf' (bit-shift-right i 3) new-byte)
      (Bitfield. buf')))
  ILookup
  (-lookup [this i]
    (-lookup this i nil))
  (-lookup [this i not-found]
    (let [mask (bit-shift-right 2r10000000 (rem i 8))
          the-byte (aget buf (bit-shift-right i 3))]
      (if-not (zero? (bit-and mask the-byte))
        i
        not-found)))
  ISet
  (-disjoin [this i]
    (let [mask (bit-not (bit-shift-right 2r10000000 (rem i 8)))
          old-byte (aget buf (bit-shift-right i 3))
          new-byte (bit-and mask old-byte)
          buf' (js/Buffer. (.-length buf))]
      (. buf (copy buf'))
      (aset buf' (bit-shift-right i 3) new-byte)
      (Bitfield. buf')))
  IFn
  (-invoke [this i]
    (-lookup this i))
  (-invoke [this i not-found]
    (-lookup this i not-found)))

(defn of-size [size]
  (let [buf (js/Buffer. (js/Array. (+ 1 (bit-shift-right size 3))))]
    (Bitfield. buf)))

(let [bf (of-size 1)]
  (assert (not (get bf 0)))
  (let [bf' (conj bf 0)]
    (assert (nil? (bf 0)))
    (assert (= 0 (bf' 0)))
    (let [bf'' (disj bf' 0)]
      (assert (= 0 (bf' 0)))
      (assert (nil? (bf'' 1))))))
