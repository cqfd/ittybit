(ns ittybit.bitfield
  (:refer-clojure :exclude [get set]))

(defn make
  [size]
  (js/Buffer. (js/Array. (+ 1 (bit-shift-right size 3)))))

(defn get
  [bf i]
  (let [mask (bit-shift-right 2r10000000 (rem i 8))
        the-byte (aget bf (bit-shift-right i 3))]
    (not (zero? (bit-and mask the-byte)))))

(defn set
  [bf i]
  (let [mask (bit-shift-right 2r10000000 (rem i 8))
        old-byte (aget bf (bit-shift-right i 3))
        new-byte (bit-or mask old-byte)]
    (aset bf (bit-shift-right i 3) new-byte)))

(defn unset
  [bf i]
  (let [mask (bit-not (bit-shift-right 2r10000000 (rem i 8)))
        old-byte (aget bf (bit-shift-right i 3))
        new-byte (bit-and mask old-byte)]
    (aset bf (bit-shift-right i 3) new-byte)))

(let [bf (make 1)]
  (assert (not (get bf 0)))
  (set bf 1)
  (assert (get bf 1))
  (unset bf 1)
  (assert (not (get bf 1))))
