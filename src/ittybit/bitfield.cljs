(ns ittybit.bitfield)

(defn -copy [buf]
  (let [buf' (js/Buffer. (.-length buf))]
    (. buf (copy buf'))
    buf'))

(defn -set [buf i]
  (let [mask (bit-shift-right 2r10000000 (rem i 8))
        old-byte (aget buf (bit-shift-right i 3))
        new-byte (bit-or mask old-byte)]
    (aset buf (bit-shift-right i 3) new-byte)))

(defn -unset [buf i]
  (let [mask (bit-not (bit-shift-right 2r10000000 (rem i 8)))
        old-byte (aget buf (bit-shift-right i 3))
        new-byte (bit-and mask old-byte)]
    (aset buf (bit-shift-right i 3) new-byte)))

(deftype IndexedBitfield [bf i]
  ISeq
  (-first [this]
    ;; invariant: the i in an (IndexedBitfield. bf i) must be set in
    ;; bf.
    (assert (= i (bf i)))
    i)
  (-rest [this]
    ;; preserve the invariant by finding the next index set in bf.
    (loop [i (inc i)]
      (if (= i (.-size bf))
        nil
        (if (bf i)
          (IndexedBitfield. bf i)
          (recur (inc i)))))))

(defn indexed-bitfield [bf]
  (rest (IndexedBitfield. bf -1)))

(deftype Bitfield [size buf]
  ICollection
  (-conj [this i]
    (let [buf' (-copy buf)]
      (-set buf' i)
      (Bitfield. size buf')))
  IEditableCollection
  (-as-transient [this]
    (let [buf' (-copy buf)]
      (Bitfield. size buf')))
  IFn
  (-invoke [this i]
    (-lookup this i))
  (-invoke [this i not-found]
    (-lookup this i not-found))
  ILookup
  (-lookup [this i]
    (-lookup this i nil))
  (-lookup [this i not-found]
    (let [mask (bit-shift-right 2r10000000 (rem i 8))
          the-byte (aget buf (bit-shift-right i 3))]
      (if-not (zero? (bit-and mask the-byte))
        i
        not-found)))
  ISeqable
  (-seq [this]
    (indexed-bitfield this))
  ISet
  (-disjoin [this i]
    (let [buf' (-copy buf)]
      (-unset buf' i)
      (Bitfield. size buf')))
  ITransientCollection
  (-conj! [this i]
    (-set buf i)
    this)
  (-persistent! [this]
    this)
  ITransientSet
  (-disjoin! [this i]
    (-unset buf i)
    this))

(defn of-size [size]
  (let [buf (js/Buffer. (js/Array. (+ 1 (bit-shift-right size 3))))]
    (Bitfield. size buf)))

(let [bf (of-size 1)]
  (assert (not (get bf 0)))
  (let [bf' (conj bf 0)]
    (assert (nil? (bf 0)))
    (assert (= 0 (bf' 0)))
    (let [bf'' (disj bf' 0)]
      (assert (= 0 (bf' 0)))
      (assert (nil? (bf'' 1))))))

(defn bytewise [op bf bf']
  {:pre [(= (.-size bf) (.-size bf'))]}
  (let [buf (js/Buffer. (.-length (.-buf bf)))]
    (dotimes [i (.-length (.-buf bf))]
      (aset buf i (op (aget (.-buf bf) i)
                      (aget (.-buf bf') i))))
    (Bitfield. (.-size bf) buf)))

(def union (partial bytewise bit-or))
(def intersection (partial bytewise bit-and))
(defn difference [bf bf']
  (bytewise (fn [b b']
              (bit-and b (bit-not b')))
            bf bf'))

(let [bf (conj (of-size 256) 0)
      bf' (conj (of-size 256) 1)]
  (assert (contains? bf 0))
  (assert (contains? bf' 1))
  (assert (contains? (union bf bf') 0))
  (assert (contains? (union bf bf') 1)))

(let [bf (into (of-size 1234) [1 2 3])
      bf' (into (of-size 1234) [2 3 4])]
  (assert (= [1] (vec (difference bf bf')))))
