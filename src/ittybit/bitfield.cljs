(ns ittybit.bitfield)

(defn -copy [buf]
  (let [buf' (js/Buffer. (.-length buf))]
    (. buf (copy buf'))
    buf'))

(defn -set [buf i]
  (let [mask (bit-shift-right 2r10000000 (rem i 8))
        old-byte (aget buf (bit-shift-right i 3))
        new-byte (bit-or mask old-byte)]
    (if-not (== old-byte new-byte)
      (do (aset buf (bit-shift-right i 3) new-byte)
          true)
      false)))

(defn -unset [buf i]
  (let [mask (bit-not (bit-shift-right 2r10000000 (rem i 8)))
        old-byte (aget buf (bit-shift-right i 3))
        new-byte (bit-and mask old-byte)]
    (if-not (== old-byte new-byte)
      (do (aset buf (bit-shift-right i 3) new-byte)
          true)
      false)))

(defn -empty? [buf]
  (loop [i 0]
    (if (= i (.-length buf))
      true
      (if (zero? (aget buf i))
        (recur (inc i))
        false))))

(deftype IndexedBitfield [bf i]
  ISeqable
  (-seq [this] this)
  ISeq
  (-first [this]
    (assert (= i (bf i)))
    i)
  (-rest [this]
    (loop [i (inc i)]
      (if (= i (.-size bf))
        nil
        (if (bf i)
          (IndexedBitfield. bf i)
          (recur (inc i)))))))

(defn indexed-bitfield [bf]
  (rest (IndexedBitfield. bf -1)))

(declare Bitfield)

(deftype TransientBitfield [size buf ^:mutable editable?]
  ITransientCollection
  (-conj! [this i]
    (when-not editable?
      (throw (js/Error. "conj! after persistent!")))
    (-set buf i)
    this)
  (-persistent! [this]
    (when-not editable?
      (throw (js/Error. "persistent! called twice")))
    (set! editable? false)
    (Bitfield. size buf))
  ITransientSet
  (-disjoin! [this i]
    (when-not editable?
      (throw (js/Error. "disjoin! afer persistent!")))
    (-unset buf i)
    this))

(deftype Bitfield [size buf]
  ICollection
  (-conj [this i]
    (if (contains? this i)
      this
      (let [buf' (-copy buf)]
        (-set buf' i)
        (Bitfield. size buf'))))
  IEditableCollection
  (-as-transient [this]
    (let [buf' (-copy buf)]
      (TransientBitfield. size buf' true)))
  IEquiv
  (-equiv [this that]
    (and (set? that)
         (every? #(contains? this %) that)))
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
    (when-not (-empty? buf)
      (indexed-bitfield this)))
  ISet
  (-disjoin [this i]
    (if-not (contains? this i)
      this
      (let [buf' (-copy buf)]
        (-unset buf' i)
        (Bitfield. size buf')))))

(defn of-size [size]
  (let [buf (js/Buffer. (js/Array. (+ 1 (bit-shift-right size 3))))]
    (Bitfield. size buf)))

(let [bf (of-size 1024)]
  (assert (= #{} bf))
  (assert (= #{1} (conj bf 1)))
  (assert (= #{1} (into bf [1])))
  (assert (= #{1 2} (into bf [1 2]))))

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

(defn full? [bf]
  (= (count bf) (.-size bf)))

(let [bf (into (of-size 8) (range 7))]
  (assert (not (full? bf)))
  (assert (full? (conj bf 7))))

(let [bf (conj (of-size 256) 0)
      bf' (conj (of-size 256) 1)]
  (assert (contains? bf 0))
  (assert (contains? bf' 1))
  (assert (contains? (union bf bf') 0))
  (assert (contains? (union bf bf') 1)))

(let [bf (into (of-size 1234) [1 2 3])
      bf' (into (of-size 1234) [2 3 4])]
  (assert (= [1] (vec (difference bf bf')))))
