(ns ittybit.protocol
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [cljs.core.match.macros :refer [match]])
  (:require [cljs.core.async :refer [<!]]
            [cljs.core.match]))

(def B js/Buffer)

(defn msg->type [msg]
  (if (keyword? msg)
    msg
    (first msg)))

(defn int->buf [i]
  (let [buf (B. 4)]
    (. buf (writeUInt32BE i 0))
    buf))

(defn msg->buf [msg]
  (match msg
    [:handshake info-hash peer-id]
    (.concat B #js [(B. #js [19])
                    (B. "BitTorrent protocol")
                    (B. #js [0 0 0 0 0 0 0 0])
                    info-hash
                    peer-id])
    :keep-alive
    (B. #js [0 0 0 0])
    :choke
    (B. #js [0 0 0 1 0])
    :unchoke
    (B. #js [0 0 0 1 1])
    :interested
    (B. #js [0 0 0 1 2])
    :not-interested
    (B. #js [0 0 0 1 3])
    [:have idx]
    (.concat B #js [(B. #js [0 0 0 5 4])
                    (int->buf idx)])
    [:bitfield bfield]
    (.concat B #js [(int->buf (+ 1 (.-length bfield)))
                    bfield])
    [:request idx begin length]
    (.concat B #js [(B. #js [0 0 0 13 6])
                    (int->buf idx)
                    (int->buf begin)
                    (int->buf length)])
    [:piece idx begin chunk]
    (.concat B #js [(int->buf (+ 9 (.-length chunk)))
                    (B. #js [7])
                    (int->buf idx)
                    (int->buf begin)
                    chunk])
    [:cancel idx begin length]
    (.concat B #js [(B. #js [0 0 0 13 8])
                    (int->buf idx)
                    (int->buf begin)
                    (int->buf length)])))

(defn literally
  [buf in]
  (go (loop [i 0]
        (if (= i (.-length buf))
          buf
          (let [next (<! in)]
            (when (= (aget buf i) next)
              (recur (inc i))))))))

(defn take-exactly
  [n in]
  (go (loop [i 0
             acc []]
        (if (= i n)
          (B. (apply array acc))
          (when-let [next (<! in)]
            (recur (inc i) (conj acc next)))))))

(defn get-int
  [in]
  (go (when-let [buf (<! (take-exactly 4 in))]
        (.readUInt32BE buf 0))))

(defn get-handshake
  [in]
  (go (when (<! (literally (.concat B #js [(B. #js [19])
                                           (B. "BitTorrent protocol")])
                           in))
        (when-let [_reserved (<! (take-exactly 8 in))]
          (when-let [info-hash (<! (take-exactly 20 in))]
            (when-let [peer-id (<! (take-exactly 20 in))]
              [info-hash peer-id]))))))

(defn get-message
  [in]
  (go (let [length (<! (get-int in))]
        (when length
          (if (= 0 length)
            :keep-alive
            (when-let [id (<! in)]
              (cond
               (and (= id 0) (= length 1))
               :choke

               (and (= id 1) (= length 1))
               :unchoke

               (and (= id 2) (= length 1))
               :interested

               (and (= id 3) (= length 1))
               :not-interested

               (and (= id 4) (= length 5))
               (when-let [piece-index (<! (get-int in))]
                 [:have piece-index])

               (and (= id 5) (> length 1))
               (when-let [bfield (<! (take-exactly (- length 1) in))]
                 [:bitfield bfield])

               (and (= id 6) (= length 13))
               (when-let [piece-index (<! (get-int in))]
                 (when-let [piece-begin (<! (get-int in))]
                   (when-let [piece-length (<! (get-int in))]
                     [:request piece-index piece-begin piece-length])))

               (and (= id 7) (> length 9))
               (when-let [piece-index (<! (get-int in))]
                 (when-let [piece-begin (<! (get-int in))]
                   (when-let [block (<! (take-exactly (- length 9) in))]
                     [:piece piece-index piece-begin block])))

               (and (= id 8) (= length 13))
               (when-let [piece-index (<! (get-int in))]
                 (when-let [piece-begin (<! (get-int in))]
                   (when-let [piece-length (<! (get-int in))]
                     [:cancel piece-index piece-begin piece-length])))

               (and (= id 9) (= length 3))
               (when-let [port-buf (<! (take-exactly 2 in))]
                 [:port (. port-buf (readUInt16BE 0))])

               :else nil)))))))
