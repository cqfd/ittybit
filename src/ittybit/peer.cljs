(ns ittybit.peer
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as n]
            [cljs.core.async :refer [<! chan close! put!]]))

(def B js/Buffer)
(def net (n/require "net"))

(defn handshake [info-hash peer-id]
  (. B (concat #js [(B. #js [19])
                    (B. "BitTorrent protocol")
                    (B. #js [0 0 0 0 0 0 0 0])
                    info-hash
                    peer-id])))

(def keep-alive (B. #js [0 0 0 0]))
(def choke (B. #js [0 0 0 1 0]))
(def unchoke (B. #js [0 0 0 1 1]))
(def interested (B. #js [0 0 0 1 2]))
(def not-interested (B. #js [0 0 0 1 3]))

(defn connect-raw [host port info-hash peer-id]
  (let [c (chan)
        sock (. net (connect port host))]
    (. sock (on "error" (fn [err] (close! c))))
    (. sock (on "end" (fn [] (close! c))))
    (. sock (on "connect"
                (fn []
                  (. sock (write (handshake info-hash peer-id)))
                  (println "wrote a handshake"))))
    (. sock (on "data" (fn [buf]
                         (. sock pause)
                         (go (loop [buf buf]
                               (if-let [b (first buf)]
                                 (if (>! c b)
                                   (recur (next buf))
                                   (. sock destroy))
                                 (. sock resume)))))))
    c))


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
          (if-let [next (<! in)]
            (recur (inc i) (conj acc next)))))))

(defn integer
  [in]
  (go (when-let [buf (<! (take-exactly 4 in))]
        (.readUInt32BE buf 0))))

(defn get-handshake
  [in]
  (go (let [preamble (.concat B (array (B. #js [19])
                                       (B. "BitTorrent protocol")))]
        (when (<! (literally preamble in))
          (when-let [_reserved (<! (take-exactly 8 in))]
            (when-let [info-hash (<! (take-exactly 20 in))]
              (when-let [peer-id (<! (take-exactly 20 in))]
                [info-hash peer-id])))))))

(defn get-message
  [in]
  (go (let [length (<! (integer in))]
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
               (when-let [piece-index (<! (integer in))]
                 [:have piece-index])

               (and (= 5 id) (> length 1))
               (when-let [bfield (<! (take-exactly (- length 1) in))]
                 [:bitfield bfield])

               (and (= 6 id) (= length 13))
               (when-let [piece-index (integer in)]
                 (when-let [piece-begin (integer in)]
                   (when-let [piece-length (integer in)]
                     [:request piece-index piece-begin piece-length])))

               (and (= id 7) (> length 9))
               (when-let [piece-index (integer in)]
                 (when-let [piece-begin (integer in)]
                   (when-let [block (take-exactly (- length 9) in)]
                     [:piece piece-index piece-begin block])))

               (and (= id 8) (= length 13))
               (when-let [piece-index (integer in)]
                 (when-let [piece-begin (integer in)]
                   (when-let [piece-length (integer in)]
                     [:cancel piece-index piece-begin piece-length])))

               (and (= id 9) (= length 3))
               (when-let [port-buf (take-exactly 2 in)]
                 [:port (. port-buf (readUInt16BE 0))]))))))))

(defn get-messages
  [in]
  (let [out (chan)]
    (go (let [[info-hash peer-id] (<! (get-handshake in))]
          (if (or (nil? info-hash) (nil? peer-id))
            (do (close! in) (close! out))
            (do (>! out [:handshake info-hash peer-id])
                (loop []
                  (if-let [m (<! (get-message in))]
                    (do (>! out m) (recur))
                    (do (close! in) (close! out))))))))
    out))

(defn connect [host port info-hash peer-id]
  (let [raw-bytes (connect-raw host port info-hash peer-id)]
    (get-messages raw-bytes)))
