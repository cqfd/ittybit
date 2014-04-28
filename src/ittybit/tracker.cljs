(ns ittybit.tracker
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as n]
            [cljs.core.async :refer [<! chan close! merge onto-chan put! timeout]]
            [clojure.set :as set]))

;; For now we only handle udp trackers.

(def B js/Buffer)
(def udp (n/require "dgram"))
(def url (n/require "url"))

(def magic (B. #js [0x00 0x00 0x04 0x17 0x27 0x10 0x19 0x80]))

(defn fresh-tid []
  (B. (array (rand-int 255) (rand-int 255) (rand-int 255) (rand-int 255))))

(defn connect-req [tid]
  {:pre [(= (.-length tid) 4)]}
  (.concat B #js [magic (B. #js [00 00 00 00]) tid]))

(defn announce-req
  [cid tid info-hash]
  (.concat B
     #js [cid
          (B. #js [0 0 0 1])             ; action
          tid
          info-hash
          (B. 20)                        ; fake peer id
          (B. #js [0 0 0 0 0 0 0 0])     ; downloaded
          (B. #js [0 0 0 0 0 0 0 0])     ; left
          (B. #js [0 0 0 0 0 0 0 0])     ; uploaded
          (B. #js [0 0 0 0])             ; event
          (B. #js [0 0 0 0])             ; ip address (?)
          (B. #js [0 0 0 0])             ; key
          (B. #js [0xff 0xff 0xff 0xff]) ; num_want
          (B. #js [0x1a 0xe1])]))

(defn dotted-quad [buf]
  {:pre [(= (.-length buf) 4)]}
  (str (aget buf 0) "." (aget buf 1) "." (aget buf 2) "." (aget buf 3)))

(defn parse-peers [buf acc]
  {:pre [(= (rem (.-length buf) 6) 0)]}
  (if (= 0 (.-length buf))
    acc
    (let [host (dotted-quad (. buf (slice 0 4)))
          port (.readUInt16BE (. buf (slice 4 6)) 0)]
      (recur (. buf (slice 6)) (conj acc [host port])))))

(defn tracker
  [[host port]]
  (let [sock (. udp (createSocket "udp4"))
        inbox (chan)
        outbox (chan)]
    (. sock (on "error" (fn [err] (println "udp error :<" err))))
    (. sock (on "message" (fn [buf _rinfo]
                            (put! inbox buf
                                  (fn [eventually-succeeded?]
                                    (when-not eventually-succeeded?
                                      (close! outbox)
                                      (. sock close)))))))
    (go (loop []
          (when-let [buf (<! outbox)]
            (. sock (send buf 0 (.-length buf) port host))
            (recur))))
    {:inbox inbox :outbox outbox}))

(defn connect!
  [t tid]
  (go (>! (:outbox t) (connect-req tid))
      (when-let [resp (<! (:inbox t))]
        (when (= (.-length resp) 16)
          (let [tid' (. resp (slice 4 8))
                cid (. resp (slice 8 16))]
            (when (= tid tid')
              cid))))))

(defn announce!
  [t cid tid info-hash]
  (go (>! (:outbox t) (announce-req cid tid info-hash))
      (when-let [resp (<! (:inbox t))]
        (if (>= (.-length resp) 20)
          (let [tid' (. resp (slice 4 8))
                peers (. resp (slice 20))]
            (when (= tid tid')
              peers))))))

(defn get-some-peers!
  [t info-hash]
  (go (let [tid (fresh-tid)]
        (when-let [cid (<! (connect! t tid))]
          (when-let [peers (<! (announce! t cid tid info-hash))]
            (parse-peers peers []))))))

(defn get-all-the-peers!
  [t info-hash]
  (let [out (chan)]
    (go (loop [seen-so-far? #{}]
          (when-let [peers (<! (get-some-peers! t info-hash))]
            (let [fresh-peers (filter (comp not seen-so-far?) peers)]
              (onto-chan out fresh-peers)
              (<! (timeout 5000)) ; yuck
              (recur (reduce conj seen-so-far? peers))))))
    out))

(defn unique-merge
  [cs]
  (let [out (chan)
        merged (merge cs)]
    (go (loop [seen-so-far? #{}]
          (let [x (<! merged)]
            (if (nil? x)
              (close! out)
              (if-not (seen-so-far? x)
                (do (>! out x)
                    (recur (conj seen-so-far? x)))
                (recur seen-so-far?))))))
    out))

(defn peers!
  "Given a collection of tracker hosts and ports, returns a channel of
  unique peer hosts and ports."
  [hosts-and-ports info-hash]
  (let [ts (map tracker hosts-and-ports)]
    (unique-merge (map get-all-the-peers! ts (repeat info-hash)))))


