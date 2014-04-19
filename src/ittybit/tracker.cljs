(ns ittybit.tracker
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as n]
            [cljs.core.async :refer [<! chan put!]]))

;; For now we only handle udp trackers.

(def udp (n/require "dgram"))
(def url (n/require "url"))

(def magic (js/Buffer. #js [0x00 0x00 0x04 0x17 0x27 0x10 0x19 0x80]))

(defn get-connection-id [{:keys [host port] :as tracker}]
  (let [c (chan)
        sock (. udp (createSocket "udp4"))
        transaction-id (js/Buffer. 4) ; random buffer
        req (. js/Buffer (concat #js [magic
                                      (js/Buffer. #js [00 00 00 00])
                                      transaction-id]))]
    (. sock (on "message"
                (fn [buf rinfo]
                  (when [= 16 (.-length buf)]
                    (let [t-id (. buf (slice 4 8))
                          conn-id (. buf (slice 8 16))]
                      (if (= (str transaction-id t-id))
                        (put! c conn-id)))))))
    (. sock (send req 0 16 port host))
    c))

(defn parse-peers [buf]
  (loop [buf buf peers []]
    (if (= 0 (.-length buf))
      peers
      (let [host (. buf (slice 0 4))
            port-buf (. buf (slice 4 6))
            port (+ (* 256 (aget port-buf 0))
                    (aget port-buf 1))]
        (recur (. buf (slice 6)) (conj peers [host port]))))))

(defn announce-req [host port info-hash conn-id]
  (. js/Buffer
     (concat #js [conn-id
                  (js/Buffer. #js [0 0 0 1]) ; action
                  (js/Buffer. 4)             ; transaction id
                  info-hash
                  (js/Buffer. 20)       ; fake peer id
                  (js/Buffer. #js [0 0 0 0 0 0 0 0]) ; downloaded
                  (js/Buffer. #js [0 0 0 0 0 0 0 0]) ; left
                  (js/Buffer. #js [0 0 0 0 0 0 0 0]) ; uploaded
                  (js/Buffer. #js [0 0 0 0])         ; event
                  (js/Buffer. #js [0 0 0 0])         ; ip address (?)
                  (js/Buffer. #js [0 0 0 0])         ; key
                  (js/Buffer. #js [0xff 0xff 0xff 0xff]) ; num_want
                  (js/Buffer. #js [0x1a 0xe1])])))

(defn get-peers [{:keys [host port] :as tracker} info-hash conn-id]
  (let [c (chan)
        sock (. udp (createSocket "udp4"))]
    (. sock (on "message"
                (fn [buf rinfo]
                  (let [peer-buf (. buf (slice 20 (.-length buf)))]
                    (put! c (parse-peers peer-buf))))))
    (. sock (send (announce-req host port info-hash conn-id)
                  0 98 port host))
    c))

(defn poke [tracker info-hash]
  (let [c (chan)]
    (go (let [conn-id (<! (get-connection-id tracker))
              peers (<! (get-peers tracker info-hash conn-id))]
          (put! c peers)))
    c))
