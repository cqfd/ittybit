(ns ittybit.peer
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [cljs.core.match.macros :refer [match]])
  (:require [cljs.nodejs :as n]
            [cljs.core.async :refer [<! alts! chan close! put!]]
            [cljs.core.match]
            [ittybit.protocol :as protocol]))

(def net (n/require "net"))

(defn get-messages
  [in]
  (let [out (chan)]
    (go (let [[info-hash peer-id] (<! (protocol/get-handshake in))]
          (if (or (nil? info-hash) (nil? peer-id))
            (do (close! in) (close! out))
            (do (>! out [:handshake info-hash peer-id])
                (loop []
                  (if-let [m (<! (protocol/get-message in))]
                    (do (>! out m) (recur))
                    (do (close! in) (close! out))))))))
    out))

(defn shake!
  [p our-info-hash our-peer-id]
  (go (>! (:outbox @p) [:handshake our-info-hash our-peer-id])
      (let [msg (<! (:inbox @p))]
        (when (= (protocol/msg->type msg) :handshake)
          (let [[_ their-info-hash their-peer-id] msg]
            (when (= our-info-hash their-info-hash)
              their-peer-id))))))

(defn connect!
  "Given a host and a port, connect to a peer. Returns a channel that
  will (hopefully) yield an inbox channel and an outbox channel. The
  inbox contains messages from the peer, and messages placed into the
  outbox will be sent to the peer over the wire."
  [host port]
  (let [c (chan)
        incoming-bytes (chan)
        outbox (chan)
        conn (. net (connect port host))]
    (. conn (on "error" (fn [err]
                          (close! incoming-bytes)
                          (close! c))))
    (. conn (on "end" (fn []
                        (close! incoming-bytes) ; race condition!
                        (close! outbox))))
    (. conn (on "connect" (fn []
                            (put! c [(get-messages incoming-bytes) outbox]
                                  (fn [_] (close! c))))))
    (. conn (on "data" (fn [buf]
                         (. conn pause)
                         (go (loop [buf buf]
                               (if-let [b (first buf)]
                                 (if (>! incoming-bytes b)
                                   (recur (next buf))
                                   (. conn destroy))
                                 (. conn resume)))))))
    (go (loop []
          (if-let [cmd (<! outbox)]
            (do (. conn (write (protocol/msg->buf cmd)))
                (recur))
            (do (close! incoming-bytes)
                (. conn destroy)))))
    c))

(defn start!
  "Returns a channel that yields a fully-connected peer."
  [host port info-hash our-peer-id]
  (go (when-let [[inbox outbox] (<! (connect! host port))]
        (let [peer (atom {:inbox inbox :outbox outbox})]
          (when-let [their-peer-id (<! (shake! peer info-hash our-peer-id))]
            (swap! peer assoc :peer-id their-peer-id)
            peer)))))
