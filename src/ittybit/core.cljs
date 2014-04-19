(ns ittybit.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as n]
            [cljs.core.async :refer [<! chan close! put!]]
            [ittybit.metainfo :as minfo]
            [ittybit.tracker :as tracker]
            [ittybit.protocol :as protocol]))

(n/enable-util-print!)

(def fs (n/require "fs"))

(def net (n/require "net"))

(defn read-file [path]
  (let [c (chan)]
    (. fs (readFile path (fn [err data] (put! c data))))
    c))

(defn connect-to-peer [host port info-hash peer-id]
  (let [c (chan)
        sock (. net (connect port host))]
    (. sock (on "error" (fn [_err] (close! c))))
    (. sock (on "connect"
                (fn []
                  (. sock (write (protocol/handshake info-hash peer-id))))))
    (. sock (on "data" (partial put! c)))
    c))

(defn -main [torrent-path]
  (. js/console (log "welcome to ittybit"))
  (go (let [minfo (minfo/parse (<! (read-file torrent-path)))
            peers (<! (tracker/poke (:tracker minfo) (:info-hash minfo)))
            peer-id (js/Buffer. 20)]
        (doseq [[host port] peers]
          (let [incoming (connect-to-peer host port (:info-hash minfo) peer-id)]
            (go (loop [buf (<! incoming)]
                  (. js/console (log buf)))))))))

(set! *main-cli-fn* -main)
