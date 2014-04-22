(ns ittybit.core
  (:require-macros [cljs.core.async.macros :refer [alt! go]])
  (:require [cljs.nodejs :as n]
            [cljs.core.async :refer [<! chan close! put!]]
            [ittybit.metainfo :as minfo]
            [ittybit.tracker :as tracker]
            [ittybit.peer :as peer]))

(n/enable-util-print!)

(def fs (n/require "fs"))

(defn read-file [path]
  (let [c (chan)]
    (. fs (readFile path (fn [err data] (put! c data))))
    c))

(defn -main [torrent-path]
  (. js/console (log "welcome to ittybit"))
  (go (let [minfo (minfo/parse (<! (read-file torrent-path)))
            peers (<! (tracker/poke (:tracker minfo) (:info-hash minfo)))
            peer-id (js/Buffer. 20)]
        (doseq [[host port] peers]
          (let [messages (peer/connect host port (:info-hash minfo) peer-id)]
            (go (loop []
                  (when-let [msg (<! messages)]
                    (println msg)
                    (recur)))))))))

(set! *main-cli-fn* -main)
