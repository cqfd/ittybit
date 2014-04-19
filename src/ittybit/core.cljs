(ns ittybit.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as n]
            [cljs.core.async :refer [<! chan put!]]
            [ittybit.metainfo :as minfo]
            [ittybit.tracker :as tracker]))

(n/enable-util-print!)

(def fs (n/require "fs"))

(defn read-file [path]
  (let [out (chan)]
    (. fs (readFile path (fn [err data] (put! out data))))
    out))

(defn -main [torrent-path]
  (. js/console (log "welcome to ittybit"))
  (go (let [minfo (minfo/parse (<! (read-file torrent-path)))
            peers (<! (tracker/poke (:tracker minfo) (:info-hash minfo)))]
        (println peers))))

(set! *main-cli-fn* -main)
