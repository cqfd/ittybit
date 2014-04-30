(ns ittybit.core
  (:require-macros [cljs.core.async.macros :refer [alt! go]])
  (:require [cljs.nodejs :as n]
            [cljs.core.async :refer [<! chan close! put! take!]]
            [ittybit.fs :as fs]
            [ittybit.metainfo :as minfo]
            [ittybit.peer :as peer]            
            [ittybit.tracker :as tracker]))

(n/enable-util-print!)

(def our-peer-id (js/Buffer. 20))

(defn main [torrent-path]
  (go (let [[_err, minfo-buf] (<! (fs/read-file torrent-path))
            minfo (minfo/parse minfo-buf)
            info-hash (:info-hash minfo)
            hosts-and-ports (tracker/peers! (:trackers minfo) info-hash)]
        (when-let [[host port] (<! hosts-and-ports)]
          (when-let [p (<! (peer/start! host port info-hash our-peer-id))]
            (loop []
              (when-let [msg (<! (:inbox @p))]
                (println msg))))))))

(set! *main-cli-fn* main)
