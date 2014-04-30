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
            info-hash (:info-hash minfo)]
        (doseq [f (:files minfo)]
          (println f)
          (fs/open-sesame! (:path f))))))

(set! *main-cli-fn* main)
