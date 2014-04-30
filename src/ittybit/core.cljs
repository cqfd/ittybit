(ns ittybit.core
  (:require-macros [cljs.core.async.macros :refer [alt! go]])
  (:require [cljs.nodejs :as n]
            [cljs.core.async :refer [<! chan close! put! take]]
            [ittybit.metainfo :as minfo]
            [ittybit.tracker :as tracker]
            [ittybit.peer :as peer]))

(n/enable-util-print!)

(def fs (n/require "fs"))

(defn read-file [path]
  (let [c (chan)]
    (. fs (readFile path (fn [err data] (put! c data))))
    c))

(def our-peer-id (js/Buffer. 20))

(defn main [torrent-path]
  (go (let [minfo (minfo/parse (<! (read-file torrent-path)))
            info-hash (:info-hash minfo)]
        (dotimes [i 10]
          (doseq [w (minfo/piece->writes minfo i)]
            (println "path:" (:path w))
            (println "slice-start" (:slice-start w))
            (println "slice-end" (:slice-end w))
            (println "seek" (:seek w))
            (println))))))

(set! *main-cli-fn* main)
