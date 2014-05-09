(ns ittybit.core
  (:require-macros [cljs.core.async.macros :refer [alt! go]])
  (:require [cljs.nodejs :as n]
            [cljs.core.async :refer [<! chan close! put! take!]]
            [ittybit.fs :as fs]
            [ittybit.metainfo :as minfo]
            [ittybit.peer :as peer]
            [ittybit.protocol :as proto]
            [ittybit.tracker :as tracker]))

(n/enable-util-print!)

(def our-peer-id (js/Buffer. 20))

(defn piece-writer [minfo]
  (let [in (chan) fds (atom {})]
    (go (loop []
          (when-let [[piece-idx buf] (<! in)]
            (let [ws (minfo/piece->writes minfo piece-idx)]
              (doseq [w ws]
                (let [fd (or (@fds (:path w))
                             (let [[_err fd] (<! (fs/open-sesame! (:path w)))]
                               (swap! fds assoc (:path w) fd)
                               fd))]
                  (<! (fs/write fd buf (:offset w) (:length w) (:position w))))))
            (recur))))
    in))

(def t (atom {}))

(defn cross-off [idx]
  (swap! t update-in [:remaining] (partial remove #(= % idx))))

(declare choked)
(declare requesting)
(declare receiving)

(defn choked [p]
  (go (>! (:outbox @p) :interested)
      (loop []
        (when-let [msg (<! (:inbox @p))]
          (condp = (proto/msg->type msg)
            :unchoke (requesting p)
            (do (println "(choked) ignoring" msg)
                (recur)))))))

(defn requesting [p]
  (go (if (empty? (:remaining @t))
        (println "all done!")
        (let [lucky-piece (rand-nth (:remaining @t))]
          (doseq [r (minfo/piece->requests (:minfo @t) lucky-piece)]
            (>! (:outbox @p) r))
          (receiving p)))))

(defn receiving [p]
  (go (when-let [msg (<! (:inbox @p))]
        (condp = (proto/msg->type msg)
          :piece (let [[_ idx begin buf] msg]
                   (cross-off idx)
                   (>! (:disk @t) [idx buf])
                   (requesting p))
          :choke (choked p)
          (do (println "(receiving) ignoring" msg)
              (recur))))))

(defn main [torrent-path]
  (go (let [[_err, minfo-buf] (<! (fs/read-file torrent-path))
            minfo (minfo/parse minfo-buf)
            disk (piece-writer minfo)
            info-hash (:info-hash minfo)
            hosts-and-ports (tracker/peers! (:trackers minfo) info-hash)]
        (reset! t {:minfo minfo
                   :remaining (vec (range (minfo/num-pieces minfo)))
                   :disk disk})
        (dotimes [_ 20]
          (go (when-let [[host port] (<! hosts-and-ports)]
                (when-let [p (<! (peer/start! host port info-hash our-peer-id))]
                  (choked p))))))))

(set! *main-cli-fn* main)
