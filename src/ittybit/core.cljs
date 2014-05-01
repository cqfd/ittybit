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

(defn main [torrent-path]
  (go (let [[_err, minfo-buf] (<! (fs/read-file torrent-path))
            minfo (minfo/parse minfo-buf)
            pw (piece-writer minfo)
            info-hash (:info-hash minfo)
            hosts-and-ports (tracker/peers! (:trackers minfo) info-hash)
            state (atom {:free (vec (range (minfo/num-pieces minfo)))
                         :taken []})]
        (println (:piece-length minfo))
        (dotimes [_ 20]
          (go
           (when-let [[host port] (<! hosts-and-ports)]
             (when-let [p (<! (peer/start! host port info-hash our-peer-id))]
               (>! (:outbox @p) :interested)
               (loop []
                 (when-let [msg (<! (:inbox @p))]
                   (condp = (proto/msg->type msg)
                     :unchoke
                     (let [piece-idx (first (:free @state))]
                       (swap! state (fn [{:keys [free taken]}]
                                      {:free (vec (next free))
                                       :taken (conj taken piece-idx)}))
                       (doseq [req (minfo/piece->requests minfo piece-idx)]
                         (>! (:outbox @p) req)))
                     :piece
                     (let [[_ idx begin buf] msg]
                       (>! pw [idx buf])
                       (let [piece-idx (first (:free @state))]
                         (swap! state (fn [{:keys [free taken]}]
                                        {:free (vec (next free))
                                         :taken (conj taken piece-idx)}))
                         (doseq [req (minfo/piece->requests minfo piece-idx)]
                           (>! (:outbox @p) req))))

                     (println "ignoring" msg))
                   (recur))))))))))

(set! *main-cli-fn* main)
