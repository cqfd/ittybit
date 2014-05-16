(ns ittybit.core
  (:require-macros [cljs.core.async.macros :refer [alt! go]])
  (:require [cljs.nodejs :as n]
            [cljs.core.async :refer [<! chan close! put! take!]]
            [ittybit.bitfield :as bf]
            [ittybit.fs :as fs]
            [ittybit.metainfo :as minfo]
            [ittybit.peer :as peer]
            [ittybit.protocol :as proto]
            [ittybit.tracker :as tracker]
            [ittybit.utils :refer [sha1]]))

(n/enable-util-print!)

(def our-peer-id (js/Buffer. 20))

(defn piece-writer [minfo]
  (let [in (chan) fds (atom {})]
    (go (loop [already-written? #{}]
          (when-let [[piece-idx buf] (<! in)]
            (if (already-written? piece-idx)
              (recur already-written?)
              (let [bs (minfo/piece->io-boundaries minfo piece-idx)]
                (doseq [b bs]
                  (let [fd (or (@fds (:path b))
                               (let [[_err fd] (<! (fs/open-sesame! (:path b)))]
                                 (swap! fds assoc (:path b) fd)
                                 fd))]
                    (<! (fs/write fd buf (:offset b) (:length b) (:position b)))))
                (recur (conj already-written? piece-idx)))))))
    in))

(def t (atom {}))

(defn cross-off! [idx]
  (swap! t update-in [:bitfield] conj idx))

(defn legit? [idx buf]
  (= (sha1 buf) (minfo/piece->hash (:minfo @t) idx)))

(defn bitfield [buf]
  (let [size (minfo/num-pieces (:minfo @t))]
    (bf/Bitfield. size buf)))

(declare choked requesting receiving cleaning-up)

(defn choked [p]
  (go (>! (:outbox p) :interested)
      (loop []
        (if-let [msg (<! (:inbox p))]
          (condp = (proto/msg->type msg)
            :unchoke (requesting p)
            :bitfield (let [[_ buf] msg
                            bf (bitfield buf)]
                        (swap! t update-in [:peers (:peer-id p)] assoc :bitfield bf)
                        (recur))
            :have (let [[_ idx] msg]
                    (swap! t update-in [:peers (:peer-id p) :bitfield] conj idx)
                    (recur))
            (do (println "(choked) ignoring" msg)
                (recur)))
          (cleaning-up p)))))

(defn requesting [p]
  (go (if (bf/full? (:bitfield @t))
        (cleaning-up p)
        (let [their-bf (get-in @t [:peers (:peer-id p) :bitfield])
              our-bf (:bitfield @t)
              lucky-piece (rand-nth (vec (bf/difference their-bf our-bf)))
              pbuf (js/Buffer. (minfo/piece->length (:minfo @t) lucky-piece))
              reqs (minfo/piece->requests (:minfo @t) lucky-piece)]
          (doseq [r reqs]
            (>! (:outbox p) r))
          (receiving p lucky-piece pbuf (set reqs))))))

(defn receiving [p idx pbuf outstanding]
  (go (if (empty? outstanding)
        (do (when (legit? idx pbuf)
              (cross-off! idx)
              (>! (:disk @t) [idx pbuf]))
            (requesting p))
        (if-let [msg (<! (:inbox p))]
          (condp = (proto/msg->type msg)
            :piece (let [[_ idx begin chunk] msg
                         corresponding-req [:request idx begin (.-length chunk)]
                         outstanding' (disj outstanding corresponding-req)]
                     (. chunk (copy pbuf begin))
                     (receiving p idx pbuf outstanding'))
            :choke (choked p)
            :have (let [[_ idx] msg]
                    (swap! t update-in [:peers (:peer-id p) :bitfield] conj idx)
                    (receiving p idx pbuf outstanding))
            (do (println "(receiving) ignoring" msg)
                (receiving p idx pbuf outstanding)))
          (cleaning-up p)))))

(defn cleaning-up [p]
  (swap! t update-in [:peers] dissoc (:peer-id p)))

(defn main [torrent-path]
  (go (let [[_err, minfo-buf] (<! (fs/read-file torrent-path))
            minfo (minfo/parse minfo-buf)
            disk (piece-writer minfo)
            info-hash (:info-hash minfo)
            hosts-and-ports (tracker/peers! (:trackers minfo) info-hash)]
        (reset! t {:minfo minfo
                   :bitfield (bf/of-size (minfo/num-pieces minfo))
                   :disk disk
                   :peers {}})
        (add-watch t :status-update
                   (fn [k r o n]
                     (let [peers (vals (:peers n))
                           num-pieces (minfo/num-pieces (:minfo n))]
                       (println {:peer-count (count peers)
                                 :pieces-remaining (- num-pieces (count (:bitfield n)))}))))
        (dotimes [_ 20]
          (go (when-let [[host port] (<! hosts-and-ports)]
                (when-let [p (<! (peer/start! host port info-hash our-peer-id))]
                  (swap! t update-in [:peers] assoc (:peer-id p) p)
                  (choked p))))))))

(set! *main-cli-fn* main)
