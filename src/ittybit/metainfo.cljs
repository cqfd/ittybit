(ns ittybit.metainfo
  (:require [cljs.nodejs :as n]
            [ittybit.utils :refer [sha1]]))

(def b (n/require "bncode"))
(def url (n/require "url"))

(defn trackers [decoded]
  (let [announce (aget decoded "announce")
        announce-list (apply concat (aget decoded "announce-list"))
        urls (map str (cons announce announce-list))]
    (map (fn [u]
           (let [parsed (. url (parse u))]
             [(aget parsed "hostname") (aget parsed "port")]))
         (filter #(re-find #"udp://" %) urls))))

(defn info-hash [decoded]
  (let [info (aget decoded "info")]
    (sha1 (. b (encode info)))))

(defn piece-length [decoded]
  (-> decoded (aget "info") (aget "piece length")))

(defn piece-hashes [decoded]
  (-> decoded (aget "info") (aget "pieces")))

(defn files [decoded]
  (let [info (aget decoded "info")
        name (aget info "name")]
    (if-let [fs (aget info "files")]
      (first (reduce (fn [[acc start] f]
                       (let [end (+ start (aget f "length"))]
                         [(conj acc {:path (mapv str (cons name (aget f "path")))
                                     :start start
                                     :end end})
                          end]))
                     [[] 0]
                     fs))
      [{:path [name]
        :start 0
        :end (aget info "length")}])))

(defn parse [buf]
  (let [decoded (. b (decode buf))
        fs (files decoded)]
    {:trackers (trackers decoded)
     :files fs
     :info-hash (info-hash decoded)
     :length (:end (last fs))
     :piece-hashes (piece-hashes decoded)
     :piece-length (piece-length decoded)}))

(defn num-pieces
  [{:keys [piece-hashes] :as minfo}]
  (/ (.-length piece-hashes) 20))

(defn piece-hash
  [{:keys [piece-hashes] :as minfo} piece-index]
  (. piece-hashes (slice (* 20 piece-index) (* 21 piece-index))))

(defn chunks [length chunk-size]
  (loop [offset 0 acc []]
    (if (>= (+ offset chunk-size) length)
      (conj acc [offset (- length offset)])
      (recur (+ offset chunk-size) (conj acc [offset chunk-size])))))

(assert (= (chunks 10 3) [[0 3] [3 3] [6 3] [9 1]]))
(assert (= (chunks 20 5) [[0 5] [5 5] [10 5] [15 5]]))

(def CHUNK-SIZE 16384)

(defn piece->start [minfo piece-idx]
  (* piece-idx (:piece-length minfo)))
(defn piece->end [minfo piece-idx]
  (min (:length minfo)
       (* (inc piece-idx) (:piece-length minfo))))

(defn piece->requests
  "Calculate the requests necessary to download a particular piece."
  [{:keys [length piece-length] :as minfo} piece-idx]
  (let [start (piece->start minfo piece-idx)
        end (piece->end minfo piece-idx)]
    (map (fn [[offset chunk-size]]
           [:request piece-idx offset chunk-size])
         (chunks (- end start) CHUNK-SIZE))))

(defn piece->writes
  [minfo piece-idx]
  (let [p-start (piece->start minfo piece-idx)
        p-end (piece->end minfo piece-idx)]
    (filter (fn [{:keys [length]}]
              (> length 0))
            (map (fn [f]
                   (let [start (max p-start (:start f))
                         end (min p-end (:end f))]
                     {:path (:path f)
                      :offset (- start p-start)
                      :length (- end start)
                      :position (- start (:start f))}))
                 (:files minfo)))))
