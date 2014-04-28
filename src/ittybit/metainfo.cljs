(ns ittybit.metainfo
  (:require [cljs.nodejs :as n]))

(def b (n/require "bncode"))
(def crypto (n/require "crypto"))
(def url (n/require "url"))

(defn sha1 [stuff]
  (.. crypto (createHash "sha1") (update stuff) digest))

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
  (let [info (aget decoded "info")]
    (if-let [fs (aget info "files")]
      (for [f fs]
        {:path (vec (aget f "path"))
         :length (aget f "length")})
      [{:path [(aget info "name")]
        :length (aget info "length")}])))

(defn length
  [decoded]
  (let [fs (files decoded)]
    (apply + (map :length fs))))

(defn parse [buf]
  (let [decoded (. b (decode buf))]
    {:trackers (trackers decoded)
     :files (files decoded)
     :info-hash (info-hash decoded)
     :length (length decoded)
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

(defn piece->requests
  "Calculate the requests necessary to download a particular piece."
  [{:keys [length piece-length] :as minfo} piece-index]
  (let [start (* piece-length piece-index)
        end (min (dec length) (+ start piece-length))]
    (map (fn [[offset chunk-size]]
           [:request piece-index offset chunk-size])
         (chunks (- end start) CHUNK-SIZE))))

