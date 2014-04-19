(ns ittybit.metainfo
  (:require [cljs.nodejs :as n]))

(def b (n/require "bncode"))
(def crypto (n/require "crypto"))
(def url (n/require "url"))

(defn sha1 [stuff]
  (.. crypto (createHash "sha1") (update stuff) digest))

(defn tracker [minfo]
  (let [announce (aget minfo "announce")
        parsed (. url (parse (str announce)))]
    {:host (aget parsed "hostname")
     :port (aget parsed "port")}))

(defn info-hash [minfo]
  (let [info (aget minfo "info")]
    (sha1 (. b (encode info)))))

(defn split-every [n buf]
  (loop [acc []
         buf buf]
    (if (= 0 (.-length buf))
      acc
      (recur (conj acc (. buf (slice 0 n)))
             (. buf (slice n))))))

(defn piece-length [minfo]
  (-> minfo (aget "info") (aget "piece length")))

(defn pieces [minfo]
  (let [pieces-buf (-> minfo (aget "info") (aget "pieces"))]
    (split-every 20 pieces-buf)))

(defn parse [buf]
  (let [minfo (. b (decode buf))
        info (aget minfo "info")]
    {:tracker (tracker minfo)
     :info-hash (info-hash minfo)
     :piece-length (piece-length minfo)
     :pieces (pieces minfo)}))
