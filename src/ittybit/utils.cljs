(ns ittybit.utils
  (:require [cljs.nodejs :as n]))

(def crypto (n/require "crypto"))

(defn sha1 [stuff]
  (.. crypto (createHash "sha1") (update stuff) digest))
