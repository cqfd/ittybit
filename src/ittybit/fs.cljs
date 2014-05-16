(ns ittybit.fs
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as n]
            [cljs.core.async :refer [<! chan close! put!]]
            [clojure.string :as string]))

(def fs (n/require "fs"))

(defn async-fn->chan-fn [f]
  (fn [& args]
    (let [c (chan)]
      (apply f (concat args [(fn [& cb-args]
                               (put! c cb-args (fn [_] (close! c))))]))
      c)))

(def read-file (async-fn->chan-fn (aget fs "readFile")))
(def read (async-fn->chan-fn (aget fs "read")))
(def mkdir' (async-fn->chan-fn (aget fs "mkdir")))
(defn mkdir [path] (mkdir' (string/join "/" path)))
(def open' (async-fn->chan-fn (aget fs "open")))
(defn open [path flags] (open' (string/join "/" path) flags))
(def write (async-fn->chan-fn (aget fs "write")))

(defn mkdirp
  ([dirs] (mkdirp dirs []))
  ([dirs within-dirs]
     (go (if (empty? dirs)
           nil
           (let [within-dirs' (conj within-dirs (first dirs))]
             (<! (mkdir within-dirs'))
             (<! (mkdirp (next dirs) within-dirs')))))))

(defn open-sesame!
  "Just--just open the file."
  [path]
  (go (<! (mkdirp (drop-last path)))
      (<! (open path "w+"))))
