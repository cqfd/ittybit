(ns ittybit.protocol)

(def B js/Buffer)

(defn handshake [info-hash peer-id]
  (. B (concat #js [(B. #js [19])
                    (B. "BitTorrent protocol")
                    (B. #js [0 0 0 0 0 0 0 0])
                    info-hash
                    peer-id])))

(def keep-alive (B. #js [0 0 0 0]))
(def choke (B. #js [0 0 0 1 0]))
(def unchoke (B. #js [0 0 0 1 1]))
(def interested (B. #js [0 0 0 1 2]))
(def not-interested (B. #js [0 0 0 1 3]))
