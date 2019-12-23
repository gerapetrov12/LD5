(use 'clojure.core)
(require '[clojure.string :as str])


(defn encrypt [msg key]
      (str/join
        (for [[pos char]
              (sort-by first (get-encrypt-chars msg key)
                       char)])))


(defn decrypt [msg key]
      (str/join
        (for [[pos ch]
              (sort-by first
                       (for [[index char] (get-decrypt-chars msg key)]
                            [(get_second_string_index index (count msg)) char]))]
             ch)))


(defn validation [msg key]
      (if (re-matches #"^[A-Za-z\s_]+$" msg)
        (> key 1)
        false))



(defn replacement [msg]
      (str/replace msg #" " "_"))
      


(defn index_calculation [width height]
      (def row 0)
      (def direction-down false)
      (vec
        (for [col (range 0 width)] (do
                                     (if (or (= 0 row) (= row (- height 1)))
                                       (def direction-down (not direction-down)))
                                     (def matrix-index (+ (* row width) col))
                                     (def row
                                       (if (true? direction-down)
                                         (inc row)
                                         (dec row)))
                                     matrix-index))))


(defn get_second_string_index [cell key]
      (mod cell key))
      


(defn get-encrypt-chars [msg key]
      (zipmap
        (index_calculation (count msg) key)
        (vec msg)))
      


(defn get-decrypt-chars [msg key]
      (zipmap
        (sort (index_calculation (count msg) key))
        (vec msg)))

      
(defn encrypt-msg [msg key]
      (if (validation msg key)
        (encrypt (replacement msg) key)
        (str "ERROR!")))


(defn decrypt-msg [msg key]
      (if (validation msg key)
        (decrypt (replacement msg) key)
        (str "ERROR!")))