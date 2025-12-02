(require '[clojure.string :as str])

(defn parse [input] (map #(str/split % #"-") (str/split input #",")))
(defn is-invalid [s]
  (let [mid (/ (count s) 2)] (== 0 (compare (subs s 0 mid) (subs s mid (count s))))))
(defn check-string [s] (if (is-invalid (str s)) s 0))
(defn gen-range [l] (range (Long/parseLong (nth l 0)) (+ 1 (Long/parseLong (nth l 1)))))
(def input (slurp *in*))
(def output (reduce + (mapcat #(map check-string (gen-range %)) (parse (str/trim input)))))
(print (str output "\n"))
