(require '[clojure.string :as str])

(defn parse [input] (map #(str/split % #"-") (str/split input #",")))
(defn is-equal [l]
  (if
    (empty? l)
    false
    (let [h (nth l 1)]
       (every?
        #(= h %)
        l))))
(defn is-invalid [s]
  (let [mid (/ (count s) 2)]
    (some true? (map
     #(is-equal (re-seq (re-pattern (format ".{1,%d}" %)) s))
     ;; #(== 1 (count (keys (group-by hash (re-seq (re-pattern (format ".{1,%d}" %)) s)))))
     (range 1 (+ 1 mid))))))
(defn check-string [s] (if (is-invalid (str s)) s 0))
(defn gen-range [l] (range (Long/parseLong (nth l 0)) (+ 1 (Long/parseLong (nth l 1)))))
(def input (slurp *in*))
(def output (reduce + (mapcat #(map check-string (gen-range %)) (parse (str/trim input)))))
(print (str output "\n"))
