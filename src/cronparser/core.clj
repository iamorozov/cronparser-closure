(ns cronparser.core)

(require '[clojure.string :as str])
(require '[clojure.core.match :refer [match]])


(defn extract-step
  [expr]

  (let [[part step] (str/split expr #"/")]
    [part (Integer/parseInt (or step "1"))]))

(defn expand-parts
  [expr, step]

  (->> (str/split expr #",")
       (map #(str/split % #"-"))
       (map #(map (fn [i] (Integer/parseInt i)) %))
       (map #(match [%]
                    [([start end] :seq)] (range start (+ 1 end) step)
                    [x] x))
       (reduce concat)))

(defn expand
  [expr, from, to]

  (let [[part step] (extract-step expr)]
    (if (= "*" part)
      (range from to step)
      (expand-parts part step))))

(defn parse
  [cron]

  (let [[minutes hours days months weekdays & command] (str/split cron #" ")]

    (str/join ["Minutes: " (str/join " " (expand minutes 0 60)) "\n"
               "Hours: " (str/join " " (expand hours 0 24)) "\n"
               "Days: " (str/join " " (expand days 1 32)) "\n"
               "Months: " (str/join " " (expand months 0 12)) "\n"
               "Weekday: " (str/join " " (expand weekdays 0 7)) "\n"
               "Command: " (str/join " " command)])))


(println (parse "5 4,10-18/3 */2 1-12/3 1,2 /usr/bin/foo -l -n"))

;"Minutes: 5
; Hours: 4 10 13 16
; Days: 1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31
; Months: 1 4 7 10
; Weekday: 1 2
; Command: /usr/bin/foo -l -n"