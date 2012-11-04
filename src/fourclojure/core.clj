(ns fourclojure.core)

;; util
(defmacro defmemoize [name & body]
  `(def ~name (memoize (fn ~@body))))

;;((fn [& args]
;;   (some #(and (zero? (mod % b)) %) (iterate #(+ a %) a)) 2 3))


((fn [& a]
   (let [m (apply max a)]
    (some
      (fn [x] (and (every? #(zero? (mod x %)) a) x))
      (iterate #(+ m %) m)))) 1/3 2/5)

;; todo: submit solution
(def p81-set-intersection
  #(set (filter %1 %2)))

;; todo: submit solution
(def p28-flatten
  (fn flatten1 [coll]
    (if (coll? coll)
      (mapcat flatten1 coll)
      [coll])))

;; todo: submit solution
(def p60-reductions
  (fn reductions1
    ([f init coll]
     (lazy-seq
      (when-not (empty? coll)
        (let [res (f init (first coll))]
          (cons res (reductions1 f res (rest coll)))))))
    ([f coll]
     (cons (first coll) (reductions1 f (first coll) (rest coll))))))

(def p122-fn-juxt
  (fn [s]
    (apply + (map #(* (- (int %1) (int \0)) %2) (reverse s)
                  (iterate #(* 2 %) 1)))))

(def p59-fn-juxt
  (fn [& fns]
    (fn [& args]
      (map #(apply % args) fns))))

(def p58-fn-comp
  (fn [& fn-list]
    (fn [& args]
      (first (reduce #(list (apply %2 %1)) args (reverse fn-list))))))

(def p77-anagram
  #(set (map set
             (filter (fn [x] (> (count x) 1))
                     (vals (group-by frequencies %))))))

(def p56-distinct
  #(loop [dist []
          seen #{}
          arr %]
     (if (empty? arr) dist
       (let [item (first arr)
             newarr (rest arr)]
         (if (seen item) (recur dist seen newarr)
           (recur (conj dist item) (conj seen item) newarr))))))

(defn p43-multiplex [coll x]
  (map #(map last %)
       (vals (group-by #(mod (first %) x)
                       (keep-indexed list coll)))))

(defn fact [n]
  (reduce #(* % (* %2 %2)) 0 (range (inc n))))

(defn p120 [coll]
   (count (filter identity
                  (map #(< % (reduce (fn [sum x] (+ sum (* x x)))
                      0
                      (for [character (str %)]
                        (Integer/parseInt (str character))))) coll))))

(def b
  #(loop [a % l []]
  (if (empty? a) l
  (recur
    (rest a)
    (conj l (first a))))))
;;
;;41
#(loop [a [] coll %1]
  (if (empty? coll) a)
  (recur (conj a (take %2 coll)) (drop (inc %2) coll)))

;; #50
(fn [_x_] (vals (reduce (fn [c x] (update-in c [(class x)] (fnil #(conj % x) []))) {} _x_)))

;; old shit
(comment
(fn pascal [n]
  (if (<= n 1) [1]
    (into []
          (flatten
            (list 1
                  (map #(apply + %) (partition 2 1 (pascal (dec n))))
                  1)))))

(fn testfn [sum x]
   (if (<= (+ (apply + (flatten sum)) x) 10)
     (conj sum x) sum))


;;(fn flatten [coll]
;;  (conj (rest coll)
;;
(comment
((fn [coll]
  (loop [l coll]
    (let [r (rest l)]
      (if (nil? )
        (peek r)
        (recur r))))) [1 2 3 4 5]))

((fn [coll]
  (loop [l coll]
    (let [r (rest l)]
      (if (= 1 (count r))
        (first l)
        (recur r))))) [1 2 3 4 5])

(comment
((fn my-flatten [coll]
   (loop [cur (first coll) left '() right (rest coll)]
     (if (empty? right)
       left
       (if (coll? cur)
         (my-flatten (first right) (rest right))
         (my-flatten (concat left (list (first right))) (rest right))))))
   [1 2 [3 4] 5 6]))

;; http://www.4clojure.com/problem/95
;; this seems to work but fails a unit test on the site
;; (is-tree? [1 nil [2 [3 nil nil] [4 nil nil]]])
(fn is-tree? [[root left right :as l]]
  (if (nil? l) true
    (if (not (and (seq? l) (= 3 (count l)))) false
      (if (nil? root)
        false
        (and (is-tree? left) (is-tree? right))))))
  )


(fn [x c]
  (loop [c c lst ()]
    (if (> x (count c)) (reverse lst)
      (recur (drop x c) (conj lst (take x c))))))

;;(__ #(* % %) #{-2 -1 0 1 2})


(defn prob98 [f s]
  (group-by f s))

;; 171 Intervals
(fn [col]
  (->> col
    set
    (#(map % (when-not (empty? col) (range (apply min col) (inc (apply max col))))))
    (partition-by nil?)
    (map #(when (first %) (vector (first %) (last %))))
    (filter identity)))

;; but, resolve is banned
(defn eval-helper [bindings form_]
  (if (seq? form_)
    (apply (resolve (first form_)) (map #(eval-helper bindings %) (rest form_)))
    (get bindings form_ form_)))

;; #121 Universal Computation Engine
(fn [form]
  (fn [bindings]
    ((fn eval-helper [bindings form_]
       (if (seq? form_)
         (apply (resolve (first form_)) (map #(eval-helper bindings %) (rest form_)))
         (get bindings form_ form_))) bindings form)))

(defn uce [form]
  (fn [bindings]
    ((fn eval-helper [bindings form_]
       (if (seq? form_)
         (map #(eval-helper bindings %) form_)
         (get bindings form_ form_))) bindings form)))

;; project euler prob 31
;; assumes coins are sorted
(defmemoize largest-coin [amt coins]
            (last (take-while #(<= % amt) coins)))

(defmemoize coin-subset [limit-exclusive coins]
            (take-while #(< % limit-exclusive) coins))

(defmemoize ways-to-make [amt coins]
            (if-let [largest (get-largest-coin amt coins)]
              (+ (if (zero? (mod amt largest)) 1 0)
                 (apply + (map #(ways-to-make % (coin-subset largest coins))
                               (range amt 0 (- largest)))))
              0))

(defn solve-31 []
  (ways-to-make 200 [1 2 5 10 20 50 100 200]))

;; #70 Word Sorting
(fn [words]
  (sort #(compare (.toLowerCase %1) (.toLowerCase %2))
        (map #(apply str %)
             (filter #(not (= '(\space) %))
                     (partition-by #(= % \space) (.replaceAll words "[.?!]" ""))))))

;; I learned from reading other people's solutions that clojure.string is auto-required. given that,
(fn [s] (sort-by #(.toUpperCase %) (clojure.string/split s #"\W+")))
;; is a much shorter solution.
