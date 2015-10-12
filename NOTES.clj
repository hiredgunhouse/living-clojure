;; lists
;; vectors
;; maps
;; sets

;; '( items ) - creates a list
;; '() - empty list
;; '(1 2 "jam" :marmalade)
;; '(1, 2, "jam", :marmalade) - you can use comma but you can also ommit it and use whitespace instead
'(1 2 3 4 5)

(first '(:rabbit :pocket-watch :marmalade :door))
(rest '(:rabbit :pocket-watch :marmalade :door))
(first (rest '(:rabbit :pocket-watch :marmalade :door)))

(cons 5 '())
(cons 5 nil)

'(1 2 3 4 5)
(list 1 2 3 4 5)

;; [ items ] - creates a vector
;; [:jar1 1 2 3 :jar2] - vector
;; first - works on vectors too
;; rest - works on vectors too
;; nth - access vector element at a given index
(nth [:jar1 1 2 3 :jar2] 0)
;; -> :jar1
(nth [:jar1 1 2 3 :jar2] 2)
;; -> 2
;; last - gets last element of a list/vector
(last [:rabbit :pocket-watch :marmalade])
;; -> :marmalade
(last '(:rabbit :pocket-watch :marmalade))
;; -> :marmalade

;; count - returns element count in collection
(count [1 2 3 4])
;; -> 4

;; conj - adds one or more elements to a collection (in the most natural way for given collection type!)
;; for vectors it adds elements at the end
(conj [:toast :butter] :jam)
;; -> [:toast :butter :jam]
(conj [:toast :butter] :jam :honey)
;; -> [:toast :butter :jam :honey]

;; for lists it adds elements at the beginning
(conj '(:toast :butter) :jam)
;; -> (:jam :toast :butter)
(conj '(:toast :butter) :jam :honey)
;; -> (:honey :jam :toast :butter)

;; { key value, key value, ... } - map
;; {:jam1 "strawberry", :jam2 "blackberry"} - you can use comma
;; {:jam1 "strawberry" :jam2 "blackberry"} - or not use comma

;; get - gets a value for a specific key
(get {:jam1 "strawberry" :jam2 "blackberry"} :jam2)
;; -> "blackberry"
(get {:jam1 "strawberry" :jam2 "blackberry"} :jam3 "not found")
;; -> "not found"

;; when using keyword as a key you can use this syntax
(:jam2 {:jam1 "strawberry" :jam2 "blackberry" :jam3 "marmalade"})
;; -> "blackberry"

;; keys - retuns keys for map elements
(keys {:jam1 "strawberry" :jam2 "blackberry" :jam3 "marmalade"})
;; -> :jam1 :jam2 :jam3

;; vals - retuns values for map elements
(vals {:jam1 "strawberry" :jam2 "blackberry" :jam3 "marmalade"})
;; -> "strawberry" "blackberry" "marmalade"


;; assoc - associates the new key-value pairs to map
(assoc {:jam1 "red" :jam2 "black"} :jam1 "orange")
;; -> {:jam2 "black", :jam1 "orange"}
(assoc {:jam1 "red" :jam2 "black"} :jam3 "orange")
;; -> {:jam3 "orange", :jam2 "black", :jam1 "red"}

;; dissoc - returns a new map with the key-value pair removed
(dissoc {:jam1 "strawberry" :jam2 "blackberry"} :jam1)
;; -> {:jam2 "blackberry"}
(dissoc {:jam1 "strawberry" :jam2 "blackberry"} :jam3)
;; -> {:jam2 "blackberry" :jam1 "strawberry"}

;; merge - merge key-value pairs from one map to another
(merge {:jam1 "red" :jam2 "black"}
       {:jam1 "orange" :jam3 "red"}
       {:jam4 "blue"})
;; -> {:jam4 "blue", :jam3 "red", :jam2 "black", :jam1 "orange"}

;; #{ items } - sets
#{:red :blue :white :pink}
;; -> #{:white :red :blue :pink}
;; #{:red :blue :white :pink :pink}
;; -> IllegalArgumentException Duplicate key :pink

;; union - set union
(clojure.set/union #{:r :b :w} #{:w :p :y})
;; -> #{:y :r :w :p}

;; difference - set difference
(clojure.set/difference #{:r :b :w} #{:w :p :y})
;; -> #{:r :b}

;; intersection - set intersection
(clojure.set/intersection #{:r :b :w} #{:w :p :y})
;; -> #{:w}

;; set - convert another type of collection to a set
(set [:rabbit :rabbit :watch :door])
;; -> #{:door :watch :rabbit}
(set {:a 1 :b 2 :c 3})
;; -> #{[:c 3] [:b 2] [:a 1]}

(get #{:rabbit :door :watch} :rabbit)
;; -> :rabbit
(get #{:rabbit :door :watch} :jar)
;; -> nil
(:rabbit #{:rabbit :door :watch})
;; -> :rabbit
(#{:rabbit :door :watch} :rabbit)
;; -> :rabbit

(contains? #{:rabbit :door :watch} :rabbit)
;; -> true

(contains? #{:rabbit :door :watch} :jam)
;; -> false

;; add element to set
(conj #{:rabbit :door} :jam)
;; -> #{:rabbit :door :jam}

;; remove element from set
(disj #{:rabbit :door} :door)
;; -> #{:rabbit}

;; Strings
;; Integers
;; Ratios
;; Decimals
;; Keywords
;; Characters
;; Booleans

'(+ 1 1)
;; -> (+ 1 1)
(first '(+ 1 1))
;; -> +

(def developer "Alice")
;; -> #'user/developer

developer
;; -> "Alice"

user/developer
;; -> "Alice"

(let [developer "Alice in Wonderland"]
  developer)
;; -> "Alice in Wonderland"

developer
;; -> "Alice"

(let [developer "Alice in Wonderland"
      rabbit "White Rabbit"]
  [developer rabbit])
;; -> ["Alice in Wonderland" "White Rabbit"]

rabbit
;; -> CompilerException java.lang.RuntimeException:
;; -> Unable to resolve symbol: rabbit in this context

(defn follow-the-rabbit [] "Of we go!")
;; -> #'user/follow-the-rabbit

(follow-the-rabbit)
;; -> "Off we go!"

(defn shop-for-jams [jam1 jam2]
  {:name "jam-basket"
   :jam1 jam1
   :jam2 jam2})
;; -> #'user/shop-for-jams

(shop-for-jams "strawberry" "marmalade")
;; -> {:name "jam-basket", :jam1 "strawberry", :jam2 "marmalade"}

(keys (shop-for-jams "strawberry" "marmalade"))
;; -> (:name :jam1 :jam2)
(vals (shop-for-jams "strawberry" "marmalade"))
;; -> ("jam-basket" "strawberry" "marmalade")

;; anonymous functions

;; returns back a function
(fn [] (str "Off we go" "!"))
;; -> #<user$eval790$fn__791 user$eval790$fn__791@2ecd16a2>

;; invode with parens
((fn [] (str "Off we go!" "!")))
;; -> "Off we go!"

(def follow-again (fn [] (str "Off we go" "!")))
;; -> #'user/follow-again

(follow-again)
;; -> "Off we go!"

;; shortuct form of anonymous function
(#(str "Off we go" "!"))
;; -> "Off we go!"

;; if there's one parameter you can use the percent sign (%)
(#(str "Off we go" "!" " - " %) "again")
;; -> "Off we go! - again"

;; with multiple parameters you can use %1 %2, etc.
(#(str "Off we go" "!" " - " %1 %2) "again" "?")
;; -> "Off we go! - again?"

;; namespaces

(ns alice.favfoods)
;; -> nil

*ns*
;; -> #<Namespace alice.favfoods>

(def fav-food "strawberry jam")
;; -> #'alice.favfoods/fav-food

fav-food
;; -> "strawberry jam"

;; access via fully qualified namespace
alice.favfoods/fav-food
;; -> "strawberry jam"

(ns rabbit.favfoods)
;; -> nil

fav-food
;; -> CompilerException java.lang.RuntimeException:
;; -> Unable to resolve symbol: fav-food in this context

(def fav-food "lettuce soup")
;; -> #'rabbit.favfoods/fav-food

fav-food
;; -> "lettuce soup"

alice.favfoods/fav-food
;; -> "strawberry jam"

;; Union
(clojure.set/union #{:r :b :w} #{:w :p :y})
;; -> #{:y :r :w :b :p}

;; load a namespace (clojure.set is auto-required into usr namespace with the REPL starts up)
(require 'clojure.set)

(ns wonderland)
;; -> nil

;; using an alias
(require '[alice.favfoods :as af])
;; -> nil

af/fav-food
;; -> "strawberry jam"

(ns wonderland
  (:require [alice.favfoods :as af]))

af/fav-food
;; -> "strawberry jam"

(ns wonderland
  (:require [alice.favfoods :refer :all]
            [rabbit.favfoods :refere :all]))
;; -> Exception:
;;    fav-food already referes to: #'alice.favfoods/fav-food
;;    in namespace: wonderland

(ns wonderland
  (:require [clojure.set :as s]))

(defn common-fav-foods [foods1 foods2]
  (let [food-set1 (set foods1)
        food-set2 (set foods2)
        common-foods (s/intersection food-set1 food-set2)]
    (str "Common Foods: " common-foods)))

(common-fav-foods [:jam :brownies :toast]
                  [:lettuce :carrots :jam])
;; ->  "Common Foods: #{:jam}"

(class true)
;; -> java.lang.Boolean

(true? true)
;; -> true

(true? false)
;; -> false

(false? false)
;; -> true

(false? true)
;; -> false

(nil? nil)
;; -> true

(nil? 1)
;; -> false

(not true)
;; -> false

(not false)
;; -> true

;; nil is treated as logically false in some logical tests
(not nil)
;; -> true

(not "hi")
;; -> false

;; = is the same as Java's equals method
(= :drinkme :drinkme)
;; -> true

(= :drinkme 4)
;; -> false

;; collection equality is special
(= '(:drinkme :bottle) [:drinkme :bottle])
;; -> true

;; not= is a shortcut for doing (not (= x y))
(not= :drinkme :4)
;; -> true

(empty? [:table :door :key])
;; -> false

(empty? [])
;; -> true

(empty? {})
;; -> true

(empty? '())
;; -> true

(seq [1 2 3])
;; -> (1 2 3)

(class [1 2 3])
;; -> clojure.lang.PersistentVector

(class (seq [1 2 3]))
;; -> clojure.lang.PersistentVector$ChunkedSeq

(seq [])
;; -> nil

(empty? [])
;; -> true

;; use this to check for not empty
(seq [])
;; -> nil

(every? odd? [1 3 5])
;; -> true

(every? odd? [1 2 3 4 5])
;; -> false

(defn drinkable? [x]
  (= x :drinkme))
;; -> #'user/drinkable?

(every? drinkable? [:drinkme :drinkme])
;; -> true

(every? drinkable? [:drinkme :poison])
;; -> false

(every? (fn [x] (= x :drinkme)) [:drinkme :drinkme])
;; -> true

(every? #(= % :drinkme) [:drinkme :drinkme])
;; -> true

(not-any? #(= % :drinkme) [:drinkme :drinkme])
;; -> false

(not-any? #(= % :drinkme) [:poison :poison])
;; -> true

(some #(> % 3) [1 2 3 4 5])
;; -> true

;; set is a function of its member
(#{1 2 3 4 5} 3)
;; -> 3

(some #{3} [1 2 3 4 5])
;; -> 3

(some #{4 5} [1 2 3 4 5])
;; -> 4

(some #{nil} [nil nil nil])
;; -> nil

(some #{false} [false false false])
;; -> nil

(if true "it is true" "it is false")
;; -> "it is true"

(if false "it is true" "it is false")
;; -> "it is false"

(if nil "it is true" "it is false")
;; -> "it is false"

(if (= :drinkme :drinkme)
  "Try it"
  "Dont try it")
;; -> "Try it"

(let [need-to-grow-small (> 5 3)]
  (if need-to-grow-small
    "drink bottle"
    "don't drink bottle"))
;; -> "drink bottle"

(if-let [need-to-grow-small (> 5 1)]
  "drink bottle"
  "don't drink bottle")
;; -> "drink bottle"

(if-let [need-to-grow-small (> 5 1)]
  ["drink bottle" need-to-grow-small]
  "don't drink bottle")
;; -> ["drink bottle" true]

(defn drink [need-to-grow-small]
  (when need-to-grow-small "drink bottle"))
;; -> #'user/drink

(drink true)
;; -> "drink bottle"

(drink false)
;; -> nil

(when-let [need-to-grow-small true]
  "drink bottle")
;; -> "drink bottle"

(when-let [need-to-grow-small false]
  "drink bottle")
;; -> nil

(let [bottle "drinkme"]
  (cond
    (= bottle "poison") "don't touch"
    (= bottle "drinkme") "grow smaller"
    (= bottle "empty") "all gone"))

;; -> "grow smaller"

(let [x 5]
  (cond
    (> x 10) "bigger than 10"
    (> x 4) "bigger than 4"
    (> x 3) "bigger than 3"))
;; -> "bigger than 4"

(let [x 5]
  (cond
    (> x 3) "bigger than 3"
    (> x 10) "bigger than 10"
    (> x 4) "bigger than 4"))
;; -> "bigger than 3"

(let [x 1]
  (cond
    (> x 3) "bigger than 3"
    (> x 10) "bigger than 10"
    (> x 4) "bigger than 4"))
;; -> nil

(let [bottle "mystery"]
  (cond
    (= bottle "poison") "don't touch"
    (= bottle "drinkme") "grow smaller"
    (= bottle "empty") "all gone"
    :else "unknown"))
;; -> "unknown"

(let [bottle "mystery"]
  (cond
    (= bottle "poison") "don't touch"
    (= bottle "drinkme") "grow smaller"
    (= bottle "empty") "all gone"
    "default" "unknown"))
;; -> "unknown"

(let [bottle "drinkme"]
  (case bottle
    "poison" "don't touch"
    "drinkme" "grow smaller"
    "empty" "all gone"))
;; -> "grow smaller"

(let [bottle "mystery"]
  (case bottle
    "poison" "don't touch"
    "drinkme" "grow smaller"
    "empty" "all gone"))
;; -> IllegalArgumentException No matching clause: mystery

(let [bottle "mystery"]
  (case bottle
    "poison" "don't touch"
    "drinkme" "grow smaller"
    "empty" "all gone"
    "unknown"))
;; -> "unknown"

(defn grow [name direction]
  (if (= direction :small)
    (str name " is growing smaller")
    (str name " is growing bigger")))
;; -> #'user/grow

(grow "Alice" :small)
;; -> "Alice is growing smaller"

(grow "Alice" :big)
"Alice is growing bigger"

(partial grow "Alice")
;; -> #<core$partial$fn__4228 clojure.core$partial$fn__4228@641624c7>

((partial grow "Alice") :small)
;; -> "Alice is growing smaller"

(defn toggle-grow [direction]
  (if (= direction :small) :big :small))
;; -> #'user/toggle-grow

(toggle-grow :big)
;; -> :small

(toggle-grow :small)
;; -> :big

(defn oh-my [direction]
  (str "Oh My! You are growing " direction))
;; -> #'user/oh-my

(oh-my (toggle-grow :small))
;; -> "Oh My! You are growing :big"

(defn surprise [direction]
  ((comp oh-my toggle-grow) direction))
;; -> #'user/surprise

(surprise :small)
;; -> "Oh My! You are growing :big"

(defn adder [x y]
  (+ x y))
;; -> #'user/adder

(adder 3 4)
;; -> 7

;; CAUTION! here we use def rather than defn
(def adder-5 (partial adder 5))
;; -> #'user/adder-5

(adder-5 10)
;; -> 15

(let [[color size] ["blue" "small"]]
  (str "The " color " door is " size))
;; -> "The blue door is small"

(let [x ["blue" "small"]
      color (first x)
      size (last x)]
  (str "The " color " door is " size))
;; -> "The blue door is small"

(let [[color [size]] ["blue" ["very small"]]]
  (str "The " color " door is " size))
;; -> "The blue door is very small"

(let [[color [size] :as original] ["blue" ["small"]]]
  {:color color :size size :original original})
;; -> {:color "blue", :size "small", :original ["blue" ["small"]]}

(let [{flower1 :flower1 flower2 :flower2}
      {:flower1 "red" :flower2 "blue"}]
  (str "The flowers are " flower1 " and " flower2))
;; -> "The flowers are red and blue"

(let [{flower1 :flower1 flower2 :flower2 :or {flower2 "missing"}}
      {:flower1 "red"}]
  (str "The flowers are " flower1 " and " flower2))
;; -> "The flowers are red and missing"

(let [{flower1 :flower1 :as all-flowers}
      {:flower1 "red"}]
  [flower1 all-flowers])
;; -> ["red {:flower1 "red"}]

(let [{:keys [flower1 flower2]}
      {:flower1 "red" :flower2 "blue"}]
  (str "The flowers are " flower1 " and " flower2))
;; -> "The flowers are red and blue"

(defn flower-colors [colors]
  (str "The flowers are "
       (:flower1 colors)
       " and "
       (:flower2 colors)))
;; -> #'user/flower-colors

(flower-colors {:flower1 "red" :flower2 "blue"})
;; -> "The flowers are red and blue"

(defn flower-colors [{:keys [flower1 flower2]}]
  (str "The flowers are " flower1 " and " flower2))
;; -> #'user/flower-colors

(flower-colors {:flower1 "red" :flower2 "blue"})
;; -> "The flowers are red and blue"


;; --- The Power of Laziness ---


(take 5 (range))
;; -> (0 1 2 3 4)

(take 10 (range))
;; -> (0 1 2 3 4 5 6 7 8 9)

(range 5)
;; -> (0 1 2 3 4)

(class (range 5))
;; -> clojure.lang.LazySeq

;; DON"T EVALUATE THIS OR YOUR REPL WILL CRASH
;;(range)

(take 10 (range))
;; -> (0 1 2 3 4 5 6 7 8 9)

(count (take 1000 (range)))
;; -> 1000

(count (take 100000 (range)))
;; -> 100000

(repeat 3 "rabbit")
;; -> ("rabbit" "rabbit" "rabbit")

(class (repeat 3 "rabbit"))
;; -> clojure.lang.LazySeq

(take 5 (repeat "rabbit"))
;; -> ("rabbit" "rabbit" "rabbit" "rabbit" "rabbit")

(count (take 5000 (repeat "rabbit")))
;; -> 5000

(rand-int 10)
;; -> 8

(rand-int 10)
;; -> 1

(repeat 5 (rand-int 10))
;; -> (2 2 2 2 2)

#(rand-int 10)
;; -> #<user$eval721$fn__722 user$eval721$fn__722@308092db>

(#(rand-int 10))
;; -> 7

(repeatedly 5 #(rand-int 10))
;; -> (1 2 5 6 9)

(take 10 (repeatedly #(rand-int 10)))
;; -> (5 1 1 0 1 7 6 7 8 4)

(take 3 (cycle ["big" "small"]))
;; -> ("big" "small" "big")

(take 6 (cycle ["big" "small"]))
;; -> ("big" "small" "big" "small" "big" "small")

(take 3 (rest (cycle ["big" "small"])))
;; -> ("small" "big" "small")


;; --- Recursion ---


["normal" "too small" "too big" "swimming"]
#(str "Alice is " %)

(def adjs ["normal"
           "too small"
           "too big"
           "is swimming"])

(defn alice-is [in out]
  (if (empty? in)
    out
    (alice-is
      (rest in)
      (conj out
            (str "Alice is " (first in))))))

(alice-is adjs [])
;; -> ["Alice is normal" "Alice is too small" "Alice is too big" "Alice is swimming"]

(defn alice-is [input]
  (loop [in input
         out []]
    (if (empty? in)
      out
      (recur (rest in)
             (conj out
                   (str "Alice is " (first in)))))))

(alice-is adjs)
;; -> ["Alice is normal" "Alice is too small" "Alice is too big" "Alice is swimming"]

(defn countdown [n]
  (if (= n 0)
    n
    (countdown (- n 1))))

(countdown 3)
;; -> 0

(countdown 100000)
;; -> java.lang.StackOverflowError

(defn countdown [n]
  (if (= n 0)
    0
    (recur (- n 1))))

(countdown 100000)
;; -> 0


;; --- The Functional Shape of Data Transformations ---


(def animals [:mouse :duck :dodo :lory :eaglet])
;; -> #'user/animals

(#(str %) :mouse)
;; -> ":mouse"

(map #(str %) animals)
;; -> (":mouse" ":duck" ":dodo" ":lory" ":eaglet")

(class (map #(str %) animals))
;; -> clojure.lang.LazySeq

(take 3 (map #(str %) (range)))
;; -> ("0" "1" "2")

(take 10 (map #(str %) (range)))
;; -> ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")


(println "Look at the mouse!")
;; Look at the mouse!
;; -> nil

(def animal-print (map #(println %) animals))
;; -> #'user/animal-print

animal-print
;; :mouse
;; :duck
;; :dodo
;; :lory
;; :eaglet
;; -> (nil nil nil nil nil)

(def animal-print (doall (map #(println %) animals)))

animal-print
;; :mouse
;; :duck
;; :dodo
;; :lory
;; :eaglet
;; #'user/animal-print

animal-print
;; -> (nil nil nil nil nil)

(def animals
  ["mouse" "duck" "dodo" "lory" "eaglet"])

(def colors
  ["brown" "black" "blue" "pink" "gold"])

(defn gen-animal-string [animal color]
  (str color "-" animal))

;; map can also take more than one collection to map agains
(map gen-animal-string animals colors)
;; -> ("brown-mouse" "black-duck" "blue-dodo" "pink-lory" "gold-eaglet")

(def colors
  ["brown" "black"])

;; map function will terminate when the shortest collection ends
(map gen-animal-string animals colors)
;; -> ("brown-mouse" "black-duck")

(map gen-animal-string animals (cycle ["brown" "black"]))o
;; -> ("brown-mouse" "black-duck" "brown-dodo" "black-lory" "brown-eaglet")


(reduce + [1 2 3 4 5])
;; -> 15

(reduce (fn [r x] (+ r (* x x))) [1 2 3])
;; -> 14

(reduce (fn [r x] (if (nil? x) r (conj r x)))
        []
        [:mouse nil :duck nil nil :lory])
;; -> [:mouse :duck :lory]

((complement nil?) nil)
;; -> false

((complement nil?) 1)
;; -> true

(filter (complement nil?) [:mouse nil :duck nil])
;; -> (:mouse :duck)

(class (complement nil?))
(class (not true))

(filter keyword? [:mouse nil :duck nil])
;; -> (:mouse :duck)

(remove nil? [:mouse nil :duck nil])
;; -> (:mouse :duck)

(for [animal [:mouse :duck :lory]]
  (str (name animal)))
;; -> ("mouse" "duck" "lory")

(def r (for [animal [:mouse :duck :lory]]
  (println (str (name animal)))))

r


(for [animal [:mouse :duck :lory]
      color  [:red :blue]]
  (str (name color) (name animal)))
;; -> ("redmouse" "bluemouse" "redduck" "blueduck" "redlory" "bluelory")

(for [animal [:mouse :duck :lory]
      color  [:red :blue]
      :let [animal-str (str "animal-" (name animal))
            color-str (str "color-" (name color))
            display-str (str animal-str "-" color-str)]]
  display-str)
;; -> ("animal-mouse-color-red" "animal-mouse-color-blue"
;;     "animal-duck-color-red" "animal-duck-color-blue"
;;     "animal-lory-color-red" "animal-lory-color-blue")

(for [animal [:mouse :duck :lory]
      color  [:red :blue]
      :let [animal-str (str "animal-" (name animal))
            color-str (str "color-"(name color))
            display-str (str animal-str "-" color-str)]
      :when (= color :blue)]
  display-str)
;; -> ("animal-mouse-color-blue" "animal-duck-color-blue" "animal-lory-color-blue")

(flatten [ [:duck [:mouse] [[:lory]]]])
;; -> (:duck :mouse :lory)

(vec '(1 2 3))
;; -> [1 2 3]

(into [] '(1 2 3))
;; -> [1 2 3]

(into '() [1 2 3])
;; -> (3 2 1)

(sorted-map :b 2 :a 1 :z 3)
;; -> {:a 1, :b 2, :z 3}

(sorted-map {:b 2 :a 1 :z 3})
;; -> java.lang.IllegalArgumentExceptio: No value supplied for key: {:b 2, :a 1, :z 3}

(class (sorted-map))
;; -> clojure.lang.PersistentTreeMap

(into (sorted-map) {:b 2 :c 3 :a 1})
;; -> {:a 1, :b 2, :c3}

(into {} [[:a 1] [:b 2] [:c 3]])
;; -> {:a 1, :b 2, :c 3}

(into [] {:a 1, :b 2, :c 3})
;; -> [[:c 3] [:b 2] [:a 1]]

(partition 3 [1 2 3 4 5 6 7 8 9])
;; -> ((1 2 3) (4 5 6) (7 8 9))

(partition 3 [1 2 3 4 5 6 7 8 9 10])
;; -> ((1 2 3) (4 5 6) (7 8 9))

(partition 3 '(1 2 3 4))
;; -> ((1 2 3))

(partition-all 3 [1 2 3 4 5 6 7 8 9 10])
;; -> ((1 2 3) (4 5 6) (7 8 9) (10))

(partition-by #(= 6 %) [1 2 3 4 5 6 7 8 9 10])
;; -> ((1 2 3 4 5) (6) (7 8 9 10))


;; --- State and Concurrency ---


(def who-atom (atom :caterpillar))

who-atom
