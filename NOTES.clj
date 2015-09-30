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
