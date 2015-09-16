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

'(+ 1 1)
