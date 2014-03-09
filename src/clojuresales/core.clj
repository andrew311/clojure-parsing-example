(ns clojuresales.core
  (:gen-class))

(use 'clojure.java.io)
(use '[clojure.string :only (split)])

(defn mapvalues [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn splitline [s] 
  (split s #"\t")
  )

(defn remove-empty-strings [list]
  (filter
    (fn [str] (not= 0 (count str)))
    list
    )
  )

(defn tomap [consume-result]
  (apply hash-map consume-result)
)

(defn size-is-two [elems] 
  (= 2 (count elems)))

(defn only-pairs [list] (filter size-is-two list))

(defn consumefile [fname]
  (with-open [rdr (reader fname)]
    (doall (only-pairs (map splitline (line-seq rdr))))
  )
)

(defn productname [pair] (first pair))
(defn categoryname [pair] (second pair))
(defn sum-product-sales [pair-list] (reduce + (map #(Float/parseFloat %) (remove-empty-strings (map second pair-list)))))

(def sales (only-pairs (consumefile "/Users/andrew/tmp/sales.txt")))
(def products (only-pairs (consumefile "/Users/andrew/tmp/products.txt")))

(def products-to-category (apply hash-map (flatten products)))
(def product-to-sales-total (mapvalues (group-by productname sales) sum-product-sales))
(def category-to-product-pair-list (mapvalues (group-by categoryname products) distinct))
(def category-to-product-list (mapvalues (group-by categoryname products) #(map first %)))

(defn get-sales-total [product] (get product-to-sales-total product))
(def category-to-product-sales (mapvalues category-to-product-list (fn [products] (map get-sales-total products))))
(def category-to-total (mapvalues category-to-product-sales (fn [sales] (reduce + (filter #(false? (nil? %)) sales )))))

(defn in-candy [product-sales-pair] (= "Candy" (get products-to-category (first product-sales-pair))))

(def categories-sorted-by-sales-desc (reverse (sort-by second category-to-total )  ))
(def products-sorted-by-sales-desc (reverse (sort-by second product-to-sales-total )  ))
(def top-candy (take 1 (filter in-candy products-sorted-by-sales-desc)))
(def top-categories (take 5 categories-sorted-by-sales-desc))

(defn -main
  [& args]


  (println "top categories:")
  (doseq [i top-categories] (println i))

  (println "top candies:")
  (doseq [i top-candy] (println i))
)

  ; (println "Hello, World!"))
