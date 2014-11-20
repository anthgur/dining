(ns anthgur.dining.scraper-test
  (:require [clojure.test :refer :all]
            [anthgur.dining.scraper :as s]))

(def philly-10-27-2014
  (-> (slurp "test/html/philly-10-27-2014.clj")
      read-string))
(def scraped (s/scrape-page philly-10-27-2014))
(def menus (:menus scraped))
(def dates (s/scrape-date-urls philly-10-27-2014 "http://foodpro.unh.edu/"))
(def soups (->> (:categories (second menus))
                (filter #(= "Soup" (:name %)))
                first))

(deftest validation
  (schema.core/validate s/Day scraped))

(deftest test-menus
  (testing "proper number of meals"
      (is (= 4 (count menus))))
  (testing "proper meal names"
    (are [x y] (= x (:meal-name y))
         "Breakfast"  (first menus)
         "Lunch"      (second menus)
         "Dinner"     (nth menus 2)
         "Late Night" (nth menus 3))))

(deftest test-categories
  (is (= #{"Soup" "Salad" "Deli" "Grill" "Pizza" "Pasta" "Home Cooking"
           "Desserts" "Breads" "All Day Breakfast" "Mongolian Grill"
           "Allergen Friendly" "Bar Recipes"}
         (into #{} (map :name (:categories (second menus)))))))

(deftest test-recipes
  (is (= #{{:name "Chef's Choice Soup" :nutrition-info [:potential-allergen]}
           {:name "Eggplant & Zucchini Soup"
            :nutrition-info [:vegan :lite :gluten-friendly :stars-0]}
           {:name "Lentil Soup"
            :nutrition-info [:vegan :lite :gluten-friendly :potential-allergen :stars-2]}
           {:name "Philbrook Soup Bar" :nutrition-info nil}}
         (into #{} (:recipes soups)))))

(deftest test-dates-urls
  (is (= 11 (count dates)))
  (is (filter
       #{{:href "http://foodpro.unh.edu/shortmenu.asp?sName=University+Of+New+Hampshire+Hospitality+Services&locationNum=30&locationName=Philbrook+Dining+Hall&naFlag=1&WeeksMenus=This+Week%27s+Menus&myaction=read&dtdate=10%2F25%2F2014"
          :date "Saturday, October 25"}}
       dates)))
