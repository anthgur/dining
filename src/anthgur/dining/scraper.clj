(ns anthgur.dining.scraper
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as st
             :refer [split trim
                     capitalize lower-case]]
            [schema.core :as s]))

(declare scrape-location
         scrape-date scrape-menu
         extract-menu-list-tables)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schemas
(def Recipe
  {(s/required-key :name) s/Str
   (s/required-key :nutrition-info) [s/Keyword]})

(def Category
  {(s/required-key :name) s/Str
   (s/required-key :recipes) [Recipe]})

(def Menu
  {(s/required-key :meal-name) s/Str
   (s/required-key :categories) [Category]})

(def Day
  {(s/required-key :date) s/Str
   (s/required-key :location) s/Str
   (s/required-key :menus) [Menu]})

(def DayUrl
  {(s/required-key :date) s/Str
   (s/required-key :href) s/Str})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
(s/defn scrape-page :- Day
  [html-string :- s/Str]
  {:date (scrape-date html-string)
   :location (scrape-location html-string)
   :menus (map scrape-menu (extract-menu-list-tables html-string))})

(s/defn scrape-date-urls :- DayUrl
  [html-string base-url]
  (let [menu-anchors (html/select html-string [:td.datebody :a])
        get-url (comp #(str base-url %) :href :attrs)
        get-content (comp first :content)]
    (->> (map get-url menu-anchors)
         (interleave (map get-content menu-anchors))
         (partition 2)
         (remove #(some nil? %))
         (map #(zipmap [:date :href] %))
         butlast)))

;; TODO implement fetching to catch net errors
(defn get-page
  [url-str]
  (-> (java.net.URL. url-str)
      html/html-resource))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
(def menu-list-table-attrs
  {:border "0"
   :width "100%"
   :height "100%"
   :cellpadding "0"
   :cellspacing "0"})

(def nutrition-legend
  {"LegendImages/gs0star_sm-transparent.gif" :stars-0
   "LegendImages/gs1star_sm.jpg" :stars-1
   "LegendImages/gs2star_sm.jpg" :stars-2
   "LegendImages/gs3star_sm.jpg" :stars-3
   "LegendImages/gf.gif" :gluten-friendly
   "LegendImages/Lite-transparent.gif" :lite
   "LegendImages/V-transparent.gif" :vegetarian
   "LegendImages/Lite-leaf-transparent.gif" :vegan
   "LegendImages/Allergen-web-transparent.gif" :potential-allergen})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extraction functions for isolating elements
(defn extract-menu-list-tables
  [html-string]
  (let [tables (html/select html-string [:td :> :table])]
    (filter #(= menu-list-table-attrs (:attrs %)) tables)))

(defn extract-menu-trs
  [menu-list-table]
  (let [outer-td (->> (:content menu-list-table)
                      (filter map?)
                      second :content
                      (filter map?)
                      first)]
    (->> (:content outer-td)
         (filter map?)
         first :content
         (filter map?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scraping/formatting functions
(defn scrape-date
  [html-string]
  (-> (html/select html-string [:div.shortmenutitle])
      first :content first
      (st/replace #"Menus for " "")))

(defn scrape-location
  [html-string]
  (-> (html/select html-string [:div.shortmenuheader])
      first :content first))

(defn scrape-meal-name
  [menu-list-table]
  (-> (html/select menu-list-table [:div.shortmenumeals])
      first
      :content
      first))

(defn scrape-recipe
  [menu-tr]
  (let [recipe (html/select menu-tr [:div.shortmenurecipes])]
    (if (seq recipe)
      (-> (first recipe)
          :content first
          :content first
          :content first))))

(defn scrape-nutrition-info
  [menu-tr]
  (let [imgs (html/select menu-tr [:img])]
    (if (seq imgs)
      (->> (map (comp nutrition-legend :src :attrs) imgs)
           (remove nil?)))))

(defn format-category
  [raw-category]
  (let [cap-firsts
        #(apply str
                (first (capitalize (first %)))
                (rest %))
        lc-words
        (-> (st/replace raw-category #"-" "")
            trim lower-case)]
    (->> (split lc-words #" ")
         (map cap-firsts)
         (interpose " ")
         (apply str))))

(defn scrape-category
  [menu-tr]
  (let [category (html/select menu-tr [:div.shortmenucats])]
    (if (seq category)
      (-> (first category)
          :content first
          :content first))))

(s/defn scrape-menu :- Menu
  [menu-list-table]
  (let [make-recipe
        (fn [r n]
          {:name r
           :nutrition-info (scrape-nutrition-info n)})
        scraped
        (reduce
         (fn [[sections section] tr]
           (if-let [recipe (scrape-recipe tr)]
             [sections
              (assoc section :recipes
                     (conj (:recipes section)
                           (make-recipe recipe tr)))]
             [(conj sections section)
              {:recipes []
               :name ((comp format-category scrape-category) tr)}]))
         [[] nil] (extract-menu-trs menu-list-table))]
    {:categories (conj (rest (first scraped))
                       (second scraped))
     :meal-name (scrape-meal-name menu-list-table)}))
