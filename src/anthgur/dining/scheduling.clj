(ns anthgur.dining.scheduling
  (:require [anthgur.dining.scraper :refer
             [scrape-page scrape-date-urls get-page]]))

(def base-url "http://foodpro.unh.edu/")

(def hall-menu-urls
  (let [fmt (fn [num name]
              (str base-url "shortmenu.asp?sName=University+Of+New+Hampshire"
                   "+Hospitality+Services&locationNum="
                   num "&locationName=" name "&naFlag=1"))]
    {:hoco (fmt 80 "Holloway+Dining+Hall")
     :philly (fmt 30 "Philbrook+Dining+Hall")
     :stillings (fmt 10 "Stillings+Dining+Hall")}))

(schema.core/validate anthgur.unh-dining.scraping/Day (scrape-page (get-page (:philly hall-menu-urls))))
