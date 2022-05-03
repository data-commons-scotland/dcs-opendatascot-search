(ns dcs.opendatascotsearch.app
  (:require  [reagent.core :as r]
             [reagent.dom :as rdom]
             [clojure.string :as str]
            [dcs.opendatascotsearch.data :refer [index-cards]]))

;; Try to match the search words against the contents of one index card.
(defn match-against-index-card
  [index-card search-words]
  (let [search-words->matches
        (->> (for [search-word search-words]
               [search-word (->> (:words index-card)
                                 (filter #(str/includes? % search-word))
                                 distinct)])
             (into {}))

        ;; how many search-words are exact equals of/contained within a word in the index card?
        count-of-search-words-with-subs-matches
        (->> (vals search-words->matches)
             (remove empty?)
             count)

        ;; how many search-words are exact equals of a word in the index card?
        count-of-search-words-with-exact-matches
        (->> search-words->matches
             (map (fn [[search-word matches]] (filter #(= % search-word) matches)))
             (remove empty?)
             count)]

    (-> index-card
        (dissoc :words)
        (assoc :metric-1 count-of-search-words-with-subs-matches
               :metric-2 count-of-search-words-with-exact-matches
               :search-words->matches search-words->matches))))

;; Try to match the search words against the contents of all of the index cards.
(defn match-against-index-cards
  [index-cards search-words]
  (for [index-card index-cards]
    (match-against-index-card index-card search-words)))

(defonce search-words (r/atom ""))

(defn search-results 
  []
  (let [search-results (->> (str/split @search-words #"\s")
                          (filter #(>= (count %) 3))
                          (map str/lower-case)
                          (match-against-index-cards index-cards)
                          (filter #(> (:metric-1 %) 0))
                          (sort-by (juxt :metric-1 :metric-2 :modified :title))
                          reverse)
      max-search-results-shown 150]
  [:div
   (let [n (count search-results)]
     [:span.text-sm n " (partial) match" (when (not= n 1) "es") "."
      (if (> n max-search-results-shown) (str " Displaying the first " max-search-results-shown ".") "")])
   [:ol
    (for [index-card (take max-search-results-shown search-results)]
      ^{:key index-card} [:li
       [:a {:href (:url index-card)} (:title index-card)] ", " (:modified index-card) ", " (:org index-card) [:br]
       (let [s (str/trim (:notes index-card))]
         (when (not (str/blank? s))
           [:<> [:span.text-gray-600 s] [:br]]))
       [:span.text-gray-400.text-sm "Matching: "]
       (-> (for [[search-word matches] (:search-words->matches index-card)]
             (when-let [match (first matches)]
               (let [ix1 (str/index-of match search-word)
                     ix2 (+ ix1 (count search-word))]
                 ^{:key search-word} [:span.text-gray-500.text-sm
                  (subs match 0 ix1)
                  [:span.font-extrabold.text-indigo-700 search-word]
                  (subs match ix2)])))
           (interleave (repeat " ")))])]]))

(defn page
  []
  [:div.container.mx-auto ;; https://umeshmk.github.io/Tailwindcss-cheatsheet/ is a handy cheatsheet for TailwindCSS
   [:div.prose.prose-a:text-blue-600.prose-a:no-underline.hover:prose-a:underline.font-serif.antialiased.leading-snug
    [:h1 "A demo of a simple alternative search over Open Data Scotland's dataset indexes"]
    [:p "(" [:a {:href "https://campuspress.stir.ac.uk/datacommonsscotland/2022/05/03/a-simple-alternative-search-over-open-data-scotlands-dataset-indexes/"} "Background and rationale"] ")"]
    [:p.text-xl.text-indigo-700.font-bold "Type words (of three or more letters)"]
    [:input {:type          :text
             :placeholder   "Search... (type words of three or more letters)"
             :initial-value @search-words
             :class         "px-3 py-3 placeholder-blueGray-300 text-blueGray-500 relative bg-white bg-white rounded text-sm border border-blueGray-300 outline-none focus:outline-none focus:ring w-full"
             :on-input      #(reset! search-words (.. % -target -value))}]
    [search-results]]])

;; called by init and after code reloading finishes
(defn ^:dev/after-load start
  []
  (rdom/render [page] (.getElementById js/document "app")))

;; init is called ONCE when the page loads
;; this is called in the index.html and must be exported
;; so it is available even in :advanced release builds
(defn ^:export init
  []
  (start))

;; this is called before any code is reloaded
(defn ^:dev/before-load stop
  []
  (js/console.log "stop"))
