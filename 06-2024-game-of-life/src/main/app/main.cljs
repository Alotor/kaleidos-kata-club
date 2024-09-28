(ns app.main
  (:require
   [app.ui :as ui]))

;; start is called by init and after code reloading finishes
(defn ^:dev/after-load start []
  (ui/render-grid)
  )

(defn init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (ui/init-handlers)
  (start))

;; this is called before any code is reloaded
(defn ^:dev/before-load stop []
  (println "stop"))

(defn greeting
  [name]
  (str "Hello " name "!"))

