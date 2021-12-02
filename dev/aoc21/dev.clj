(ns aoc21.dev
  (:require [portal.api :as p]))


  (comment
  ;; setup to tap output in a Portal window
    
    (def portal (p/open {:launcher :vs-code}))
    (add-tap #'p/submit)

    )