(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(def plan
  [[[:fox :goose :corn :you] [:boat] []]
   [[:fox :corn] [:boat :you :goose] []]
   [[:fox :corn] [:boat] [:you :goose]]
   [[:fox :corn] [:boat :you] [:goose]]
   [[:fox :corn :you] [:boat] [:goose]]
   [[:corn] [:boat :you :fox] [:goose]]
   [[:corn] [:boat] [:you :fox :goose]]
   [[:corn] [:boat :you :goose] [:fox]]
   [[:corn :you :goose] [:boat] [:fox]]
   [[:goose] [:boat :you :corn] [:fox]]
   [[:goose] [:boat] [:you :fox :corn]]
   [[:goose] [:boat :you] [:fox :corn]]
   [[:goose :you] [:boat] [:fox :corn]]
   [[] [:boat :you :goose] [:fox :corn]]
   [[] [:boat] [:you :goose :fox :corn]]])

(defn river-crossing-plan []
  plan)
