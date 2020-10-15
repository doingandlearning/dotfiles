;;; clojure.el -*- lexical-binding: t; -*-


;;
;; Clojure
;;
(map! :map clojure-mode-map
      "<f5>"    #'cider-jack-in
      "M-<f5>"  #'cider-jack-in-clj&cljs)

(require 'ob-clojure)
    (setq org-babel-clojure-backend 'cider)
    (require 'cider)
