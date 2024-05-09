

(with-meta [:h2 "REPL in the first place"] {:shashurup.quf/hint :html})

(ns intro (:require [shashurup.quf.ui :refer :all]))

(+ 1 2 3)

(read-string (slurp "deps.edn"))

(html [:p "By now, there is nothing special here. Of course you may play with it and figure that parts of the datastructure can be collapsed. However that is not what I dreamt about. One of the ideas I wanted to play with was having structured data in shell piplines. Here is where the ideas cross - lisp is a natural choice to represent such kind of data. So I started with filesystem module."])

(html [:h2 "A shell, a bit improved"])

(require '[shashurup.quf.fs :as f])

(f/c "~/devel/quasi-una-fantasia")

(f/l :m "resources/public")

(html [:p "Hey, what has just happened? This doesn't look like a clojure data structure!"])

(with-meta (f/l :m "resources/public") {})

(html [:p "Here they are - our clojure data structures."])

(meta (f/l :m "resources/public"))

(with-meta [:p "The magic is caused by the special metadata attached to our clojure data representing these files. " 
               "Metadata activates custom client side code which may render data returned by a function in a special way. "
               "In this case we have specialized renderer for files. (See " [:a {:href "fs.cljs"} "fs.cljs"] " module for example). "
               "There are also some generic renderers such as " [:span.quf-keyword ":table"] " or " [:span.quf-keyword ":html"] ", check it out."]
           {:shashurup.quf/hint :html})

(table [{:name "Giuseppe" :age 23}
        {:name "Василий" :age 42}
        {:name "Ὅμηρος" :age :undefined}])

(html [:p "There are convenience functions such as " [:span.quf-symbol "table"] " which add appropriate metadata"])

(f/l :m "*.edn")

(f/f "*.clj")

(f/l :m)

$s

(html [:p [:span.quf-client-var "$s"] " is so called client side variable. "
       "It is evaluated before an expression is sent to backend for evaluation. "
       [:span.quf-client-var "$s"] " is for the nearest cell with checkboxes (they appear when you press Ctrl+m), "
       [:span.quf-client-var "$s-all"] " is for all checkboxes on the page etc."
       "These variables are controlled by client side code and cannot be assigned."])

(f/r "package.json")

(html [:p "This detects file type and parses it accordingly. XML and CSV are also supported."])

(f/c "~/wallpapers")

(f/l :c "autumn*.jpg")

(html [:p [:span.quf-keyword ":c"] " makes output to appear as a list of thumbnails. (Ctrl+m also works here)"])

(f/v "20230220_154530.jpg")

(html [:h2 "Conventional shell"])


(require '[shashurup.quf.sh :refer [! !>]])

(f/c "~/devel/quasi-una-fantasia")

(!> "cat dev.cljs.edn")

(require '[clojure.string :as s])

(update-vals (->> (!> "pacman -Qg")
                  (map #(s/split % #" "))
                  (group-by first))
             #(map second %))


(! "ls --color /etc")

(html [:p "Escape sequences are handled via xterm.js. And, yes, you can run Vim here."])

(html [:h1 "Databases"])

(html [:p "It was a bit hard to resist temptation to plug jdbc infrastructure"])

(require '[shashurup.quf.db :as db])

(db/c "postgresql://postgres:123@localhost/postgres")

(db/q "select id, name, country_id from capitals limit 10")

(db/q "select id, name from countries where id < ?" 8)


(html [:h3 "Geo"])

(require '[shashurup.quf.geo :as g])

(db/q "select id, name from countries where name like 'W%'")

(g/v (db/q "select geom, name, id from countries where name like 'Po%'"))


(html [:h2 "Charts"])

(require '[shashurup.quf.chart :as chart])

(chart/histogram (repeatedly 100 #(rand-int 100)))

(html [:h2 "Dessert"])

(require '[shashurup.quf.theme :as theme])

(theme/ls "gruv")

(html [:p "There is a collection of base16 themes, use " [:span.quf-symbol "ls"]
       " to browse, click to apply, enjoy!"])

(store-cells "doc/doc.clj" $cells)