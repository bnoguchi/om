(ns om.devcards.bugs
  (:require-macros [devcards.core :refer [defcard deftest dom-node]])
  (:require [cljs.test :refer-macros [is async]]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(def init-data
  {:dashboard/posts
   [{:id 0 :favorites 0}]})

(defui Post
  static om/Ident
  (ident [this {:keys [id]}]
    [:post/by-id id])

  static om/IQuery
  (query [this]
    [:id :favorites])

  Object
  (render [this]
    (let [{:keys [id favorites] :as props} (om/props this)]
      (dom/div nil
        (dom/p nil "Favorites: " favorites)
        (dom/button
          #js {:onClick
               (fn [e]
                 (om/transact! this
                   `[(post/favorite {:id ~id})]))}
          "Favorite!")))))

(def post (om/factory Post))

(defui Dashboard
  static om/IQuery
  (query [this]
    `[({:dashboard/posts ~(om/get-query Post)} {:foo "bar"})])

  Object
  (render [this]
    (let [{:keys [dashboard/posts]} (om/props this)]
      (apply dom/ul nil
        (map post posts)))))

(defmulti read om/dispatch)

(defmethod read :dashboard/posts
  [{:keys [state query]} k _]
  (let [st @state]
    {:value (om/db->tree query (get st k) st)}))

(defmulti mutate om/dispatch)

(defmethod mutate 'post/favorite
  [{:keys [state]} k {:keys [id]}]
  {:action
   (fn []
     (swap! state update-in [:post/by-id id :favorites] inc))})

(def reconciler
  (om/reconciler
    {:state  init-data
     :parser (om/parser {:read read :mutate mutate})}))

(defcard test-om-466
  "Test that Parameterized joins work"
  (dom-node
    (fn [_ node]
      (om/add-root! reconciler Dashboard node))))

;; ==================
;; OM-552

(defui Child
  Object
  (componentWillUpdate [this next-props _]
    (.log js/console "will upd" (clj->js (om/props this)) (clj->js next-props)))
  (render [this]
    (let [{:keys [x y]} (om/props this)]
      (dom/p nil (str "x: " x "; y: " y)))))

(def child (om/factory Child))

(defui Root
  Object
  (initLocalState [_]
    {:x 0 :y 0 :pressed? false})
  (mouse-down [this e]
    (om/update-state! this assoc :pressed? true)
    (.mouse-move this e))
  (mouse-move [this e]
    (when-let [pressed? (-> this om/get-state :pressed?)]
      (om/update-state! this assoc :x (.-pageX e) :y (.-pageY e))))
  (render [this]
    (let [{:keys [x y]} (om/get-state this)]
      (dom/div #js {:style #js {:height 200
                                :width 600
                                :backgroundColor "red"}
                    :onMouseDown #(.mouse-down this %)
                    :onMouseMove #(.mouse-move this %)
                    :onMouseUp #(om/update-state! this assoc :pressed? false :x 0 :y 0)}
        (child {:x x :y y})))))

(def rec (om/reconciler {:state {}
                         :parser (om/parser {:read read})}))

(defcard test-om-552
  "Test that componentWillUpdate receives updated next-props"
  (dom-node
    (fn [_ node]
      (om/add-root! rec Root node))))

;; ==================
;; OM-543

(def om-543-data
  {:tree {:node/type :tree/foo
          :id 0
          :foo/value "1"
          :children [{:node/type :tree/bar
                      :bar/value "1.1"
                      :id 1
                      :children [{:node/type :tree/bar
                                  :id 2
                                  :bar/value "1.1.1"
                                  :children []}]}
                     {:node/type :tree/foo
                      :id 3
                      :foo/value "1.2"
                      :children []}]}})

(declare item-node)

(defui UnionBarNode
  static om/IQuery
  (query [this]
    '[:id :node/type :bar/value {:children ...}])
  Object
  (render [this]
    (let [{:keys [bar/value children]} (om/props this)]
      (dom/li nil
        (dom/p nil (str "Bar value: " value))
        (dom/ul nil
          (map item-node children))))))

(def bar-node (om/factory UnionBarNode))

(defui UnionFooNode
  static om/IQuery
  (query [this]
    '[:id :node/type :foo/value {:children ...}])
  Object
  (render [this]
    (let [{:keys [foo/value children]} (om/props this)]
      (dom/li nil
        (dom/p nil (str "Foo value: " value))
        (dom/ul nil
          (map item-node children))))))

(def foo-node (om/factory UnionFooNode))

(defui ItemNode
  static om/Ident
  (ident [this {:keys [node/type id]}]
    [type id])
  static om/IQuery
  (query [this]
    {:tree/foo (om/get-query UnionFooNode)
     :tree/bar (om/get-query UnionBarNode)})
  Object
  (render [this]
    (let [{:keys [node/type] :as props} (om/props this)]
      (({:tree/foo foo-node
         :tree/bar bar-node} type)
         props))))

(def item-node (om/factory ItemNode))

(defui UnionTree
  static om/IQuery
  (query [this]
    `[{:tree ~(om/get-query ItemNode)}])
  Object
  (render [this]
    (let [{:keys [tree]} (om/props this)]
      (dom/ul nil
        (item-node tree)))))

(defmulti om-543-read om/dispatch)

(defmethod om-543-read :default
  [{:keys [data] :as env} k _]
  {:value (get data k)})

(defmethod om-543-read :children
  [{:keys [parser data union-query state] :as env} k _]
  (let [st @state
        f #(parser (assoc env :data (get-in st %)) ((first %) union-query))]
    {:value (into [] (map f) (:children data))}))

(defmethod om-543-read :tree
  [{:keys [state parser query ast] :as env} k _]
  (let [st @state
        [type id :as entry] (get st k)
        data (get-in st entry)
        new-env (assoc env :data data :union-query query)]
    {:value (parser new-env (type query))}))

(def om-543-reconciler
  (om/reconciler {:state om-543-data
                  :parser (om/parser {:read om-543-read})}))

(defcard om-543
  "Test that recursive queries in unions work"
  (dom-node
    (fn [_ node]
      (om/add-root! om-543-reconciler UnionTree node))))

;; ==================
;; OM-595

(defmulti om-595-read om/dispatch)

(defmethod om-595-read :item
  [{:keys [state query parser] :as env} key _]
  (let [st @state]
    {:value (om/db->tree query (get st key) st)}))

(def om-595-state
  {:item [:item/by-id 1]
   :item/by-id {1 {:id 1
                   :title "first"
                   :next  [:item/by-id 2]}
                2 {:id 2
                   :title "second"}}})

(def om-595-reconciler
  (om/reconciler {:state  om-595-state
                  :parser (om/parser {:read om-595-read})}))

(defui OM-595-App
  static om/IQuery
  (query [this]
    '[{:item [:id :title {:next ...}]}])
  Object
    (render [this]
      (dom/div nil (str (om/props this)))))

(defcard om-595
  "Test that `index-root` doesn't blow the stack"
  (dom-node
    (fn [_ node]
      (om/add-root! om-595-reconciler OM-595-App node))))

;; ==================
;; OM-601

(defn om-601-read
  [{:keys [state] :as env} k _]
  (let [st @state]
    {:value (get st k)}))

(defn om-601-mutate
  [{:keys [state] :as env} _ _]
  {:action #(swap! state update-in [:foo :foo/val] inc)})

(def om-601-state
  {:foo {:foo/val 0}
   :bar {:bar/val 0}})

(def om-601-reconciler
  (om/reconciler {:state  om-601-state
                  :parser (om/parser {:read om-601-read :mutate om-601-mutate})}))

(defui OM-601-Foo
  static om/IQuery
  (query [this]
    [:foo/val])
  Object
  (render [this]
    (println "Render Foo")
    (dom/div nil
      (dom/p nil (str "Foo: " (:foo/val (om/props this)))))))

(def om-601-foo (om/factory OM-601-Foo))

(defui OM-601-Bar
  static om/IQuery
  (query [this]
    [:bar/val])
  Object
  (render [this]
    (println "Render Bar")
    (dom/div nil
      (dom/p nil (str "Bar: " (:bar/val (om/props this)))))))

(def om-601-bar (om/factory OM-601-Bar))

(defui OM-601-App
  static om/IQuery
  (query [this]
    [{:foo (om/get-query OM-601-Foo)
      :bar (om/get-query OM-601-Bar)}])
  Object
  (render [this]
    (let [{:keys [foo bar]} (om/props this)]
      (dom/div nil
        (om-601-foo foo)
        (om-601-bar bar)
        (dom/button #js {:onClick #(om/transact! this '[(increment/foo!)])}
          "Inc foo")))))

(defcard om-601-card
  "Test that `shouldComponentUpdate` doesn't render unchanged children"
  (dom-node
    (fn [_ node]
      (om/add-root! om-601-reconciler OM-601-App node))))

;; ==================
;; OM-598

(defmulti om-598-read om/dispatch)

(defmethod om-598-read :default
  [{:keys [state]} k _]
  (let [st @state]
    {:value (get st k)}))

(defui OM-598-Child
  static om/IQuery
  (query [this]
   [:bar])
  Object
  (initLocalState [_]
    {:a 1})
  (render [this]
    (let [{:keys [a]} (om/get-state this)]
      (dom/div nil
        (dom/button #js {:onClick #(om/update-state! this update :a inc)}
          (str "a: " a))))))

(def om-598-child (om/factory OM-598-Child))

(defui OM-598-App
  static om/IQuery
  (query [this]
   [:foo])
  Object
  (componentDidMount [this]
    (om/set-query! this {:query [{:child (om/get-query OM-598-Child)}]}))
  (render [this]
    (let [props (om/props this)]
      (dom/div nil
        (when-let [child-props (:child props)]
          (om-598-child child-props))))))

(def om-598-reconciler
  (om/reconciler {:state {:foo 3 :child {:bar 2}}
                  :parser (om/parser {:read om-598-read})}))

(defcard om-598
  "Test that `reindex!` works on query transitions"
  (dom-node
    (fn [_ node]
      (om/add-root! om-598-reconciler OM-598-App node))))

;; ==================
;; OM-628

(defn om-628-read
  [{:keys [state query] :as env} k _]
  (println "query" query)
  (let [st @state]
    {:value (om/db->tree query (get st k) st)}))

(defui om-628-Child
  static om/Ident
  (ident [this {:keys [id]}]
    [:item/by-id id])
  static om/IQueryParams
  (params [this]
    {:count 3})
  static om/IQuery
  (query [this]
    '[:id :title (:items {:count ?count})])
  Object
  (render [this]
    (let [{:keys [title items] :as props} (om/props this)]
      (dom/div nil
        (dom/h2 nil title)
        (dom/ul nil
          (map-indexed #(dom/li #js {:key %1} (name %2)) items))
        (dom/button #js {:onClick #(om/set-query!
                                     this {:query [:id :title]} (om/path this))}
          "Set this query to: [:id :title]")
        (dom/button #js {:onClick #(println "my query" (om/get-query this))}
          "Get local component query")))))

(def om-628-child (om/factory om-628-Child {:keyfn :id}))

(defui om-628-Parent
  static om/IQuery
  (query [this]
    [{:child1 (om/get-query om-628-Child)}
     {:child2 (om/get-query om-628-Child)}])
  Object
  (render [this]
    (let [{:keys [child1 child2]} (om/props this)]
      (dom/div nil
        (dom/p nil (str "Root query: " (om/get-query this)))
        (om-628-child child1)
        (om-628-child child2)))))

(defonce om-628-state
  {:child1 {:id 1 :title "Child 1" :items ["item 1" "item 2"]}
   :child2 {:id 2 :title "Child 2" :items ["item 1" "item 2"]}})

(def om-628-reconciler
  (om/reconciler {:state om-628-state
                  :parser (om/parser {:read om-628-read})}))

(defcard om-628-card
  (dom-node
    (fn [_ node]
      (om/add-root! om-628-reconciler om-628-Parent node))))


(def child-join-via-params-state
  {:route/name :route-list/books
   :route/params {}
   :route/data {:list/books [[:books/by-id "x"]]}
   :books/by-id {"x" {:id "x" :book/author {:name "X"}}
                 "y" {:id "y" :book/author {:name "Y"}}}
   })

(defmulti child-join-via-params-read om/dispatch)
(defmulti child-join-via-params-mutate om/dispatch)

(def child-join-via-params-parser
  (om/parser
    {:read child-join-via-params-read
     :mutate child-join-via-params-mutate}))


(def child-join-via-params-reconciler
  (om/reconciler
    {:state child-join-via-params-state
     :parser child-join-via-params-parser
     :send (fn [queries cb]
             (let [{:keys [remote]} queries]
               ; REMOTE IS NEVER FIRED FOR THE SECOND MUTATION
               (println "remote called for child-join-via-params")
               (cljs.pprint/pprint remote)
               ; simulate remote data coming back for first
               ; request
               (if (some-> remote first :route/data first :list/books)
                 ; IF YOU COMMENT OUT THIS CALLBACK, THINGS WORK.
                 ; IF YOU LEAVE THIS CALLBACK IN, IT DOES NOT WORK.
                 (cb
                  {:route/data
                   {:list/books
                    [{:id "w" :book/author {:name "W"}}
                     {:id "x" :book/author {:name "X"}}
                     {:id "y" :book/author {:name "Y"}}
                     {:id "z" :book/author {:name "Z"}}
                     ]
                    }
                   }
                   remote)
                 nil
               )
               (if (some-> remote first :route/data ffirst :lookup/book)
                 ; IF YOU COMMENT OUT THIS CALLBACK, THINGS WORK.
                 ; IF YOU LEAVE THIS CALLBACK IN, IT DOES NOT WORK.
                 (cb
                   {:route/data
                     {:lookup/book
                       {:id "y"
                        :title "EXPECTED CORRECT BEHAVIOR"
                        :book/author {:name "Y"}}
                      }
                    }
                    remote)
                 nil
               )
               ))}))


(defmethod child-join-via-params-read :default
  [{:keys [state book]} k _]
  (if book
    {:value (get book k)}
    {:value (get @state k)}))

(defmethod child-join-via-params-read :route/data
  [{:keys [state query target ast] :as env} k _]
  (let [x (child-join-via-params-parser env query target)]
    {:value x
     :remote (assoc ast
               :query x
               :type :join)}))

(defmethod child-join-via-params-read :list/books
  [{:keys [state query ast] :as env} k _]
  (let [st @state
        book-idents (get-in st [:route/data k])]
    {:value (mapv
              #(child-join-via-params-parser
                 (assoc env :book (get-in st %))
                 query)
             book-idents)
     :remote (assoc ast :query-root true)}))

(defmethod child-join-via-params-read :lookup/book
  [{:keys [state query ast] :as env} k _]
  (let [st @state
        route-params (:route/params st)
        book-id (:id route-params)]
    {:value (get-in st [:books/by-id book-id])
     :remote (-> ast
                 (assoc
                   :query-root true
                   :params route-params))}))


(defui ChildJoinViaParamsBook
  static om/Ident
  (ident [this {:keys [id]}]
    [:books/by-id id])
  static om/IQuery
  (query [this]
    [:id {:book/author [:name]}])
  Object
  (render [this]
    (let [{:keys [id book/author]} (om/props this)]
      (dom/li #js {:key (author :name)}
              (author :name)))))
(def child-join-via-params-book (om/factory ChildJoinViaParamsBook))

(defui ChildJoinViaParamsList
  static om/IQuery
  (query [this]
    [{:list/books (om/get-query ChildJoinViaParamsBook)}])
  Object
  (render [this]
    (let [{:keys [list/books]} (om/props this)]
      (dom/ul nil (map child-join-via-params-book
                       books)))))

(def child-join-via-params-list (om/factory ChildJoinViaParamsList))

(defui ChildJoinViaParamsBookWithTitle
  static om/Ident
  (ident [this {:keys [id]}]
    [:books/by-id id])
  static om/IQuery
  (query [this]
    [:id :title {:book/author [:name]}])
  Object
  (render [this]
    (let [{:keys [id title book/author]} (om/props this)]
      (dom/div nil
               (dom/div nil (str "Author " (:name author)))
               (dom/div nil (str "Title" (if title title "UNEXPECTED BEHAVIOR")))))))

(def child-join-via-params-book-with-title (om/factory ChildJoinViaParamsBookWithTitle))

(defui ChildJoinViaParamsLookup
  static om/IQuery
  (query [this]
    [{:lookup/book (om/get-query ChildJoinViaParamsBookWithTitle)}])
  Object
  (render [this]
    (let [{:keys [lookup/book]} (om/props this)]
      (child-join-via-params-book-with-title book))))
(def child-join-via-params-lookup (om/factory ChildJoinViaParamsLookup))

(defmethod child-join-via-params-mutate 'change/route
  [{:keys [state]} _ {:keys [route/name route/params]}]
  {:value {:keys [:route :data]}
   :action
     (fn []
       (let [component (case name
                          :route-list/books ChildJoinViaParamsList
                          :route-lookup/book ChildJoinViaParamsLookup
                          nil)
             query (om/get-query component)
             root (om/app-root child-join-via-params-reconciler)]
         (swap! state assoc :route/name name :route/params params)
         (om/set-query! root {:params {:query query}})))})


(defui ChildJoinViaParamsRoot
  static om/IQueryParams
  (params [this]
    {:query []})
  static om/IQuery
  (query [this]
   '[:route/name :route/params {:route/data ?query}])
  Object
  (render [this]
    (let [{:keys [route/name route/data]} (om/props this)
          component (case name
                      :route-list/books child-join-via-params-list
                      :route-lookup/book child-join-via-params-lookup
                      nil)]
      (dom/div nil
        (dom/button
          #js {:onClick
               (fn []
                 (om/transact!
                   (om/app-root child-join-via-params-reconciler)
                   '[(change/route {:route/name :route-lookup/book :route/params {:id "y"}})]))}
          "Change Route")
        (component data)))))

(defcard child-join-via-params
  "URL routing scenario bug caused by the sequence

  1. set-query on the app-root
  2. Receive and render data from remote
  3. set-query again on the app-root that causes a different child component to render
  4. BUG The root query resulting from 3 is never sent up to the remote
  
  Click the Button 'Change Route'. You should expect to see a book title 
  \"EXPECTED CORRECT BEHAVIOR\" but instead a title is never fetched from
  remote so we fall back to the default book title \"UNEXPECTED BEHAVIOR\"
  which indicates the presence of a bug."
  (dom-node
    (fn [_ node]
      (om/add-root! child-join-via-params-reconciler ChildJoinViaParamsRoot node)
      (om/transact!
        (om/app-root child-join-via-params-reconciler)
        '[(change/route {:route/name :route-list/books})]))))


(comment

  (require '[cljs.pprint :as pprint])

  (pprint/pprint @(om/get-indexer reconciler))

  )
