(ns den1k.shortcuts
  "CLJC solely for SSR purposes"
  (:require [clojure.string :as str]))

(def meta-keys
  #{"control" "meta" "option" "cmd" "shift"})

(defn event->key [e] (-> e .-key str/lower-case))

(def default-remap {"control" "ctrl"
                    "alt"     "option"
                    "meta"    "cmd"})

(defn parse-combos [combos+handlers]
  (into {}
        (map (fn [[combo handler]] [(set (str/split combo (re-pattern "\\+"))) handler]))
        combos+handlers))

(defn shortcuts
  "Component-local shortcut handler.
  Takes a map of combos and handlers and returns a map of keyboard handlers ready to
  be merged into a component. Handlers are passed the keyboard event.
  Example:
  [:input (shortcuts {\"enter\" handle-enter!})]
  ;; OR
  [:input
    (merge (shortcuts {\"enter\" handle-enter!})
           {:placeholder \"Type something!\"
            :value       @input-value})]
  As a convenience, handler can return `false` to prevent event and stop propagation."
  ([combos+handlers] (shortcuts combos+handlers default-remap))
  ([{:as combos+handlers :keys [debug?]} re-map]
   #?(:cljs
      (let [current-combo (atom #{})
            shortcuts     (parse-combos (dissoc combos+handlers :debug?))
            lookup-key    (fn [e] (as-> (event->key e) k
                                        (or (re-map k) k)))
            on-key-down   (fn [e]
                            (let [k      (lookup-key e)
                                  combo  (swap! current-combo conj k)
                                  meta-k (meta-keys k)]
                              (when-not meta-k
                                ; meta keys block the keyup event, so we throw out
                                ; non-meta keys immediately.
                                ; This means there can only be at most one non-meta-key
                                ; as part of the combo
                                (swap! current-combo disj k))
                              (let [handler (get shortcuts combo)]
                                (when debug?
                                  (js/console.log ::debug :k k :combo combo :handler handler))
                                (when handler
                                  (when (false? (handler e))
                                    (.preventDefault e)
                                    (.stopPropagation e))))))
            on-key-up     (fn [e]
                            (let [k (lookup-key e)]
                              (swap! current-combo disj k)))]
        {:on-key-down on-key-down
         :on-key-up   on-key-up}))))

(defonce ^:private global-shortcuts-map (atom {}))

(defn global-shortcuts
  "Like `shortcuts` for js/document."
  [& args]
  #?(:cljs
     (do
       (when-let [{:keys [on-key-down on-key-up]} (not-empty @global-shortcuts-map)]
         (doto js/document
           (.removeEventListener "keydown" on-key-down)
           (.removeEventListener "keyup" on-key-up)))

       (let [{:as shm :keys [on-key-down on-key-up]} (apply shortcuts args)]
         (doto js/document
           (.addEventListener "keydown" on-key-down)
           (.addEventListener "keyup" on-key-up))

         (reset! global-shortcuts-map shm)))))

(comment


  ; clj
  (shortcuts {"enter" :enter})
  ; => nil

  ; cljs
  (shortcuts {"enter" #(js/console.log "pressed enter, event: " %)
              "cmd+z" #(js/console.log "undo, event: " %)})

  ;; => {:on-key-down #object[Function]
  ;;     :on-key-up   #object[Function]}


  (global-shortcuts {"cmd+z"       #(do (js/console.log "UNDO")
                                        false
                                        )
                     "cmd+shift+z" #(do (js/console.log "REDO")
                                        false)})

  )
