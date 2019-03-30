(ns den1k.shortcuts
  (:require [clojure.string :as str]))

(def clear-set #{"control" "meta" "option" "cmd" " " ; space
                 "arrowleft" "arrowright" "arrowup" "arrowdown"})

(defn event->key [e] (-> e .-key str/lower-case))

(def default-remap {"control" "ctrl"
                    "alt"     "option"
                    "meta"    "cmd"})

(defn parse-combos [combos+handlers]
      (into {}
            (map (fn [[combo handler]] [(set (str/split combo "+")) handler]))
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
      ([combos+handlers re-map]
       (let [current-combo (atom #{})
             shortcuts     (parse-combos combos+handlers)
             lookup-key    (fn [e] (as-> (event->key e) k
                                         (or (re-map k) k)))
             on-key-down   (fn [e]
                               (let [k     (lookup-key e)
                                     combo (swap! current-combo conj k)]
                                    ;(js/console.log combo)
                                    (when-let [handler (get shortcuts combo)]
                                              (when (false? (handler e))
                                                    (.preventDefault e)
                                                    (.stopPropagation e)))))
             on-key-up     (fn [e]
                               (let [k (lookup-key e)]
                                    (swap! current-combo disj k)
                                    ; key ups don't register when modifier keys are pressed simulaneously
                                    ; since these _become_ different keys such as π (opt+p) so we clear all
                                    (when (clear-set k)
                                          (swap! current-combo empty))))]
            {:on-key-down on-key-down
             :on-key-up   on-key-up})))

(comment

 (shortcuts {"enter" #(js/console.log "pressed enter, event: " %)
             "cmd+z" #(js/console.log "undo, event: " %)})

 ;; => {:on-key-down #object[Function]
 ;;     :on-key-up   #object[Function]}

 )
