(ns sdf.dsl)

(defn compose
  "f*gの合成関数を返す"
  [f g]
  (fn [args]
      (f (g args))))

(defn iter
  "関数fをn回適用する"
  [n f]
  (if (= n 0)
      identity
      (compose f (iter (- n 1) f))))


(defn parallel-combine
  "h(f(arg), g(arg))を返す"
  [h f g]
  (fn [& args]
      (h (apply f args) (apply g args))))


(defn get-arity
  "関数のarityを取得(無名関数もOK)"
  [proc]
  {:pre [(instance? clojure.lang.AFunction proc)]}
  (if-let [arglist (:arglists (meta proc))]
    (count (first arglist))
    (-> proc
        class
        .getDeclaredMethods
        first
        .getParameterTypes
        alength)))

(defn restrict-arity
  "arityをmetadateに書き込む。get-arityが動けばいいので全部'x"
  [proc nargs]
  (with-meta proc {:arglists (list (repeat nargs 'x)) }))

(defn spread-combine
  "argsをfとgに分配する。"
  [h f g]
  (let [f-arity (get-arity f)
        g-arity (get-arity g)
        total-arity (+ f-arity g-arity)
        the-combination (fn [& args]
                            (assert (= (count args) total-arity) ; Paranoid Programming Style...
                                    (str "spread-combine: The invalid number of args.:" (count args)))
                            (h (apply f (take f-arity args))
                               (apply g (take-last g-arity args))))]
    (restrict-arity the-combination total-arity)))

