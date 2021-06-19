(ns sdf.dsl)

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


(defn compose
  "f*gの合成関数を返す"
  [f g]
  (let [f-arity (get-arity f)
        g-arity (get-arity g)
        check-arity (assert (= f-arity g-arity)
              (str "Arity mismatch: f: " f-arity ", g: " g-arity))
        the-combination
          (fn [& args]
            (assert (= f-arity (count args))
                    (str "Invalid number of args: " (count args) ", " f-arity " expected"))
            (f (apply g args)))]
    (restrict-arity the-combination f-arity)))

(defn iter
  "関数fをn回適用する"
  [n f]
  (if (= n 0)
      identity
      (compose f (iter (- n 1) f))))


(defn parallel-combine
  "h(f(arg), g(arg))を返す"
  [h f g]
  (let [h-arity (get-arity h)
        f-arity (get-arity f)
        g-arity (get-arity g)
        check-arity (assert (and
                              (= h-arity 2)
                              (= f-arity g-arity))
                            (str "Arity mismatch: h: " h-arity ", f: " f-arity ", g: " g-arity))
        the-combination
          (fn [& args]
            (assert (= (count args) f-arity)
                    (str "Invalid number of args: " (count args) ", " f-arity " expected"))
            (h (apply f args) (apply g args)))]
    (restrict-arity the-combination f-arity)))


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

