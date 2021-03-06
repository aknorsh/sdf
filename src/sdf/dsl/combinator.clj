(ns sdf.dsl.combinator)

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

;; clojureは多値を返す手段が存在しなさそうなので、
;; 返り値をリストに格納するという制約を入れてcompose等を書き直すことで再現してみる。
;; ただしこの方法には欠点があり、ある一般的な関数f(arg) => [2 3]について、
;; - 2と3の多値を返している (今回の制約を満たしてくれている)
;; - リストを返す関数で、中身が2 3 (制約を満たしてくれているわけではない一般の関数)
;; の区別がつかないなどの問題を抱えている。
(defn spread-apply
  "fとgに引数を分配し、返り値をリストで返す。
   fとgは多値を返す関数で、返り値は常にリストとして返す。"
  [f g]
  (let [f-arity (get-arity f)
        g-arity (get-arity g)
        total-arity (+ f-arity g-arity)
        the-combination (fn [& args]
                            (assert (= (count args) total-arity)
                                    (str "spread-apply: The invalid number of args.:" (count args)))
                            (concat
                              (apply f (take f-arity args))
                              (apply g (take-last g-arity args))))]
    (restrict-arity the-combination total-arity)))

(defn multi-compose
  "fとgの合成関数を返す。
   ただし、fとgはともに多値を返しうる関数。
   引数は必ずリストに格納して返す。"
  [f g]
  (let [f-arity (get-arity f)
        g-arity (get-arity g)
        the-composition (fn [& args]
                            (let [check-g-arity (assert (= g-arity (count args))
                                                        (str "multi-compose: The invalid number of args.:" (count args)))
                                  g-res (apply g args)
                                  check-f-arity (assert (= f-arity (count g-res))
                                                        (str "multi-compose: The invalid number of results.:" (count g-res)))]
                              (apply f g-res)))]
    (restrict-arity the-composition g-arity)))

(defn composed-spread-combine
  "spread-combineをcomposeでつくる。"
  [h f g]
  (multi-compose h (spread-apply f g)))

(defn parallel-apply
  "fとgに引数を渡し、返り値をリストで返す。
   fとgは多値を返す関数で、返り値は常にリストとして返す。"
  [f g]
  (let [f-arity (get-arity f)
        g-arity (get-arity g)
        check-arity (assert (= f-arity g-arity)
                            (str "parallel-apply: The number of args should be same.: f: " f-arity ", g: " g-arity))
        the-combination (fn [& args]
                            (assert (= (count args) f-arity)
                                    (str "parallel-apply: The invalid number of args.:" (count args)))
                            (concat
                              (apply f args)
                              (apply g args)))]
    (restrict-arity the-combination f-arity)))

(defn composed-parallel-combine
  "parallel-combineをcomposeでつくる。"
  [h f g]
  (multi-compose h (parallel-apply f g)))


(defn remove-nth
  [i ls]
  (concat (take i ls) (take-last (- (count ls) i 1) ls)))

(defn discard-argument
  "次の関数を返す：
   - 関数fを受け取り、fのarity+1引数をとる関数を返す。
   - ith引数を無視する"
  [i]
  (fn [f]
      (let [f-arity (get-arity f)
            ret-arity (inc f-arity)
            check-f-arity-in-range-i (assert (< i ret-arity)
                                             (str "Invalid arity: arity of f should be larger than " i))
            the-composition (fn [& args]
                                (assert (= (count args) ret-arity)
                                        (str "Invalid number of args: " (count args) ", expect: " ret-arity))
                                (apply f (remove-nth i args)))]
        (restrict-arity the-composition ret-arity))))


(defn insert-nth
  [i el ls]
  (concat (take i ls) (list el) (take-last (- (count ls) i) ls)))

;; currying: 一部引数に所与の値を与えて、残りの引数をとる関数を生成するテクニック
(defn curry-argument
  "次の関数を返す：
   - 関数fを受け取り、fの引数のith以外をargsで埋めた１引数関数を返す。"
  [i & args]
  (fn [f]
      (let [f-arity (get-arity f)
            check-f-arity-fits-args-cnt (assert (= f-arity (inc (count args)))
                                                (str "Invalid arity: arity of f shuold be " (inc (count args))))
            the-composition (fn [arg]
                                (apply f (insert-nth i arg args)))]
        (restrict-arity the-composition 1))))

(defn make-permutation
  "permspecに応じて並び替えたlistを返す関数を返す"
  [permspec]
  (fn [ls]
   (map (fn [ith] (nth ls ith)) permspec)))

(defn permute-arguments
  "次の関数を返す：
   - 関数fを受け取り、fに対してpermspecに応じて並び替えた引数を渡す関数を返す。"
  [& permspec]
  (fn [f]
      (let [f-arity (get-arity f)
            check-f-arity-equals-to-permspec-len (assert (= f-arity (count permspec))
                                                         (str "Invalid arity: arity of f should be " (count permspec)))
            permute (make-permutation permspec)
            the-composition (fn [& args]
                                (assert (= f-arity (count args))
                                        (str "Invalid number of args: " (count args) ", expect " f-arity))
                                (apply f (permute args)))]
        (restrict-arity the-composition f-arity))))

(defn multi-discard-argument
  "次の関数を返す：
   - 関数fを受け取り、fのarity+1引数をとる関数を返す。
   - ith引数を無視する
   - fの返り値は多値。vecに入れて表現する。"
  [i]
  (fn [f]
      (let [f-arity (get-arity f)
            ret-arity (inc f-arity)
            check-f-arity-in-range-i (assert (< i ret-arity)
                                             (str "Invalid arity: arity of f should be larger than " i))
            the-composition (fn [& args]
                                (assert (= (count args) ret-arity)
                                        (str "Invalid number of args: " (count args) ", expect: " ret-arity))
                                (apply f (remove-nth i args)))]
        (restrict-arity the-composition ret-arity))))

(defn multi-curry-argument
  "次の関数を返す：
   - 関数fを受け取り、fの引数のith以外をargsで埋めた１引数関数を返す。
   - fの返り値は多値。vecに入れて表現する。"
  [i & args]
  (fn [f]
      (let [f-arity (get-arity f)
            check-f-arity-fits-args-cnt (assert (= f-arity (inc (count args)))
                                                (str "Invalid arity: arity of f shuold be " (inc (count args))))
            the-composition (fn [arg]
                                (apply f (insert-nth i arg args)))]
        (restrict-arity the-composition 1))))

(defn multi-permute-arguments
  "次の関数を返す：
   - 関数fを受け取り、fに対してpermspecに応じて並び替えた引数を渡す関数を返す。
   - fの返り値は多値。vecに入れて表現する。"
  [& permspec]
  (fn [f]
      (let [f-arity (get-arity f)
            check-f-arity-equals-to-permspec-len (assert (= f-arity (count permspec))
                                                         (str "Invalid arity: arity of f should be " (count permspec)))
            permute (make-permutation permspec)
            the-composition (fn [& args]
                                (assert (= f-arity (count args))
                                        (str "Invalid number of args: " (count args) ", expect " f-arity))
                                (apply f (permute args)))]
        (restrict-arity the-composition f-arity))))

(defn remove-nths
  "lisのremove-idxs番目を除いたリストを返す"
  [remove-idxs lis]
  (filter some? (map-indexed (fn [idx el] (if (some #(= idx %) remove-idxs)
                                nil
                                el))
               lis)))

(defn discard-arguments
  "discard-argumentの複数引数版"
  [& idxs]
  (fn [f]
      (let [f-arity (get-arity f)
            ret-arity (+ f-arity (count idxs))
            check-f-arity-in-range-idxs (assert (< (apply max idxs) ret-arity)
                                             (str "Invalid arity: arity of f should be larger than " (apply max idxs)))
            the-composition (fn [& args]
                                (assert (= (count args) ret-arity)
                                        (str "Invalid number of args: " (count args) ", expect: " ret-arity))
                                (apply f (remove-nths idxs args)))]
        (restrict-arity the-composition ret-arity))))

(defn insert-nths
  "lsに値を追加して、idxs番目をelsにする"
  [idxs els ls]
  (assert (= (count idxs) (count els)))
  (assert (< (apply max idxs) (+ (count els) (count ls))))
  (let [zm (zipmap idxs els)]
    (reduce-kv (fn [acc i el] (insert-nth i el acc))
               ls
               zm)))

(defn curry-arguments
  "curry-argumentの複数引数版"
  [idxs & pre-args]
  (fn [f]
      (let [f-arity (get-arity f)
            ret-arity (count idxs)
            check-f-arity-fits-args-cnt (assert (= f-arity (+ (count idxs) (count pre-args)))
                                                (str "Invalid arity: arity of f shuold be " (+ (count idxs) (count pre-args))))
            the-composition (fn [& args]
                                (apply f (insert-nths idxs args pre-args)))]
        (restrict-arity the-composition ret-arity))))

(defn fill-with-default
  [dval lis]
  (map (fn [el] (if (nil? el) dval el)) lis))

(defn force-default-arguments
  "関数にdefault値をもたせる関数。nilな値を勝手に書き換えるので実用上不便そう"
  [default-val]
  (fn [f]
      (let [f-arity (get-arity f)
            the-composition (fn [& args]
                                (apply f (fill-with-default default-val args)))]
        (restrict-arity the-composition f-arity))))

(defn default-arguments
  "force-default-argumentsは関数定義時点でdefault値を指定するので不便そう。呼び出し時点で受け取れるようにする。"
  []
  (fn [f]
      (let [f-arity (get-arity f)
            ret-arity (inc f-arity)
            the-composition (fn [defval & args]
                                (assert (= (count args) f-arity))
                                (apply f (fill-with-default defval args)))]
        (restrict-arity the-composition ret-arity))))

(defn serial-compose
  "複数の関数を直列に合成するcompose"
  [& fs]
  (reduce (fn [acc f] (compose acc f))
          fs))
