# how to setup
'''
docker-compose up --build -d
lein repl :connect localhost:46123
'''

test (exercise)
'''
docker-compose exec clojure bash
lein test
'''

## Domain-Specific Language

DSLでFlexibilityアップ！

- 問題としてるドメインに関わる名詞動詞の抽象化。ドメインの言葉で実装ができる。いわばドメインのモデル。
- 特定の問題のみでなく、ドメイン全体を包括するようにDSLレイヤーを用意する。関連する別のプログラムを書くときなど、アプリケーションに何かを追加する際の基盤となる。

# Combinator
階層的な構造をもったカスタムパーツと組み合わせ
組み合わせること自体の抽象化はあまりされてない => Combinator System

- 新たな要求への対応は構成要素の入れ替えに帰着される
- 構成要素間の意図せぬ干渉を防ぐ

Systemの要素がDSLの単語、Combinatorがその単語の組み合わせであるフレーズに対応。

どう実現するのか？
- identify a set of primitive components and a set of combinators.
  - they have the same interface!

例：Function combinators
- 関数の合成
  - f ○ g receives input in g's domain and produces output in f's codomain.
    - ここでのdomainは定義域。codomainは終域。
  - combinationの振る舞いはパーツの振る舞いとその組み合わせ方にに依存してる
    - 各パーツの振る舞いはどのコンテキストの影響も受けない。
    - easy to write, read,and verify
    - パーツの追加が既存のパーツに影響しないので、プログラムはextensibleになる。


# regexpの例

# Wrapper - adaptorの導入

# Domainの例 - ボードゲーム
