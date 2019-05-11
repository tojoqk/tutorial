#lang slideshow/widescreen
(require slideshow/code)

(slide
 (t "Racket入門"))

(slide
 #:title "Racketの紹介"
 (item "インストールが簡単")
 (item "教育向け")
 (item "言語指向プログラミングのエコシステムを持つ")
 (item "関数型プログラミング"))

(slide
 #:title "インストールが簡単"
 (item "Windows|macOS|GNU/Linux|Unix")
 (subitem "https://download.racket-lang.org/ でダウンロード")
 (item "Arch Linux")
 (subitem "pacman -S racket")
 (item "macOS (homebrew)")
 (subitem "brew cask install racket"))

(slide
 #:title "教育向け"
 (item "インストールの段階で詰まない")
 (item "統合開発環境付き(DrRacket)")
 (item "エラーメッセージが親切でバグの修正がしやすい")
 (item "ルールがシンプルで理解しやすい")
 (subitem "Schemeがベース言語"))

(slide
 #:title "言語指向プログラミングのエコシステムを持つ"
 (item "#lang の機構によって使用する言語の切り替え")
 (subitem "#lang typed/racket: 型のあるRacket")
 (subitem "#lang lazy: 遅延評価なRacket")
 (subitem "#lang slideshow: スライドショー作成用言語")
 (item "本資料では #lang racket を使用する"))

(slide
 #:title "言語指向プログラミングのエコシステムを持つ"
 (item "強力なマクロ")
 (subitem "sytnax-rules: シンプルなパターンマッチによるマクロ")
 (subitem "syntax-case: 展開時にRacketを実行可能な強化版")
 (subitem "syntax-parse: 構文を抽象化できるさらなる強化版"))

(slide
 #:title "関数型プログラミング"
 (item "式指向")
 (subitem "ほとんど全てが式で構成され0個以上の値を返す")
 (item "第一級の手続き")
 (subitem "手続きを値として扱える")
 (item "イミュータブルなデータ構造")
 (subitem "イミュータブルなリスト(set-car!とset-cdr!がない)")
 (subitem "イミュータブルなhash(ミュータブルなhashもある)"))

(slide
 (t "Racketの基礎"))

(slide
 #:title "定義エリア(Definitions area)と対話エリア(Interactions area)"
 (item "定義エリア")
 (subitem "手続きなどの定義の並び")
 (subitem "スクリプト")
 (subitem "テスト")
 (item "対話環境(REPL)")
 (subitem "定義した手続きや変数の確認")
 (subitem "スクリプトを書かずにREPLのみで完結する場合もある"))

(slide
 #:title "定義エリア"
 (item "DrRacketであれば上のエリアのこと")
 (item "手続きや変数の定義を並べる")
 (item "スクリプト")
 (subitem "DrRacketの右上の「Run」ボタンを押すと実行される")
 (item "プログラムのテスト"))

(slide
 #:title "対話環境(REPL)"
 (item "DrRacketであれば下のエリアのこと")
 (item "「Run」ボタンを押すと定義した変数が読み込まれる")
 (item "\">\"の後に式を入力してEnterキーを押すと値が出力される")
 (item "式の値を求めることを「評価する(evalute)」という"))

(slide
 #:title "手続き呼び出し"
 (item "リストの先頭に手続きを残りには引数を書く")
 (code (proc arg ...))
 (subitem "\"proc\"が手続き")
 (subitem "\"arg ...\"が引数")
 (subitem "\"x ...\"は「xが0個以上並ぶ」という意味")
 (item "式全体の値は手続きの返却値"))

(slide
 #:title "手続き呼び出しの例"
 (item "要素を一つ追加したリストを作る")
 (code
  (cons 'a '(b c d e)) => '(a b c d e))
 (subitem "ここで\"=>\"は「の値は」と読む")
 (item "引数の総和を求める")
 (code
  (+ 10 11 21) => 42)
 (item "文字列かどうかを調べる")
 (code
  (string? 'foo) => #f)
 (code
  (string? "foo") => #t)
 (item "式の値を調べるときはREPLに入力してEnterキーを押す"))

(slide
 #:title "述語について"
 (item "真(#t)か偽(#f)のいずれかを返す手続きのこと")
 (item "ある値xがhogeであるかどうかを調べる")
 (code (hoge? x))
 (item "hoge?が真になるような値のことをhogeと呼ぶ"))

(slide
 #:title "等価性"
 (tt "xとyが等しいとはどういうことか？")
 (item (code (eq? x y)))
 (subitem "同一のオブジェクトかどうか")
 (item (code (eqv? x y)))
 (subitem "数同士と文字同士の比較もできる eq?")
 (item (code (equal? x y)))
 (subitem "見掛け上等しいかどうか"))

(slide
 #:title "eq?の意味で等しい"
 (code (eq? x y))
 (item "xとyは同一のオブジェクトである")
 (subitem "xがミュータブルであればxを変化させるとyも同様に変化する")
 (subitem "構造が完全に共有されていることを意味する")
 (item "高速に比較できる")
 (subitem "計算機のメモリ上の位置を比較するだけでよいので速い")
 (item "数同士と文字同士の等価性を調べてはならない")
 (subitem "同じ値であっても「eq?の意味で等しい」とは限らない"))

(slide
 #:title "eqv?の意味で等しい"
 (code (eqv? x y))
 (item "数同士と文字同士の等価性を調べることができるeq?")
 (item "数値として等しくても型が異なる場合は偽となる")
 (subitem "正確な整数と非正確な整数")
 (code (eqv? 1 1.0) => #f)
 (code (= 1 1.0) => #t)
 (subitem "正確な実数と非正確な実数")
 (code (eqv? 0.5 1/2) => #f)
 (code (= 0.5 1/2) => #t))

(slide
 #:title "equal?の意味で等しい"
 (code (equal? x y))
 (item "xとyが見掛け上等しいことを意味している")
 (subitem "例: ペアの比較")
 (code (define x (cons 'a 'b))
       (define y (cons 'a 'b))
       (eq? x x) => #t
       (eq? x y) => #f
       (equal? x x) => #t
       (equal? x y) => #t)
 (item "構造が共有されているとは限らない")
 (item (code eq?) (code eqv?) "と比較すると遅い"))

(slide
 #:title "基本的なデータ型について"
 (item "シンボル")
 (item "ペア")
 (item "数")
 (item "手続き"))

(slide
 #:title "シンボル"
 (item "述語は" (code symbol?))
 (item "例:" (code 'hoge 'huga))
 (item "同じ名前のシンボルは" (code eq?) "の意味で等しい")
 (code (eq? 'hoge 'huga) => #f)
 (code (eq? 'hoge 'hoge) => #t)
 (tt "名前によって他と識別できることに意味がある場合に使用する")
 (item "シンボル同士のみを比較する" (code symbol=?) "もある"))

(slide
 #:title "数"
 (item "述語は" (code number?))
 (tt "数の分類には二つの軸がある")
 (item "数学的な数の分類")
 (t "整数 ⊂ 有理数 ⊂ 実数 ⊂ 複素数 ⊂ 数")
 (code (integer? 1) => #t)
 (code (complex? 1) => #t)
 (code (integer? 1.0) => #t)
 (item "正確(exact)か非正確(inexact)か")
 (code (exact? 1) => #t)
 (code (exact? 1.0) => #f))
