# Nesting data integrity investigation

Investigated: 2026-10-01

## 結果と対象

2026-10-01の固定ソースと依存構成で、`nest_with_margins()`と
`nest_by_with_margins()`のlocal／dtplyr経路を検証した。正常対照が成立した
1,334件はすべて成功し、確認済みmarginplyr契約違反は0件だった。
48件はpacked／matrix列がdata.tableへの入力変換で展開されるため、元の
構造を保つ正常入力としては非適用だった。変換後の列を入力として扱った
別の対照は成功した。件数はfixture・経路・optionの組合せを数えたもので、
独立した不具合類型の数でも、未検証領域の保証でもない。

対象HEADは`faefeb6ec0c14e64103410c964e6f89412ee9b86`だった。
開始時のブランチは`main`、作業ツリーはcleanだった。SQLクエリ境界の
修正を含むこのHEADを、`git archive`でリポジトリ外へ固定し、隔離libraryへ
インストールした。測定では公開APIを使い、製品patch、trace、namespace
差替え、製品の内部planを正解とする生成は行わなかった。
関連テストとlintのプロセスだけが`pkgload::load_all()`を使用した。

実験終了後、固定コピーと作業ツリーの全追跡ファイルの内容は一致した。
`R/`のファイルを相対パス順に並べ、各パス、NUL、内容、NULをSHA-256へ
与えたdigestは次だった。

```text
44db54801a5e06f2230a71b4bb15ea98b3f97265aff58b973d05a0a9594566e2
```

実験用ソース・スクリプト・結果・ログは`/private/tmp`の専用領域に置いた。
リポジトリへの追加はこのノートだけで、過去の調査、通常library、既存データ、
製品コード、恒久tests、依存宣言、CI、公開契約は変更しなかった。

## 環境と一次資料

macOS arm64、R 4.6.1で実行した。必要な58個の依存パッケージのclosureと
版・元のロード場所を記録し、通常libraryから専用libraryへコピーした。
`R_LIBS_USER`を専用library、`R_LIBS_SITE`を空にして、`Rscript --vanilla`の
別プロセスで測定した。R本体のlibraryを除き、通常libraryは測定の検索先に
含めなかった。対象marginplyrのロード場所も専用libraryであることを確認した。

| パッケージ／ツール | 版 |
|---|---|
| marginplyr | 0.1.0、上記HEADからインストール |
| dplyr / tidyr | 1.2.1 / 1.3.2 |
| dtplyr / data.table | 1.3.3 / 1.18.6.1 |
| vctrs / tibble | 0.7.3 / 3.3.1 |
| rlang / tidyselect | 1.3.0 / 1.2.1 |
| testthat / pkgload | 3.3.2 / 1.5.3 |
| lintr / jarl | 3.4.0 / 0.6.0 |

条件判断では次の一次資料を読んだ。空要素の消失や参照の共有を、単に
見かけが異なるという理由でmarginplyrの不具合にしなかった。

- [tidyr nest](https://tidyr.tidyverse.org/reference/nest.html)：外側キー、内側列、`.key = NULL`。
- [tidyr unnest](https://tidyr.tidyverse.org/reference/unnest.html)：空要素、`keep_empty`、`names_sep`、共通型と名前衝突。
- [dplyr rowwise](https://dplyr.tidyverse.org/reference/rowwise.html)：セル単位のlist抽出と後続計算。
- [data.table reference semantics](https://rdatatable.gitlab.io/data.table/articles/datatable-reference-semantics.html)：`:=`と独立した比較用copy。

## 契約、既存証拠、今回の差分

[CONTEXT](../CONTEXT.md)と適用される[AGENTS](../AGENTS.md)、公開Rdとその
roxygenソースを読み、ADR 0009・0012・0016・0018・0020・0029を根拠とした。
[ADR 0033](../design/adr/0033-preserve-declared-types-in-empty-margin-results.md)
の`.id`の空結果型も確認した。属性保存については
[ADR 0016](../design/adr/0016-delegate-result-class-and-attributes-to-dplyr.md)
の責任境界を採用し、新しい保証は作らなかった。

実装では、[nest_with_margins](../R/nest_with_margins.R)の元キーコピー、
identity列、union、factor復元、`nest_cell_expr()`、セルへのfoldを調べた。
[nest_by_with_margins](../R/nest_by_with_margins.R)の取得、キーなし空入力の
補完、rowwise化も調べた。共有union経路は
[grouping-adapter-union](../R/grouping-adapter-union.R)で確認した。

| 検査族・契約根拠 | 読んだ既存assertionと既存の実行証拠 | 今回の追加・理由 |
|---|---|---|
| 各group・setの元行、`.keep`：nesting reference | [test-nest-operation](../tests/testthat/test-nest-operation.R)は元行ID、置換関係、キー射影、両verbの関係を検査する。所属対照にはexpandも使う | 手書き所属表と直接の元入力sliceを正解にし、共有展開の誤りを独立に検出 |
| 表示と元キー：Display labels and grouping identity、ADR 0012 | 同テストは近接doubleとDST反復時刻、keep時の元キーを検査する | 同じ表示になる二つのdimension、固定区分、任意set、idなし、取得順序を交差 |
| 特殊名と重複：内部名非予約、nesting reference | [test-dtplyr-special-nesting](../tests/testthat/test-dtplyr-special-nesting.R)はtyped outerとセルの多重集合、元入力を検査する | list構造、セル列名、固定キー、dimension、取得順序を交差 |
| factor：ADR 0012、復元の例外 | [test-margin-label](../tests/testthat/test-margin-label.R)はpayload、dimensionコピー、固定キーコピーのlevels・code・orderedを検査する。[test-factor](../tests/testthat/test-factor.R)は復元を検査する | 未使用level、NA levelと欠損code、複数dimension、後続利用を同時検査 |
| packed／matrix：typed-missingの公開契約、ADR 0016 | [test-packed-grouping](../tests/testthat/test-packed-grouping.R)は主に外側キーとセル行数。[test-matrix-grouping](../tests/testthat/test-matrix-grouping.R)は外側shapeとtotalセルの値 | セル内payloadとkeepされたキーの全構造を元sliceと比較 |
| 空・ゼロ列：Relationship to tidyr and dplyr | test-nest-operationは独立行数期待値、両verbの空入力差、変換限界を検査する | id、drop、keep、sort、空要素unnest、0列のpacked／matrix payloadを交差 |
| 入力保全：Mutable-step政策、公開入力契約 | [test-data-frame-subclass](../tests/testthat/test-data-frame-subclass.R)はraw data.tableを独立copyと比較する | 構築・取得・再取得・読み取り後の入力と、既取得セルを比較 |

過去の[property-based testing調査](property-based-testing-margin-semantics.md)
は非空nesting、1–3 rollup dimensions、スカラー中心の生成入力について
元行関係を検証していた。ただしexpandとの関係を含むため、それだけを正解に
しなかった。[metamorphic調査](metamorphic-testing-margin-semantics.md)と
[9月24日の調査](bug-hunt-2026-09-24.md)、
[9月26日の組合せ調査](bug-hunt-2026-09-26-edge-combinations.md)には
独立対照によるlocal nestingの証拠があった。
[downstream integration調査](downstream-integration-2026-10-01.md)のW5は
インストール済み外部consumerから値、levels、rowwiseを検証していた。
それぞれの対象版・入力範囲での過去の証拠として再利用し、現在HEAD全体の
証明とは扱わなかった。

採用した境界は次だった。

- `.keep = TRUE`は元の固定キーとdimensionをセルに保持する。外側ラベルで置換しない。
- `FALSE`はそのキーだけを除き、`.id`は両方で外側にだけ存在する。
- duplicate setのdropはsetを再番号付けする。元行の完全重複は削除しない。
- `.duplicates = "keep"`は両nesting verbで拒否される。
- `nest`はungrouped、dtplyrではlazy。`nest_by`は取得してvisible keysとidでrowwise化する。
- キーなし空入力はnestで外側0行、nest_byで空セル1個。キーあり空入力は両方で外側0行。
- `.key = NULL`はnestでは`"data"`、nest_byでは拒否される。
- ゼロ列セルも所属元行数を保持する。N行0列入力のdtplyr変換時の行消失は別境界。
- セルのdata.frame性を検査するが、具体的subclassやlist-of subclassは一律に要求しない。
- セル内の行順は多重集合で比較する。外側のMargin orderは要求したsortだけで別に検査する。

## 独立した元行対応と校正

代表fixtureは、次の8行だった。IDはpayloadに置き、Groupingにも重複除去にも
使用しなかった。各詳細セルは2行・amount合計10で、外側の行数と合計だけでは
セル交換を検出できない。

| 元行ID | fixed | region | store | amount | tag |
|---|---|---|---|---|---|
| 1, 2 | N | a | x | 2, 8 | p1, p2 |
| 3, 4 | N | b | x | 4, 6 | p3, p4 |
| 5, 6 | S | a | x | 1, 9 | p5, p6 |
| 7, 8 | S | b | NA | 3, 7 | p7, p8 |

fixedを`.by`とし、最後に詳細setを重複指定してdropした。正解setと所属は
実装のplanやexpandから生成せず、次の表で定義した。

| set | 含むdimension | 元行対応 |
|---|---|---|
| 1 | region, store | N/a/x→1,2；N/b/x→3,4；S/a/x→5,6；S/b/NA→7,8 |
| 2 | region | N/a→1,2；N/b→3,4；S/a→5,6；S/b→7,8 |
| 3 | なし | N→1,2,3,4；S→5,6,7,8 |
| 4 | store | N/x→1,2,3,4；S/x→5,6；S/NA→7,8 |

期待する外側は13セル、全セルの元行数は32、各元行は4コピーだった。
後続unnestもこの多重度を期待した。複数粒度を一緒に再集計した二重計上や、
元データが一度だけ戻らないことを不具合とは数えなかった。

完全重複行の系列では、元行1と7をそれぞれ複製し、IDも渡さない入力を使った。
行集合をunique化せず、多重度を検査した。ゼロ列payloadの系列では、IDを
追加せず外部行位置表から期待行数を作った。ゼロ列セルから元行のidentityを
直接読めたとは主張しない。

比較器はouterとcellを一対一に照合し、各cellの行数、列名・列順、空prototype、
行の多重集合、列値の構造を検査した。listのNULLを含む位置と長さ、入れ子、
要素の型・長さ、data.frameの次元、matrixの型・shape、factor code・levels・
ordered、Date／POSIXct／difftimeを比較した。表示が重なるouterではcellも含む
組を一つずつ消費し、公開結果で区別できない元キーに任意の順位を割り当てなかった。

構築前の構造確認をした後、8種類の合成した誤結果を比較器へ渡した。
同じ行数・合計のセル交換、行欠落、完全重複の一つの削除、NULL要素の削除、
NULLの型付き空要素への置換、元キーの表示ラベル化、factorの文字列化、
matrixのshape変更をすべて拒否した。許容するセル内の行置換は受理した。
製品コードから誤結果を作るためのpatchは行わなかった。

## 実行結果と重要な観測

スカラーで小規模試行を成立させてから、共有経路と具体的な仮説に沿って広げた。
実行回数・発見数による上限や停止基準は置かなかった。fixtureとその構造的な
組合せは決定的で、ランダムstreamの件数やseedで終了を決めなかった。

| 検査族 | 正常対照成功 | 非適用 | 結果 |
|---|---:|---:|---|
| 基本13fixture、4入力経路、両verb、keep、sort；plan・label相互作用 | 864 | 48 | セル所属、全構造、元キー、Margin order、計算とunnestが対照と一致 |
| 単独set、変換後入力、空境界、NAキー、packed／matrixキー、grouped入力、行置換 | 214 | 0 | 公開契約と独立sliceに一致 |
| A/B取得、再利用、取得境界、Mutable-step拒否、特殊dimension／固定キー／セル列名 | 104 | 0 | 入力・既取得結果の意図しない変更なし |
| NULLだけのpayload、型付き空要素、内側data.table、0列packed／matrix、product、idなし表示同値、3 dimensions | 152 | 0 | 構造・多重度・拒否条件が対照に一致 |
| 合計 | 1,334 | 48 | 確認済み契約違反0件、最終実験でwarning 0件 |

4入力経路はbase data.frame、tibble、raw data.tableのlocal経路、immutable
`lazy_dt`だった。基本fixture系列にはスカラー、list atoms、list内data.frame／
list、packed、matrix、factor、日時、表示同値、全列キー、1列payload、
完全重複、factorキー、特殊payload名を含めた。

- 基本fixtureの13セルと32コピーを保持した。固定区分が違う同じ子キーが混ざらず、完全重複行も消えなかった。
- list atomsの要素長は`0,0,1,1,3,0,1,2`だった。最初のNULLはNULLのまま、型付き長さ0とNAも区別された。
- list内の0行0列、0行1列、1／2行のframe、N行0列frame、さらに内側のlistは構造を保持した。
- factorの未使用levelとordered、NA levelを指すcodeと本物の欠損codeを保持した。keepコピーにはsynthetic Margin levelが混入しなかった。
- matrixの各詳細sliceは2行3列、整数のままだった。0列matrix／packed payloadも元入力の行数に対応した。
- 二つのdimensionで表示が同じになる近接doubleとDST反復時刻を使っても、元行の分割とkeep元キーは対照と一致した。idなしの系列も一致した。
- 任意Grouping sets、true composite、cube、rollup、product、3 dimensionsとmissing fixed keysを検査した。drop後のidとセル数は独立set定義に一致した。
- Margin orderはfixed keyの欠損・値、各dimensionのGrouping bit・欠損・値、set idの順序項を独立に作って検査した。セル内の順序とは別判定にした。
- セル計算はnestに`lapply()`、nest_byに適切なrowwise `mutate()`を使い、元sliceにも同じ処理を適用した。行数・合計に加え、全元行とlistの型・長さを観測した。
- unnestはungroup後、セル列と`names_sep = "__"`を明示した。outer表示キーとinner元キーを分離し、両`keep_empty`で独立セル群と一致した。
- A→B→AとB→A→Bの取得では、keep／Groupingの違う結果も元入力も変わらなかった。rootは`data.table::copy()`との`identical()`で段階別に検査した。
- 取得済みセルを適切にungroupして別のMargin nestingへ渡す系列も、元sliceからの対照と一致した。rowwise入力をそのまま渡す呼び方は既存契約どおり拒否された。
- nestの返却はdtplyr step、nest_byの返却はlocal rowwiseだった。show_queryと取得経路を確認した。Mutable rootは両verbで変更前に拒否された。

### 候補の分類と棄却理由

| 初期候補 | 独立確認 | 最終分類 |
|---|---|---|
| packed／matrixの列が消える・増える | Margin前の`as.data.table()`だけで`payload.code`／`payload.day`や`payload.u`等へ展開。8行は保持 | 上流仕様／入力変換による差。48組合せは元構造の検査として非適用 |
| local matrixでdimnamesが消える | `dplyr::union_all()`単独で同じ消失。単独setではcolnamesを保持。値・型・shapeは一致 | 上流属性規則。ADR 0016の非保証であり、製品バグではない |
| 空rowwise結果へのセル計算が失敗 | 0行で評価されるlistをframeとして読んだ検証コードが原因 | 利用側の処理・期待値の誤り。0行では対象セルがないことを扱って再実行 |
| 内側data.tableがserialize後に不一致 | 値・型・次元は同じで`.internal.selfref`の外部pointerだけが比較を壊した | 検証器の問題。pointerと由来のrow.namesを意味的比較から除き、実データを比較 |
| 実行counter入りのderived dtplyr入力が拒否される | metadata取得で元行を評価し得る式を含む。counterは0のままで拒否 | 安全なmetadata取得の既存契約どおり。拒否を迂回しない |
| `.SD`をdimensionとセル列名の両方に使用すると拒否 | `.key`はGrouping列と同名にできない。別名で正常対照を成立させた | 利用側の呼び方の誤り |

検証器を修正したときは、該当系列と校正を再実行した。matrixの上流属性規則は
独立した元行所属とshapeを緩める理由にはせず、複数setの属性対照だけを上流の
`vec_c()`で確認した。内側data.tableの意味的比較でも、そのclass、列、値、
次元は残した。rootの変更監視は独立copyとの直接比較を維持した。

## Issue照合と委任判断

GitHubのnest関連Issue／PRを全状態で検索し、既存指摘と修正履歴を照合した。
特に次のIssueの本文・コメントを読み、関連PRも確認した。

- [#175](https://github.com/sayuks/marginplyr/issues/175)：ゼロ列セルの元行数、[PR #183](https://github.com/sayuks/marginplyr/pull/183)。
- [#421](https://github.com/sayuks/marginplyr/issues/421)：セル内NA factor level、[PR #425](https://github.com/sayuks/marginplyr/pull/425)。
- [#424](https://github.com/sayuks/marginplyr/issues/424)：callee formalと列名、[PR #426](https://github.com/sayuks/marginplyr/pull/426)。
- [#531](https://github.com/sayuks/marginplyr/issues/531)：nesting membershipの関係検査、[PR #536](https://github.com/sayuks/marginplyr/pull/536)。
- [#620](https://github.com/sayuks/marginplyr/issues/620)：型付き元キーのidentity、[PR #633](https://github.com/sayuks/marginplyr/pull/633)。
- [#717](https://github.com/sayuks/marginplyr/issues/717)：特殊キーをnesting foldで保持。
- [#468](https://github.com/sayuks/marginplyr/issues/468)：Mutable-stepの上流報告草案。今回のimmutable入力の正常結果を、この未解決の上流問題の修正証拠とはしなかった。

`to-tickets`を読み、ユーザーの事前委任に従ってCodexが分類・投稿要否を判断した。
この実行では対応が必要な指摘が残らなかったため、新規Issueは0件、既存Issueへの
追記も0件とした。棄却仮説や検証器の誤りをticketへ変換せず、件数を増やすための
正常報告も投稿しなかった。これはCodexによる委任判断であり、個々の判断を
ユーザーが個別承認したとは扱わなかった。仕様変更、製品修正、PRのマージは
行わなかった。

## 実行可能な代表対照

以下のRコードは、必要なguard、fixture、手書き所属表、比較器の校正、両verb、
local／dtplyr、構造化payload、セル計算、unnest、空境界、A/B再取得、
上流属性対照を再実行する代表コードである。全1,382組合せのdriverをそのまま
掲載するものではなく、上表の重要な実測を一時スクリプトなしで再確認するための
自己完結した正常対照である。二つのR blockを順に連結して実行する。

必要なパッケージと版は環境表のとおりで、`marginplyr`は記載HEADから隔離libraryへ
インストールする。通常libraryへインストールし直す必要はない。
行順とセルsubclassの非保証、zero-columnの観測限界は前節のとおりである。

```r
source(system.file("suggests", "guard.R", package = "marginplyr"))
stopifnot(
  marginplyr_suggest_available("dtplyr"),
  marginplyr_suggest_available("tidyr")
)
verbs <- list(
  nest = marginplyr::nest_with_margins,
  nest_by = marginplyr::nest_by_with_margins
)
copy_value <- function(x) unserialize(serialize(x, NULL))
slice_rows <- function(x, rows) {
  tibble::new_tibble(
    lapply(as.list(x), vctrs::vec_slice, i = rows),
    nrow = length(rows)
  )
}
semantic_value <- function(x) {
  if (is.data.frame(x)) {
    metadata <- attributes(x)
    metadata[c("row.names", ".internal.selfref")] <- NULL
    return(list(
      metadata = metadata, dimensions = dim(x),
      columns = lapply(as.list(x), semantic_value)
    ))
  }
  if (is.list(x)) {
    return(list(metadata = attributes(x), elements = lapply(x, semantic_value)))
  }
  x
}
row_values <- function(x) {
  lapply(seq_len(nrow(x)), function(i) {
    lapply(lapply(as.list(x), vctrs::vec_slice, i = i), semantic_value)
  })
}
bag_equal <- function(x, y) {
  if (length(x) != length(y)) return(FALSE)
  remaining <- seq_along(y)
  for (value in x) {
    hit <- remaining[vapply(y[remaining], identical, logical(1L), value)]
    if (!length(hit)) return(FALSE)
    remaining <- remaining[remaining != hit[[1L]]]
  }
  !length(remaining)
}
cell_equal <- function(x, y) {
  tryCatch(
    is.data.frame(x) && is.data.frame(y) &&
      identical(names(x), names(y)) && identical(dim(x), dim(y)) &&
      identical(
        lapply(as.list(x), vctrs::vec_slice, i = integer()),
        lapply(as.list(y), vctrs::vec_slice, i = integer())
      ) && bag_equal(row_values(x), row_values(y)),
    error = function(e) FALSE
  )
}
records <- function(x) {
  x <- dplyr::ungroup(x)
  lapply(seq_len(nrow(x)), function(i) {
    list(
      outer = as.list(slice_rows(x[setdiff(names(x), "data")], i)),
      cell = x$data[[i]]
    )
  })
}
records_equal <- function(actual, expected) {
  if (length(actual) != length(expected)) return(FALSE)
  remaining <- seq_along(expected)
  for (item in actual) {
    hit <- remaining[vapply(expected[remaining], function(value) {
      identical(item$outer, value$outer) && cell_equal(item$cell, value$cell)
    }, logical(1L))]
    if (!length(hit)) return(FALSE)
    remaining <- remaining[remaining != hit[[1L]]]
  }
  !length(remaining)
}
x <- tibble::tibble(
  fixed = rep(c("N", "S"), each = 4L),
  region = rep(rep(c("a", "b"), each = 2L), 2L),
  store = c(rep("x", 6L), NA_character_, NA_character_),
  row_id = seq_len(8L), amount = c(2L, 8L, 4L, 6L, 1L, 9L, 3L, 7L),
  tag = paste0("p", seq_len(8L))
)
sets <- list(c("region", "store"), "region", character(), "store")
membership <- list(
  list(1:2, 3:4, 5:6, 7:8), list(1:2, 3:4, 5:6, 7:8),
  list(1:4, 5:8), list(1:4, 5:6, 7:8)
)
spec <- marginplyr::grouping_sets(
  marginplyr::grouping_set(region, store),
  marginplyr::grouping_set(region), marginplyr::grouping_set(),
  marginplyr::grouping_set(store), marginplyr::grouping_set(region, store)
)
expected_records <- function(input, keep) {
  expected <- list()
  columns <- if (keep) names(input) else setdiff(
    names(input), c("fixed", "region", "store")
  )
  for (set in seq_along(sets)) {
    for (rows in membership[[set]]) {
      outer <- as.list(slice_rows(
        input[c("fixed", "region", "store")], rows[1L]
      ))
      for (column in setdiff(c("region", "store"), sets[[set]])) {
        outer[column] <- list(NA_character_)
      }
      outer$set <- as.integer(set)
      expected[[length(expected) + 1L]] <- list(
        outer = outer, cell = slice_rows(input[columns], rows)
      )
    }
  }
  expected
}
inspect_cell <- function(cell) {
  list(
    rows = nrow(cell),
    amount = if ("amount" %in% names(cell)) sum(cell$amount) else NULL,
    details = row_values(cell)
  )
}
fixtures <- list(scalar = x, list_atoms = x, list_frames = x, factor = x)
fixtures$list_atoms$payload <- list(
  NULL, integer(), NA_integer_, 1L, 1:3, character(),
  NA_character_, c("a", "b")
)
fixtures$list_frames$payload <- list(
  data.frame(), data.frame(z = integer()), data.frame(z = 1L),
  data.frame(z = 1:2), list(NULL, list(integer())),
  list(a = NA_integer_, b = character()), data.frame(row.names = 1:2),
  list(inner = list(data.frame(z = 2L)))
)
fixtures$factor$payload <- ordered(
  c("b", "a", NA, "b", "a", "b", NA, "a"),
  levels = c("a", "b", "unused")
)
fixtures$factor$na_level <- factor(
  c("x", NA, "x", NA, "x", NA, "x", NA),
  levels = c("x", "unused", NA), exclude = NULL
)
fixtures$factor$na_level[3L] <- NA
fixtures$packed <- x
fixtures$packed$payload <- tibble::tibble(
  code = seq_len(8L), day = as.Date("2026-01-01") + seq_len(8L)
)
fixtures$matrix <- x
fixtures$matrix$payload <- matrix(seq_len(24L), nrow = 8L)
fixtures$time <- x
fixtures$time$day <- as.Date("2026-01-01") + seq_len(8L)
fixtures$time$instant <- as.POSIXct(
  "2026-01-01", tz = "Asia/Tokyo"
) + seq_len(8L)
fixtures$keys_only <- x[c("fixed", "region", "store")]
fixtures$duplicates <- slice_rows(x, c(1L, 1L, 3:8))
fixtures$duplicates$row_id <- NULL

# Calibration uses synthetic wrong results, never a modified package.
expected <- expected_records(fixtures$list_atoms, TRUE)
stopifnot(records_equal(expected, expected))
bad <- copy_value(expected)
bad[[1L]]$cell <- expected[[2L]]$cell
bad[[2L]]$cell <- expected[[1L]]$cell
stopifnot(!records_equal(bad, expected))
bad <- copy_value(expected)
bad[[1L]]$cell <- slice_rows(bad[[1L]]$cell, 1L)
stopifnot(!records_equal(bad, expected))
bad <- copy_value(expected)
shortened <- as.list(bad[[1L]]$cell)
shortened$payload[[1L]] <- NULL
bad[[1L]]$cell <- tibble::new_tibble(shortened, nrow = 2L)
stopifnot(!records_equal(bad, expected))
bad <- copy_value(expected)
bad[[1L]]$cell$region <- rep("Total", 2L)
stopifnot(!records_equal(bad, expected))
for (fixture in c("factor", "matrix", "duplicates")) {
  expected <- expected_records(fixtures[[fixture]], FALSE)
  bad <- copy_value(expected)
  if (fixture == "factor") {
    bad[[1L]]$cell$payload <- as.character(bad[[1L]]$cell$payload)
  } else if (fixture == "matrix") {
    bad[[1L]]$cell$payload <- bad[[1L]]$cell$payload[, 1L, drop = FALSE]
  } else {
    bad[[1L]]$cell <- slice_rows(bad[[1L]]$cell, 1L)
  }
  stopifnot(!records_equal(bad, expected))
}
good <- copy_value(expected)
good[[1L]]$cell <- slice_rows(good[[1L]]$cell, 2:1)
stopifnot(records_equal(good, expected))

run_normal <- function(input, backend, verb, keep) {
  source <- switch(
    backend, df = as.data.frame(input), tbl = input,
    dtplyr = dtplyr::lazy_dt(data.table::as.data.table(copy_value(input)))
  )
  collect_value <- function(value) {
    if (is.data.frame(value)) value else dplyr::collect(value)
  }
  before <- copy_value(collect_value(source))
  stopifnot(cell_equal(before, input))
  result <- verbs[[verb]](
    source, .by = dplyr::all_of("fixed"), .grouping = spec,
    .duplicates = "drop", .margin_label = NULL, .id = "set",
    .keep = keep, .sort = "last"
  )
  if (backend == "dtplyr" && verb == "nest") {
    stopifnot(inherits(result, "dtplyr_step"))
  }
  result <- collect_value(result)
  stopifnot(identical(
    dplyr::group_vars(result),
    if (verb == "nest") character() else c("fixed", "region", "store", "set")
  ))
  expected <- expected_records(input, keep)
  actual <- records(result)
  stopifnot(records_equal(actual, expected))
  stopifnot(sum(vapply(result$data, nrow, integer(1L))) == 32L)
  observed <- lapply(result$data, inspect_cell)
  if (verb == "nest_by") {
    observed <- dplyr::mutate(
      result, observed = list(inspect_cell(!!rlang::sym("data")))
    )$observed
  }
  reference <- dplyr::ungroup(copy_value(result))
  for (i in seq_along(actual)) {
    hit <- which(vapply(expected, function(value) {
      identical(actual[[i]]$outer, value$outer) &&
        cell_equal(actual[[i]]$cell, value$cell)
    }, logical(1L)))[1L]
    reference$data[i] <- list(expected[[hit]]$cell)
    stopifnot(bag_equal(
      observed[[i]]$details, inspect_cell(expected[[hit]]$cell)$details
    ))
  }
  for (keep_empty in c(FALSE, TRUE)) {
    flat <- tidyr::unnest(
      dplyr::ungroup(result), dplyr::all_of("data"),
      names_sep = "__", keep_empty = keep_empty
    )
    control <- tidyr::unnest(
      reference, dplyr::all_of("data"), names_sep = "__",
      keep_empty = keep_empty
    )
    stopifnot(cell_equal(flat, control), nrow(flat) == 32L)
  }
  stopifnot(cell_equal(collect_value(source), before))
  invisible(result)
}
for (fixture in names(fixtures)) {
  backends <- if (fixture %in% c("packed", "matrix")) {
    c("df", "tbl")
  } else {
    c("df", "tbl", "dtplyr")
  }
  for (backend in backends) {
    for (verb in names(verbs)) {
      for (keep in c(FALSE, TRUE)) {
        run_normal(fixtures[[fixture]], backend, verb, keep)
      }
    }
  }
}
cat("Independent membership, structure, calibration and workflows passed.\n")
```

空入力・再取得・上流対照は、上のsetupと比較器を続けて使う。

```r
for (verb in names(verbs)) {
  input <- tibble::tibble(
    payload = list(NULL, integer(), NA_integer_, 1:2)
  )
  result <- verbs[[verb]](input)
  stopifnot(nrow(result) == 1L, cell_equal(result$data[[1L]], input))
  for (backend in c("tbl", "dtplyr")) {
    empty <- input[integer(), , drop = FALSE]
    source <- if (backend == "dtplyr") dtplyr::lazy_dt(empty) else empty
    result <- verbs[[verb]](source, .id = "set")
    if (!is.data.frame(result)) result <- dplyr::collect(result)
    count <- if (verb == "nest") 0L else 1L
    stopifnot(nrow(result) == count)
    stopifnot(identical(result$set, if (count) 1L else integer()))
    if (count) stopifnot(cell_equal(result$data[[1L]], empty))
    for (keep_empty in c(FALSE, TRUE)) {
      flat <- tidyr::unnest(
        dplyr::ungroup(result), dplyr::all_of("data"),
        names_sep = "__", keep_empty = keep_empty
      )
      stopifnot(nrow(flat) == if (keep_empty) count else 0L)
    }
  }
  rows_only <- tibble::tibble(.rows = 3L)
  result <- verbs[[verb]](rows_only)
  stopifnot(identical(dim(result$data[[1L]]), c(3L, 0L)))
  stopifnot(identical(
    dim(dplyr::collect(dtplyr::lazy_dt(rows_only))), c(0L, 0L)
  ))
  rejected <- tryCatch(verbs[[verb]](NULL), error = identity)
  stopifnot(inherits(rejected, "marginplyr_error"))
  rejected <- tryCatch(verbs[[verb]](x, .duplicates = "keep"), error = identity)
  stopifnot(inherits(rejected, "marginplyr_error"))
  nullable <- tryCatch(verbs[[verb]](x, .key = NULL), error = identity)
  if (verb == "nest") {
    stopifnot("data" %in% names(nullable))
  } else {
    stopifnot(inherits(nullable, "marginplyr_error"))
  }
}

# Independent reference cells also own the A/B comparisons.
for (order in list(c("A", "B", "A"), c("B", "A", "B"))) {
  for (verb in names(verbs)) {
    input <- fixtures$list_frames
    root <- data.table::as.data.table(copy_value(input))
    original <- data.table::copy(root)
    source <- dtplyr::lazy_dt(root, immutable = TRUE)
    queries <- list(
      A = verbs[[verb]](
        source, .by = dplyr::all_of("fixed"), .grouping = spec,
        .duplicates = "drop", .margin_label = NULL, .id = "set",
        .keep = TRUE, .sort = "last"
      ),
      B = verbs[[verb]](
        source, .by = dplyr::all_of("fixed"),
        .grouping = marginplyr::grouping_set(region, store),
        .margin_label = NULL, .id = "set", .keep = FALSE, .sort = "first"
      )
    )
    stopifnot(identical(root, original))
    saved <- list()
    snapshots <- list()
    for (which in order) {
      value <- queries[[which]]
      if (!is.data.frame(value)) value <- dplyr::collect(value)
      expected <- expected_records(input, which == "A")
      if (which == "B") expected <- expected[seq_len(4L)]
      stopifnot(records_equal(records(value), expected))
      lapply(value$data, inspect_cell)
      for (prior in names(saved)) {
        stopifnot(records_equal(records(saved[[prior]]), snapshots[[prior]]))
      }
      saved[which] <- list(value)
      snapshots[which] <- list(copy_value(records(value)))
      stopifnot(identical(root, original))
    }
  }
}

# The dimnames difference arises in upstream union, before a cell is built.
m <- x
m$payload <- matrix(
  seq_len(24L), nrow = 8L, dimnames = list(NULL, c("u", "v", "w"))
)
union <- dplyr::union_all(m, m)
stopifnot(is.null(dimnames(union$payload)), ncol(union$payload) == 3L)
result <- marginplyr::nest_with_margins(
  m, .by = dplyr::all_of("fixed"), .grouping = spec,
  .duplicates = "drop", .margin_label = NULL, .id = "set"
)
stopifnot(is.null(dimnames(result$data[[1L]]$payload)))
single <- marginplyr::nest_with_margins(
  m, .by = dplyr::all_of("fixed"),
  .grouping = marginplyr::grouping_set(region, store), .margin_label = NULL
)
stopifnot(identical(colnames(single$data[[1L]]$payload), c("u", "v", "w")))
for (fixture in c("packed", "matrix")) {
  converted <- data.table::as.data.table(fixtures[[fixture]])
  stopifnot(nrow(converted) == 8L, !("payload" %in% names(converted)))
}
cat("Empty boundaries, A/B reuse and upstream controls passed.\n")
```

## チェック、終了理由、再実行方法

関連7テストファイルを固定コピーの別プロセスで実行し、terminal exit 0を確認した。
6ファイルのfocused実行は失敗0・skip 11、factor単独実行は失敗0・skip 1だった。
skipは隔離libraryへ入れなかったArrow、RSQLite、DuckDBの検査で、今回の
local／dtplyr検査に必要な依存は満たしていた。full package suiteやtarball checkを
実行したとは主張しない。

- `jarl check .`：成功、0.6.0、現在の`jarl.toml`を使用。
- `pkgload::load_all()`後の`lintr::lint_package()`：成功、lintなし。
- 最終MarkdownのR blockをリポジトリ外へ抽出・連結：掲載コードとbyte一致。
- 抽出コードを隔離processで実行：成功。jarlとnamespaceをloadしたlintr：成功。
- context budget：20,008 bytes、22,005-byte baseline以内。
- verifier invocationとdocument referencesの検証：成功。
- `git diff --check`：成功。公開する差分はこのノート1ファイルのみ。

このノートはRepository-onlyで、生成・インストール・package checkの入力を
変更しないため、`design/agents/local-checks.md`に従いreview-ready checkは
非適用とした。調査ノートのcode reviewは実施していない。

採用した具体的仮説について実行証拠と分類がそろい、最終見直しで優先すべき
未処理仮説が残らなかったため終了した。最後にNULLだけのpayload、0列の
構造化payload、3 dimensions、missing fixed keys、idなし表示同値、特殊fixed
keysへ広げ、いずれも正常対照を確認した。件数や資源上限への到達で停止した
ものではなく、続行を妨げる環境・投稿権限の阻害要因は残らなかった。

未検証は他のR・依存版・OS、任意の全型／任意オブジェクト、全Grouping
specification、並行して元入力が変更される場合、利用者が返却後に意図的に`:=`
等を行う場合、environment／R6のdeep-copyである。SQL／Arrow nesting、Mutable
stepの正常実行、rowwise入力の直接再投入、入力変換前のpacked／matrix構造を
保つdtplyrへの対応追加は対象外である。N行0列dtplyr入力の復元も要求しない。

再実行では、まず対象HEADと依存版を照合し、リポジトリ外の新しいソースコピー・
libraryを作る。上の二つのR blockを掲載順に抽出し、専用libraryを検索先とする
`Rscript --vanilla`で実行する。追加探索は対応表の不足に従い、元入力とset表を
独立に定義して行う。失敗した場合は校正と入力変換対照を先に確認し、元行・
payload・set・optionを減らして同じ失敗を保持する最小再現へ縮小する。
同じキャンペーンの追試は、このノートへ日付を伴う追試結果として集約する。
