# Installed downstream integration investigation

Investigated: 2026-10-01

## 結果

2026-10-01のmacOS arm64、R 4.6.1、固定した依存構成で、未改変の
marginplyrと実際にインストールした外部consumerを検証した。最終実行の
395ケースはすべて`NO VIOLATION FOUND`だった。確認済みmarginplyrバグは
0件だった。この件数は採用したfixture・呼び出し経路での結果であり、
パッケージ全体や未実行の組合せの正しさを示すものではない。

外部namespaceの非公開関数、wrapperのローカルscalar、caller由来の
quosure、Grouping specification、返却後に取得するquery、公開genericの
取得経路を確認した。marginplyrとdplyrはattachしなかった。

| backend | 最終ケース数 | 確認した経路 |
|---|---:|---|
| local | 96 | 4種類のMargin verb、alias、specification、字句環境、quosure、nesting、factor、表示 |
| dtplyr | 101 | 外部／consumer内で作った入力、返却後collect/compute、factor生成式、nesting、評価カウンター |
| SQLite | 69 | portable SQL、専用typed結果、render、有限collect、直接compute、audit |
| DuckDB | 69 | native/portable SQL、直接compute、`.env` expansionの専用compute、audit |
| Arrow | 60 | TableとDataset、翻訳可能なsummary/expansion、返却後collect、Grouping helpers、表示 |

Aの直接namespace呼び出しは131ケース、Bの`::` consumerとCの
`importFrom()` consumerは各132ケースだった。差の1ケースは、実際の
外部namespace内で`lazy_dt()`を作る校正をB/Cだけに実施したためだった。
ケースは複数のassertionを含み、395個の独立したAPIや入力を意味しない。

最終一覧は[final-cases.csv](2026-10-01-downstream-integration/final-cases.csv)、
途中の試行と分類は[all-attempts.csv](2026-10-01-downstream-integration/all-attempts.csv)、
実測値・型・SQL・namespace状態は
[evidence](2026-10-01-downstream-integration/evidence/)に保存した。

## 対象と既存証拠との差分

対象HEADは`471b78f64c98049cfa7a43fcc137647d7d4077a9`だった。調査開始前の
作業ツリーはcleanだった。追跡済みファイルを隔離領域へ複製し、そこで
ソースtarballを一度だけbuild・installした。全consumerが同じインストール
済みmarginplyrを使用した。製品コードへのpatch、trace、namespace差替え、
`pkgload::load_all()`、marginplyrのテストヘルパーは使用しなかった。

tarballのSHA-256は次だった。

```text
6707946858221c6c5a90ea5a3e06e988ca4a44fccb7935dcf9c45bd94afd6a9a
```

`source-sha256.tsv`はソースの識別情報、installログは実際のインストール
経路を記録した。別日の再buildはtarballのタイムスタンプ等によりbyteが
変わり得るため、再現時はソース内容も照合する必要がある。

#491の修正`216765a`と後続`dfa38d4`は、dtplyrで後から評価する内部関数の
headを修飾し、marginplyr内部名が見えない環境でのfactor回帰テストと
構造検査を追加していた。今回のW5はその値・levels契約を外部consumerの
namespaceと返却後取得で通した。既存テストの再実行は行わなかった。

既存のwrapper、quosure、share、Contextual helperテストは契約の根拠として
読み、外部のインストール済み関数による捕捉と通常のimports環境を追加した。
`tests/testthat.R`のインストール済み検証は`library(marginplyr)`を使う。
今回のBはconsumerロード時にmarginplyrをimportせず、Cは明示的importを
持つため、consumerロード後のnamespace状態も異なった。

## Consumer、依存、校正

5 backendごとに`mpconsumerqualified<backend>`と
`mpconsumerimports<backend>`を作成し、計10個を実際にインストールした。
`DESCRIPTION`、`NAMESPACE`、`R/consumer.R`、ヘルプを持つ通常のRパッケージ
だった。`.onLoad()`や`.onAttach()`は置かなかった。各測定プロセスでは
対応するB/Cの片方だけをロードした。

Bはmarginplyr/dplyr/dbplyrを`::`で呼び、Cは使用する公開関数だけを
`importFrom()`した。backend別の依存、rlang、tidyselect、utilsを宣言した。
dtplyr構成はdata.tableと`.datatable.aware <- TRUE`も宣言した。
`capture.output()`は最終consumerで`utils::capture.output()`とした。
この宣言修正後に全workflowを最終実行し直した。

[consumer-declarations.csv](2026-10-01-downstream-integration/consumer-declarations.csv)
は最終宣言の監査結果を示す。`consumer-template.R.in`と`prepare.py`から同じ
構成を再生成できる。consumerソースとバイナリは隔離領域にも保持した。

依存は通常ライブラリから読み取り専用で複製し、測定では専用ライブラリと
R本体のlibraryだけを使った。主要版はR 4.6.1、dplyr 1.2.1、dbplyr 2.6.0、
rlang 1.3.0、tidyselect 1.2.1、dtplyr 1.3.3、data.table 1.18.6.1、
DBI 1.3.0、RSQLite 3.53.3、DuckDB 1.5.5、Arrow 25.0.1だった。
完全な依存closure・元の場所は
[dependencies.tsv](2026-10-01-downstream-integration/dependencies.tsv)に保存した。
再生成スクリプトはこの構成と異なる依存版を黙って採用しない。

各プロセスは専用cwdで`Rscript --vanilla`から起動した。初期、consumer
ロード後、構築後、終了時の`search()`、`loadedNamespaces()`、`.libPaths()`、
版・ロード場所を保存した。marginplyr/dplyrのattach、pkgload/testthatの
ロード、通常ユーザーlibraryからのロードがないことをassertした。
Cのimportが公開genericと同一であること、consumer関数の定義環境が
インストール済みnamespaceであることもassertした。

校正で次を確認した。

- 比較器は値変更、integer/doubleの型変更、行削除、行複製、値の入替えを
  すべて拒否した。順序未保証の比較は多重集合とし、重複を消さなかった。
- localでは非公開`ordinary()`がnamespace定数100を参照し、wrapperの7/17を
  加えた。αは`111,113,117`、βは`121,123,127`だった。
- dtplyr入力をconsumer内で作った場合もnamespace関数の期待値になった。
  caller側で作った入力ではcallerの同名関数を参照し、通常dtplyr対照と一致した。
- dtplyrの専用counter fixtureでは構築時0、取得後3の評価だった。
  入力はwrapper終了後も生存させ、取得は同一プロセスで行った。
- SQLiteの専用結果クラスを実際に生成し、公開genericからrender、collect、
  computeを呼んだ。メソッドの存在だけでなく値・型・列・順序を比較した。

backend/generic側namespaceを先にロードする別順序でも、A/B/Cの校正を
5 backendで実施して通った。最終utils宣言後にもSQLiteのB/Cでこの順序を
再確認した。marginplyrのImportsが要求するロードを強制的に逆転させなかった。

## Workflowと期待値

基本fixtureは`g = c("a", "a", "b")`、`v = c(1, 3, 6)`だった。rollupの
合計は`4,6,10`、件数は`2,1,3`、set identifierは`1,1,2`、Grouping bitと
Grouping identifierは`0,0,1`、Parent/Total shareは`0.4,0.6,1`と手計算した。
通常dplyrによるgroup別summaryをconsumer内でも実行し、`a=4,b=6`を確認した。

| ID | consumerの操作 | 確認結果と根拠 |
|---|---|---|
| W1 | `all_of()`文字列選択、`{{}}`、quosure splicing、`.by`転送、オプション既定値 | 直接対照と手計算に一致。固定partitionごとの合計`4,4,6,6`を保持。公開引数説明とADR 0007 |
| W2 | 5種のconstructorによるfactory、α/β specの後日のinspection | 選択列g/vと手書きGrouping setsを各specが保持。公開`How an argument is read`とADR 0026 |
| W3 | 非公開関数、local scalar、caller quosure、query α/βの両取得順 | local/dtplyrの対照に一致。全backendでAlpha/lastとBeta/firstを両順序で取得して混在なし。ADR 0007/0018とbackend通常評価 |
| W4 | Grouping helpers、shares、`across()`、同名の通常関数によるshadow | 認識対象のbare/所有package修飾を確認。shadow関数は実行されず、手計算に一致。ADR 0019、各helperの公開説明 |
| W5 | summary/alias、expansion、nest、nest_by、factor/ordered factor | local/dtplyrで4種類、SQL/Arrowでsummary/expansionを確認。nest payload行数`2,1,3`と値、factor levels、rowwiseを確認。ADR 0012/0016 |
| W6 | 公開genericの内部／返却後呼出し、有限collect、compute | SQLiteのtyped結果と公開列、integerキー、set identifier、順序を保持。dtplyr/DuckDBも内部と外部computeが一致。ADR 0031と公開S3登録 |
| W7 | DuckDB nativeとportable、SQLite portable | nativeに`GROUPING SETS`、portableに`UNION ALL`を観測し、値と直接compute結果が一致。公開`.duplicates`と`.id`を使用 |
| W8 | `.env`列を持つDuckDB expansionのcompute | `marginplyr_duckdb_env`を実際に通り、公開3列と6行のpayloadを保持。専用compute登録 |
| W9 | Arrow TableとParquet Datasetのquery返却・取得 | Table/Datasetとも手計算と直接対照に一致。外部ポインタを同一プロセスで保持 |
| W10 | spec/resultのprint、構築直後のaudit、古いquery取得 | 表示後の値は不変。古いquery取得で新しい構築のSent query記録は変わらず。公開`last_sent_queries()`契約 |

比較器は列順、`typeof()`、class、factor levels、値、行数、Grouping対応、
保証された行順を検査した。浮動小数点の許容誤差は`1e-10`とした。
期待値を内部planや生成SQLから作らなかった。inspect結果自身も手書きの
included列・set identifierと比較した。

## 途中の観測の分類

途中の診断を成功へ読み替えず、原記録と修正後の結果を両方保存した。

| 観測 | 分類 | 対照・結論 |
|---|---|---|
| 最初の記録器がbase namespaceのpath取得を拒否された | HARNESS / ENVIRONMENT ISSUE | baseを個別処理。製品呼出し前の失敗だった |
| R.frameworkのsymlink表記差がlibrary境界assertに抵触 | HARNESS / ENVIRONMENT ISSUE | canonical pathへ正規化し、許可範囲を広げずに修正 |
| dtplyr外部入力の非公開関数がnamespaceではなくroot環境の同名関数を参照 | EXPECTED R / UPSTREAM BEHAVIOR | marginplyrは`10011,10013,10017`、通常dtplyrは先頭2groupで`10011,10013`。consumer内で入力を作ると両者`111,113` |
| dtplyr consumer内入力で`cedta()`の拒否 | CONSUMER PACKAGE ERROR | data.table対応宣言が欠けていた。適切な宣言を追加して校正・最終実行が通った |
| rootに存在しない`counted()`をquosure環境だけに置いたdtplyr式 | EXPECTED R / UPSTREAM BEHAVIOR | marginplyrと通常dtplyrがともに`could not find function "counted"`。正常なcounter fixtureはrootにも関数を保持 |
| SQLite shareを既定のsource checkで要求 | UNSUPPORTED SPELLING OR WORKFLOW | 公開契約どおりsource適格性を確立できず拒否。既知のnumeric aggregateに`check=FALSE`を明示した検証は通った |
| Arrow shareを要求 | UNSUPPORTED SPELLING OR WORKFLOW | 公開仕様の非対応どおり拒否。最終Arrow検証はGrouping helpersと通常summaryへ限定 |
| SQLite `collect(n=0)`に通常summaryの非空時型を要求 | HARNESS / ENVIRONMENT ISSUE | `total`と`rows`は`logical(0)`、通常dbplyrの`total`も`logical(0)`。packageが所有するg/sidは`integer(0)`を保持 |

dtplyrの実測対照と該当版`lazy_dt()`/`dt_eval()`のsourceは
[calibration-controls.R](2026-10-01-downstream-integration/calibration-controls.R)に残した。
`probe-calibration.R`で同じインストール済みconsumerを使って再確認できる。

Arrowロード時にはsandboxがCPU cache情報の`sysctlbyname`を拒否した旨が
nativeログへ出たが、プロセスはexit 0でTable/Dataset検証を完了した。
この環境上の観測もログに保持した。

`CONFIRMED MARGINPLYR BUG`と`CONTRACT QUESTION`へ分類した候補はなかった。
確認済み製品候補がないため、そのための最小バグパッケージ・Issue案は作らなかった。
正常workflowとupstream観測の再現用consumer生成器・コマンド・実測値は保存した。

## 再現、資源、未検証範囲

隔離領域は`/private/tmp/marginplyr-downstream-2026-10-01`だった。そこに
ソースtarball、複製ライブラリ、consumerソース、全ログ、Datasetを保持した。
SQLite/DuckDBは新規の`:memory:`接続を使い、実行終了時に切断した。
既存DBと通常ライブラリへの書込みは行わなかった。

再現はリポジトリrootから次を実行する。元の隔離領域を再利用せず、
未使用の絶対パスを選ぶ。記録した依存版とR環境を用意する必要がある。

```sh
python3 investigation/2026-10-01-downstream-integration/prepare.py \
  "$PWD" /private/tmp/marginplyr-downstream-replay
python3 investigation/2026-10-01-downstream-integration/run.py \
  /private/tmp/marginplyr-downstream-replay calibration
DOWNSTREAM_SUFFIX=final python3 \
  investigation/2026-10-01-downstream-integration/run.py \
  /private/tmp/marginplyr-downstream-replay main
python3 investigation/2026-10-01-downstream-integration/run.py \
  /private/tmp/marginplyr-downstream-replay load-order
R_LIBS_USER=/private/tmp/marginplyr-downstream-replay/library R_LIBS_SITE=NULL \
  Rscript --vanilla investigation/2026-10-01-downstream-integration/probe-calibration.R \
  /private/tmp/marginplyr-downstream-replay
```

一つの経路だけの再現には`DOWNSTREAM_BACKENDS=sqlite`、
`DOWNSTREAM_MODES=imports`、`DOWNSTREAM_CASES=W6-finite-typed`、
`DOWNSTREAM_SUFFIX=repro`を`run.py`に指定できる。起動スクリプトは個々の
assert失敗をケース記録へ保存するため、プロセスexit 0だけで合格としない。
`cases.csv`と保存した観測を確認する必要がある。

測定は逐次実行した。最初のharness失敗18プロセスを含む119測定プロセスと、
1個のdtplyr対照probeを実行し、120の上限内に収まった。最後の6プロセスは、
既に観測していたsummary評価回数3を明示的にassertした追加確認だった。
入力は最大3行、
主fixtureのGrouping setsは最大2個だった。各測定の120秒timeoutは発生しなかった。
再インストールはconsumerのみで、marginplyr tarballは変更しなかった。
隔離領域は約251 MiB、保存したテキスト証拠は約2.3 MiBだった。

以下は`NOT EXECUTED`または理由付き非適用として残した。

- 別OS、別R版、依存版マトリクス、PostgreSQLや外部DBサービス。
- Arrowへのshares、SQL/Arrow nesting。公開契約の非対応である。
- 任意のR関数のSQL翻訳、backendが通常対応しない式。
- live接続／Arrow pointerのRDS化、worker転送、接続破棄後の取得。
- 例外注入、中断・復旧、mutable dtplyr、stream reader。
- 大規模データ、同時実行、複数dimensionの網羅、重複Grouping setsの実際の
  multiplicity網羅。W7は同じ非重複rollupをnative/portableへ振り分けた検証だった。
- Contextual helperの別名化、foreign namespaceでの再export、computed call head。
  認識対象への追加を要求しなかった。
- `if_any()`、`if_all()`、`pick()`、`where()`全組合せ、全オプション、任意の
  属性やclass保持、byte単位のprint snapshot。今回のfixtureでは網羅しなかった。

## 保全確認

終了時にHEADとgit diffを確認し、開始時に複製した追跡済み556ファイルの
SHA-256がすべて一致した。複製した依存1477ファイルも通常ライブラリ側の
元ファイルと一致した。証拠は
[preservation.json](2026-10-01-downstream-integration/preservation.json)と
source/dependency hash表に残した。

リポジトリへの追加はこのノートと今回の新規ディレクトリだけだった。
製品コード、恒久tests、DESCRIPTION、NAMESPACE、生成文書、ADR、CI、
既存調査ノートの変更はなかった。commit、push、Issue・PR投稿も行わなかった。

## 読んだ契約・一次資料

- 公開RdのMargin verbs、Grouping constructors、Grouping helpers、shares、
  inspection、Sent query各説明、NAMESPACE、関連実装と既存tests。
- ADR 0007、0012、0016、0018、0019、0020、0026、0031、0033。
- #491の`216765a`、`dfa38d4`、#674の`4751c43`。
- [pkgload load_all](https://pkgload.r-lib.org/reference/load_all.html)：
  開発用ロードと通常インストールの違い。
- [R Packages: Dependencies in Practice](https://r-pkgs.org/dependencies-in-practice.html)：
  DESCRIPTIONとNAMESPACEの宣言の役割。
- [rlang quosures](https://rlang.r-lib.org/reference/topic-quosure.html)：式と環境の保持。
- [dtplyr lazy_dt](https://dtplyr.tidyverse.org/reference/lazy_dt.html)と、
  インストール済みdtplyr 1.3.3の`lazy_dt()`/`dt_eval()`実装。
- [data.table importing](https://rdatatable.gitlab.io/data.table/articles/datatable-importing.html)：
  `data.table in Imports but nothing imported`の対応宣言。

このノートは上記の日付の証拠を記録したもので、公開契約を追加・変更しない。
