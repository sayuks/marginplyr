# DuckDB 2.0 互換性調査と検証計画

Investigated: 2026-10-28
Revised: 2026-10-28 — investigation/2026-10-28-duckdb-2-compatibility/README.md に実行結果と Arrow の戻り値の訂正を記録。
Status: 計画のみ。実装、依存パッケージのインストール、テスト実行は未実施。

## 調査の読み方

「確定」は公式資料または対象ソースから確認した事実、「影響候補」はその事実からの推測、「要検証」は後続の実行で判定する事項を表す。このノートは 2026-10-28 の調査記録であり、互換性合格の宣言ではない。

## 検証対象の版と配布状況

確定: core DuckDB v2.0.0 は 2026-10-21 に公開された LTS で、少なくとも 2027 年 10 月までのサポートが示されていた。新機能の全体像は Quack/CONNECT、非同期 I/O、VARIANT、triggers、PEG parser などである。[公式発表](https://duckdb.org/2026/10/21/announcing-duckdb-200)、[core v2.0.0 release](https://github.com/duckdb/duckdb/releases/tag/v2.0.0)

確定: R クライアントの `duckdb` v2.0.0 も公開済みだった。公式 GitHub API の fresh GET は `prerelease: false`、`published_at: 2026-10-25T05:50:04Z`、対象 commit `0f355370617dc3669fa1ca170753fd8b8ace5a41` を返した。タグの DESCRIPTION は `Version: 2.0.0`、NEWS は bundled engine の v2.0.0 への更新を記載した。[R v2.0.0 release](https://github.com/duckdb/duckdb-r/releases/tag/v2.0.0)、[release metadata](https://api.github.com/repos/duckdb/duckdb-r/releases/latest)、[tagged DESCRIPTION](https://github.com/duckdb/duckdb-r/blob/v2.0.0/DESCRIPTION)、[tagged NEWS](https://github.com/duckdb/duckdb-r/blob/v2.0.0/NEWS.md)

要検証: CRAN/macOS binary の v2.0.0 配布まではこの調査で確定しなかった。検索キャッシュでは CRAN mirror が 1.5.6、r-universe の package page が 2.0.0.9000、同 API が 1.5.6.9002 と不一致だった。これは配布不能の証拠ではなく、可変 URL や検索結果だけでは対象版を確定できないという観測である。検証対象は公式の stable tag `v2.0.0` に固定し、実行前にインストール元、R package version、`SELECT version()` の engine version、可能なら source commit を記録する。`duckdb.2.0.dev` や `2.0.0.9000` を正式版の代用にしない。[CRAN mirror](https://www.stat.ethz.ch/CRAN/web/packages/duckdb/index.html)、[r-universe page](https://duckdb.r-universe.dev/duckdb)、[r-universe metadata](https://duckdb.r-universe.dev/api/packages/duckdb)

R 自体の版、R package `duckdb` の版、core engine の版は別々に記録する。R bindings は engine を同梱するため、別に CLI 2.0 を入れても R 接続の engine が 2.0 になった証拠にはならない。[公式 README の Goals](https://github.com/duckdb/duckdb-r/blob/v2.0.0/README.md)

## Arrow の変更を API ごとに分ける

| 入口 | 公式資料で確認した事実 | marginplyr への影響候補・要検証 |
| --- | --- | --- |
| R の `duckdb_register_arrow()` / `duckdb_fetch_arrow()` / DBI Arrow API | v2.0.0 の NAMESPACE に export / method が残る。`duckdb_register_arrow()` は Arrow source をコピーせず view として登録する。 | R Arrow 連携全体が削除されたとはいえない。Arrow source を DuckDB の dbplyr table にした経路は通常の SQL backend として結果・型を検証する。[tagged NAMESPACE](https://github.com/duckdb/duckdb-r/blob/v2.0.0/NAMESPACE)、[R register docs](https://r.duckdb.org/reference/duckdb_register_arrow.html) |
| R `to_arrow_stream()` | 1.5.6 で追加された experimental API。`DBI::dbGetQueryArrow()` を `arrow::as_record_batch_reader()` に渡し、`RecordBatchReader`、grouped input ならその上の `arrow_dplyr_query` を返す。`arrow::to_arrow()` は先に全結果を materialize する別経路。 | 2.0 専用の追加ではない。reader の再読可能性を前提にせず、既存の one-shot reader 拒否契約との関係を確認する。[R stream docs](https://r.duckdb.org/reference/to_arrow_stream.html)、[tagged source](https://github.com/duckdb/duckdb-r/blob/v2.0.0/R/to_arrow_stream.R)、[R NEWS](https://github.com/duckdb/duckdb-r/blob/v2.0.0/NEWS.md) |
| R stream の寿命 | 未完了 stream の接続で別 statement を実行すると、次の read が invalidation error になる。reader を読む query を同じ接続に投げても失敗する。stream は一回だけ読める。第二接続は独立する。 | schema inspection や表示でも同一接続の statement が発生しうる。直接の stream 対応を追加する計画にはせず、受理・拒否と、その過程で読み込まないことを確認する。[tagged integration handbook: Arrow](https://github.com/duckdb/duckdb-r/blob/v2.0.0/handbook/usage/integrations/README.md#arrow) |
| R の Arrow 型・実行タイミング | `SELECT NULL` は Arrow format `n` の null array になり、以前の int32 ではなくなった。書込みを伴う `SELECT` は `dbSendQueryArrow()` が戻る前に完了する。Arrow stream の progress が最初の batch 後に止まる既知問題も掲載された。 | 型 prototype、zero-row schema、未型付け NULL を含む結果の重要な検証点。副作用付き SELECT と progress は使われる経路を先に調べて優先度を決める。[R v2.0.0 breaking changes / known issues](https://github.com/duckdb/duckdb-r/releases/tag/v2.0.0) |
| Python の Arrow 出力 | `execute()` の `fetch_arrow_table()` / `fetch_record_batch()` / `arrow()` / `pl()` が削除され、relation の `to_arrow_table()` / `to_arrow_reader()` が推奨された。 | Python API の変更を R の同名・類似 API に転用しない。R 実装への直接の削除根拠にはならない。[Python migration](https://duckdb.org/docs/2.0/clients/python/migration)、[Arrow export guide](https://duckdb.org/docs/2.0/guides/python/export_arrow) |
| C の旧 Arrow API | 2.0 の API reference に `duckdb_query_arrow()` 等は存在し、将来削除予定の deprecated API として記載された。 | 2.0 で既に削除されたという扱いは誤り。C API を直接呼ぶ場合にだけ、その呼出元の対応を別途確認する。[C API Arrow Interface](https://duckdb.org/docs/2.0/clients/c/api#arrow-interface) |
| Arrow IPC extension | community のページは IPC file/buffer の読書き用として `INSTALL nanoarrow FROM community; LOAD nanoarrow;` を案内する。 | IPC extension と R Arrow C stream interchange は別入口。ページだけでは「2.0 で R Arrow API が extension へ移動した」とは結論しない。[community extension](https://duckdb.org/community_extensions/extensions/arrow) |

## SQL・型・実行に関する変更候補

以下は公式 migration guide、機能別 reference、リリースに含まれた公式 PR を突き合わせた確認事項である。既存コードへの具体的な到達範囲とテストの対応は後段で整理する。

Migration guide は公式サイトに加え、公式リポジトリの本文を fresh GET で確認した。取得した本文の blob SHA は `0009bbf681debe76fe75541df81d937930492ebe` だった。[取得した公式本文](https://api.github.com/repos/duckdb/duckdb-web/git/blobs/0009bbf681debe76fe75541df81d937930492ebe)

| 項目 | 確定した変更・仕様 | 影響候補・要検証 |
| --- | --- | --- |
| 未型付け `NULL` | `SELECT NULL` は `INTEGER` ではなく `NULL` 型を保持する。CTAS のその列は non-NULL を受け付けない。 | omitted grouping key、empty input、全欠損、SQL prototype、UNION の common type、`compute()` 後の型を優先する。[migration: Untyped NULL Columns](https://duckdb.org/docs/2.0/operations_manual/migration#untyped-null-columns)、[R breaking changes](https://github.com/duckdb/duckdb-r/releases/tag/v2.0.0) |
| 除算 | `//` と `%` の zero denominator は NULL の代わりに error。通常の `/` は `inf` のまま。 | share の zero-denominator 契約、ユーザー式 `%/%` / `%%` の SQL translation と External condition を確認する。通常除算まで変更されたと扱わない。[migration: Division by Zero](https://duckdb.org/docs/2.0/operations_manual/migration#division-by-zero) |
| parser / user SQL | PEG parser が default。`~` / `!~` が partial regex、`!` の優先順位、`X` / `B` literals の型が変わり、single-arrow lambda は default で無効。 | marginplyr/dbplyr が生成する SQL と、caller supplied SQL を区別する。引用識別子、CTE/subquery/window、SQL literal・診断の snapshot を確認する。[migration: SQL Parser](https://duckdb.org/docs/2.0/operations_manual/migration#sql-parser)、[lambda docs](https://duckdb.org/docs/2.0/sql/functions/lambda) |
| identifiers | `preserve_identifier_case` が boolean から三値文字列へ変更。旧 true/false は互換 mapping がある。 | mixed case、空白、Unicode、予約語、private alias の quoting/返却名を確認する。C++ 内部の Identifier class 化を SQL 名の一律変更とみなさない。[migration: Identifier Case Setting](https://duckdb.org/docs/2.0/operations_manual/migration#identifier-case-setting) |
| native grouping optimizer | `ROLLUP` / `CUBE` / `GROUPING SETS` を CTE へ fold し cascading aggregation する最適化が入った。 | 同値性、重複 grouping sets、grand total、real NULL と subtotal の識別、empty input、floating-point aggregate の許容差を確認する。plan の形や速さから正しさを推定しない。[official PR #23287](https://github.com/duckdb/duckdb/pull/23287)、[2.0 grouping reference](https://duckdb.org/docs/2.0/sql/query_syntax/grouping_sets) |
| dbplyr windows | 2.0 は volatile value で order する window を拒否する。R backend は `slice_sample()` の random key を inner query に移す修正を含む。 | caller が渡す summary に volatile window がある場合、既存 helper rewriting や dbplyr translation と合わせて確認する。[tagged dbplyr handbook](https://github.com/duckdb/duckdb-r/blob/v2.0.0/handbook/usage/integrations/README.md#dbplyr)、[R release fix #2964](https://github.com/duckdb/duckdb-r/releases/tag/v2.0.0) |
| R 側の型変換 | `TIMESTAMP_TZ_NS` は session TimeZone の POSIXct、TUPLE は `element1` 等の列名を持つ data frame、empty STRUCT は 0-column data frame として読む。 | `where()` 等の型選択、summary metadata、Date/POSIXct/factor/decimal/nested type の zero-row schema を確認する。[R v2.0.0 features](https://github.com/duckdb/duckdb-r/releases/tag/v2.0.0) |
| 診断・DBI | EXPLAIN のレイアウトと operator 名、error LINE の underline が変化。`dbExistsTable()` は open/failed transaction の扱いを修正。 | 診断全文の snapshot 差と意味上の失敗を分ける。transaction、DBI metadata query、prepared statement の必要経路を確認する。[R v2.0.0 release](https://github.com/duckdb/duckdb-r/releases/tag/v2.0.0) |
| storage | 新規 DB は storage v69。2.0 より前は読めない。既存旧形式 DB を 2.0 が更新する場合は旧形式を維持する。 | in-memory 検証と file-backed 検証を区別し、共用 DB を migration fixture に使わない。[migration: Storage Format](https://duckdb.org/docs/2.0/operations_manual/migration#storage-format) |
| build / runtime | C++17 required、ICU dependency の除去と独自実装への変更、macOS x86_64 の段階的終了が発表された。 | R package の install/build と extension availability の条件として記録する。timezone/collation は実データに使う場合に追加確認する。[core announcement](https://duckdb.org/2026/10/21/announcing-duckdb-200#internals-and-deprecations) |

Quack/CONNECT、triggers、VARIANT、async I/O、data-modifying CTEs、nested schemas、C API v2 は追加機能として確認した。互換性調査だけを理由に採用・対応拡張は行わず、既存経路が触れる項目に検証を絞る。特に async I/O や optimizer の速度改善は正しさの代替証拠にはならない。[What's New in the Docs for 2.0](https://duckdb.org/docs/2.0/whats_new_in_the_docs)

## リポジトリとの照合（2026-10-28）

照合した marginplyr は `bd99ee5fa2c0351647c4a1469eaff8390481294b`。
専用ワークツリーは
`/Users/sayuks/.codex/worktrees/duckdb-2-compatibility/marginplyr` に作成した。
この調査ではソース、テスト、依存関係、CI、生成ドキュメントを変更せず、
DuckDB のインストールや互換性テストも行わなかった。

読み取りだけで確認したローカル環境は次のとおりだった。
これはインストール済み R パッケージの版であり、接続してエンジンの版を測定した結果ではない。

| 構成要素 | 版 |
| --- | --- |
| R | 4.6.1 |
| duckdb | 1.5.5 |
| arrow | 25.0.1 |
| DBI | 1.3.0 |
| dbplyr | 2.6.0 |
| dplyr | 1.2.1 |
| testthat | 3.3.2 |

[DESCRIPTION](../DESCRIPTION) は `duckdb (>= 1.5.5)`、
`arrow (>= 13.0.0)`、`dbplyr (>= 2.6.0)` を宣言していた。
[grouping_backend()](../R/grouping-backend.R) は DuckDB の SQL 入力と
Arrow 入力を別の backend kind に分類していた。
`R/`、`tests/`、`vignettes/`、`README.qmd` を検索した範囲では、
`to_arrow()`、`to_duckdb()`、`to_arrow_stream()`、`duckdb_register_arrow()`、
`duckdb_fetch_arrow()`、`duckdb_fetch_record_batch()` の直接呼び出しは見つからなかった。
したがって Arrow 連携変更の影響は、DBI/dbplyr の経路と、利用者が変換して渡す入力で調べる。
この検索結果だけでは互換性を判定できない。

既に取り込まれていた
[commit 26cfe5da96adf102ce8f2f3aedaf88e6f0611692](https://github.com/sayuks/marginplyr/commit/26cfe5da96adf102ce8f2f3aedaf88e6f0611692)
は DuckDB 2.0 を意識した二つのテスト修正を含んでいた。
`test-duckdb-storage.R` は設定一覧に頼らず `current_setting()` で保存先を読み、
`test-share-backends.R` はエラーの SQL 抜粋から完全な引用識別子だけを抽出していた。
この履歴は対応範囲の証拠であり、今回の対象 SHA と DuckDB 2.0.0 の全体検証の証拠ではない。

### 影響範囲と優先順位

以下の優先順位は静的照合からの評価で、障害を再現した結果ではない。

| 優先度 | 接点・変更候補 | 対応するコード | 検証する契約と既存テスト |
| --- | --- | --- | --- |
| P0 | `NULL` 型、空結果、`UNION ALL` の共通型 | [backend-metadata.R](../R/backend-metadata.R) の `margin_column_info()`、[grouping-adapter-union.R](../R/grouping-adapter-union.R)、[margin-operation.R](../R/margin-operation.R) | DuckDB 向け省略次元の `dbplyr::sql("NULL")`、型付き欠損、空入力・空結果・全欠損、`.id`・Grouping helpers・share の型を、直接 `collect()`、有限件数 collection、`compute()` 後で比較。`test-duckdb-scalar-missing.R`、`test-grouping-backends.R`、`test-sqlite-grouping-helper-types.R` の DuckDB ケース。 |
| P0 | PEG parser とクエリ最適化 | [grouping-adapter-native.R](../R/grouping-adapter-native.R)、[grouping-context.R](../R/grouping-context.R)、[grouping-adapter-union.R](../R/grouping-adapter-union.R) | native `GROUPING SETS` と portable `UNION ALL`、重複 grouping sets、固定 `.by`、Grand total set、Grouping bit / identifier の値と行数。`test-grouping-backends.R`、`test-branch-union.R`、`test-metamorphic-margin-semantics.R`。 |
| P0 | R/SQL 型変換、ENUM と factor 復元 | [factor.R](../R/factor.R) の `reconstruct_factor.tbl_duckdb_connection()`、[backend-metadata.R](../R/backend-metadata.R) | 未使用 level、欠損、Margin label、level 順の Margin order、`TIME`・`INTERVAL`・`BLOB`、日時・小数・integer64 を確認。`test-factor.R`、`test-margin-order.R`、`test-duckdb-scalar-missing.R`。新しい `NULL`・`TIMESTAMP_TZ_NS`・`TUPLE`・空 `STRUCT` は通常 DBI/dbplyr の対照例が扱える範囲で追加調査。 |
| P0 | window 集計、型拒否、share の生成 | [share.R](../R/share.R) | Parent share / Total share、numeric 最大値、ゼロ・欠損の分母、文字列・logical source の拒否、native / portable / local の同値性。`test-share-backends.R`、`test-sql-share-fixed-keys.R`、`test-share-visible-denominators.R`。 |
| P0 | zero-row メタデータ取得、SQL 記録、診断 | [backend-metadata.R](../R/backend-metadata.R)、[sent-queries.R](../R/sent-queries.R)、[conditions.R](../R/conditions.R) | schema snapshot の取得回数、利用者データを読まない構築、audit on/off の結果・condition の同値性、詳細状態とエラーの帰属。`test-query-policy.R`、`test-sent-query-details.R`、`test-sent-query-views.R`、`test-share-backends.R`。SQL の見た目だけを理由に snapshot を更新しない。 |
| P1 | class dispatch、識別子と materialization | [grouping-backend.R](../R/grouping-backend.R)、[duckdb-env-expansion.R](../R/duckdb-env-expansion.R)、[margin-operation.R](../R/margin-operation.R) | `duckdb_connection` / dialect / lazy table class、`.env`、大文字小文字の等価名、引用名、LIMIT 付き入力、`compute()` 後の公開列・型・Margin order。`test-sql-env-keys.R`、`test-sql-env-expansion.R`、`test-sql-identifier-equivalence.R`、`test-sql-expansion-input-limits.R`。 |
| P1 | 保存先、接続キャッシュ、旧 DB ファイル | [helper-duckdb.R](../tests/testthat/helper-duckdb.R)、[test-duckdb-storage.R](../tests/testthat/test-duckdb-storage.R) | `shared_home = FALSE` の保存先、終了処理、設定と既存 instance の衝突。ファイル互換性はコピーだけで旧版→2.0 と 2.0 作成→旧版を調べ、想定された非互換と marginplyr の障害を区別。 |
| P1 | Arrow との受け渡し | [grouping-backend.R](../R/grouping-backend.R)、[utils.R](../R/utils.R)、[backend-metadata.R](../R/backend-metadata.R) | 直接呼び出しは見つからなかった。利用者が変換した Table / Dataset / reader / reader を含む query の受理・拒否、schema と reader の寿命を別の結合検証で確認。通常の Arrow テスト群も単独で実行。 |

### 設計上の判定基準

- [ADR 0002](../design/adr/0002-acquire-typed-metadata-once.md) の schema snapshot と、
  [ADR 0020](../design/adr/0020-ask-before-reading-a-lazy-input.md) の構築時の読取制約を維持する。
- [ADR 0016](../design/adr/0016-delegate-result-class-and-attributes-to-dplyr.md) と
  [ADR 0033](../design/adr/0033-preserve-declared-types-in-empty-margin-results.md) に従い、
  upstream 由来の表現変更と marginplyr が所有する型・値の保証を区別する。
- [ADR 0018](../design/adr/0018-order-margin-results-by-grouping-structure.md) の Margin order、
  [ADR 0032](../design/adr/0032-respect-sql-identifier-equivalence.md) の識別子等価性、
  [ADR 0037](../design/adr/0037-keep-sql-diagnostics-local-to-marginplyr.md) の SQL 診断範囲を使う。
  legacy parser や互換設定を一律に有効化して合格とはしない。

## 実行段階の検証計画（未実施）

### 1. 対象を固定し、環境を分離する — P0

専用ワークツリーの同じ marginplyr SHA と同じ R・DBI・dbplyr・dplyr・Arrow を使い、
`duckdb 1.5.5`、`1.5.6`、正式 `2.0.0` を比較する。
1.5.5 は読み取ったローカル環境と DESCRIPTION の下限、1.5.6 は直前版の橋渡し対照とする。
これにより 1.5.6 で入った Arrow・日時・接続変更を 2.0 固有の変更と誤認しない。

版ごとに専用 R library と新しい R process を使う。
実行時に配布元、取得日時、tag/SHA または配布物の識別情報、`packageVersion("duckdb")`、
`SELECT version()`、読み込んだ namespace の実パス、全依存の版、OS/architecture を保存する。
「latest」や開発 flavor だけを根拠に正式 2.0.0 の検証とはしない。
正式版 binary が取得できなければ公式 tag からの source build を選び、
C++17 toolchain と実際の取得・build 成否を記録する。
利用者の既存 library や DB ファイルを更新する必要はない。

### 2. 既存 suite を変更せず比較し、失敗を切り分ける — P0

まず既存 suite の実行結果を各版で保存する。
`NOT_CRAN=true` と `MARGINPLYR_REQUIRED_SUGGESTS=duckdb,DBI` を指定し、
DuckDB の未導入・版不足・意図しない skip を成功として扱わない。
DuckDB 単独の library 構成と Arrow 単独の構成は
[release-matrix.yaml](../.github/workflows/release-matrix.yaml) と
[optional_suggest_spec()](../tests/testthat/helper-optional-backends.R) に合わせる。
tarball 検証では既存の library isolation / backend verifier の判定も利用する。

前表の既存テストを優先して差分を読み、最後まで全 suite の結果を確定する。
失敗ごとに通常の DBI/dbplyr/Arrow の最小対照例を用意し、
marginplyr 固有の回帰、上流の変更・不具合、テスト期待値の変更、環境不足を分類する。
既存の失敗も別記し、2.0 の影響と混ぜない。
診断の class と安定した意味を比較し、位置表示や SQL 抜粋の差だけでは動作不良と判定しない。

### 3. 公式変更点に対応する不足ケースを調べる — P0

既存の網羅範囲と実行差分を確認した後、欠けたケースだけを検証する。
各ケースには単純な DBI/dbplyr の対照と、期待値を独立に計算できる小さな入力を置く。

1. `SELECT NULL`、明示 cast の `NULL`、型付き列との union、空入力、
   `.margin_label = NULL` / `NA_character_` / 文字列を比較する。
   omitted dimension の SQL 型、`.id`、直接 Grouping helpers、share の型を
   `collect()` / `head(..., 0)` / `compute()` 後で確認する。
2. grouping sets / rollup / cube / 複合次元 / 重複 set / 固定 `.by` を、
   native と portable の両経路で比較する。
   `summarize_with_margins()`、`expand_with_margins()`、`inspect_grouping()` と
   DuckDB 入力の nesting verbs の対応範囲を含める。
3. ENUM、日時と timezone、精度境界、`TIME`・`INTERVAL`・`BLOB`、
   `NULL`・`TIMESTAMP_TZ_NS`・`TUPLE`・空 `STRUCT` の R/SQL schema を確認する。
   上流の通常入力が扱えないものまで marginplyr の新機能に広げない。
4. parser の変更に対して生成 SQL と利用者の summary/SQL 入力を確認する。
   正規表現演算子、旧 lambda、`X'…'` / `B'…'`、`preserve_identifier_case` は、
   marginplyr の生成有無と利用者入力由来を区別して最小対照で判断する。
   `//` / `%` のゼロ除算と R の `%/%` / `%%` の translation も確認し、
   share が使う通常の `/` と混同しない。
5. audit on/off、成功・拒否・エラーを比較し、
   query 数、schema snapshot、zero-row read、condition と公開結果の契約を維持しているか確認する。

この段階で再現した障害は最小再現例と根拠を保存し、修正案を別に提示する。
互換性確認のためという理由だけでコード・snapshot・依存下限を更新しない。

### 4. Arrow 連携を結合検証する — P1

通常 suite とは別の調査 harness で次を比較する。
[AGENTS.md](../AGENTS.md) の Release matrix 方針に従い、
duckdb と arrow の両方を必須とするテストを通常の単独 backend suite に追加しない。

- Arrow Table / Dataset → DuckDB 登録 → lazy SQL table → Margin operation → collection。
  factor/dictionary、欠損、空 batch、日時と list/struct の対応範囲、filter/projection を確認する。
- DuckDB Margin result → `arrow::to_arrow()` と `duckdb::to_arrow_stream()`。
  materialized Table と stream の schema・値・空結果を比較する。
  stream は最初の一 batch だけで成功とせず読み切り、複数 batch と途中終了も含める。
- stream を使い切る前の同一 connection の再利用・無効化・close と、
  別 connection に分けた対照を、timeout を設けた独立 R process で検証する。
  upstream が規定する invalidation error と marginplyr 固有の停止・データ損失を区別する。
- reader を Margin verb へ直接渡す場合と、grouped query / join / union に埋めた場合は、
  既存の one-shot reader 拒否が読取前に働くことを確認する。
  `arrow::as_arrow_table()` で明示的に materialize した入力は、
  再利用可能な Arrow 入力として期待値を比較する。

`dbSendQueryArrow()` と `nextval()` 等の副作用に関する 2.0 の変更は、
変換層の対照で別記する。計画中の調査を、Margin verb が副作用付き SQL を安全に実行すると
保証する機能追加にはしない。

### 5. パッケージ全体と利用者向け経路を確認して報告する — P1

対象環境で source tarball の `R CMD check --as-cran`、example と vignette を確認する。
サイト確認が必要な変更・差分があれば、対象版の library に作業ツリーを install してから
`altdoc::render_docs(parallel = FALSE, freeze = FALSE)` と
`Rscript .github/scripts/verify-site.R` を使う。
優先実行環境は調査時の macOS / R 4.6.1 とし、
最終的な対象環境には Linux・Windows の R release と、
DuckDB の `R (>= 4.2.0)` 制約を満たす下限側の構成を含める。
取得できない binary や未実行の環境は、未検証として明記する。

不具合修正・テスト追加・依存変更など Package-affecting な変更を行う段階になった場合は、
[local-checks.md](../design/agents/local-checks.md) に従い、確定した clean committed HEAD に
`Rscript tools/review-ready-check.R` を一度実行して正式な完了結果を保存する。
これは互換性比較の各版でのテストや release matrix の代わりにはならない。
この計画だけの Repository-only な文書追加には package check を実行しない。

最終報告は、変更点とコード接点の対応表、実際に使った client/engine/dependency の版、
テストの pass/fail/skip、最小再現例、既知の上流制約、未検証の環境を含める。
「修正不要で動作」「marginplyr の修正が必要」「上流対応待ち」「未検証」を区別し、
将来の CI 固定版 job や依存下限変更は実測した必要性から提案する。

## 完了判定（計画）

対象の正式 2.0.0 で必須 DuckDB ケースが skip せず、値・行数・公開列・
marginplyr 所有の型・Margin order・遅延構築・SQL 記録の契約が満たされ、
Arrow 受け渡しの対応範囲と既存 reader 拒否が確認されたとき、
その client/engine/依存関係/OS の組合せに限って互換性確認済みとする。
パッケージが従来対応していなかった新機能への対応は、この判定の前提にしない。
2026-10-28 の計画作成時点では、この互換性判定は未実施だった。


## Revisions (2026-10-28)

[実行結果](2026-10-28-duckdb-2-compatibility/README.md) は、最新 main の
`cb957d9be09440796defde808bf6f04006edd6dd` を取り込んだ後の比較を記録した。
CRAN の正式 2.0.0 バイナリを確認でき、1.5.5／1.5.6／2.0.0 の比較と
正式 2.0.0 での全体チェックを行った。Linux・Windows 等の未実行環境は同報告に明記した。

Arrow 25.0.1 の `arrow::to_arrow()` は `RecordBatchReader` を返した。
本ノートの「materialized Table」との対比は戻り値を取り違えていた。
DuckDB 内部の結果の materialization と、利用者が `arrow::as_arrow_table()` で
再利用可能な Table を作ることを分けて検証した。元の記録は上に残す。
