# SQLite 型・順序契約と専用経路の履歴調査

Investigated: 2026-09-25
Base: `6a5f611d41bd462f3c933edea1dd0b9861d6e892`
Scope: 元の作業フォルダと GitHub は読み取りのみ。実測は `/private/tmp/marginplyr-sqlite-design-20260925` の HEAD archive とインメモリ SQLite を用いた。

## 読み取った契約

| 性質 | 保証・委譲の境界 | 根拠 |
|---|---|---|
| grouping values、行数、重複、public column 名と順 | Margin operation が構築する値・構造として保持する。一般的な downstream 操作で値が壊れてよいという例外はない | [公開 reference の Grouping set identifiers/Result class and attributes](https://github.com/sayuks/marginplyr/blob/6a5f611d41bd462f3c933edea1dd0b9861d6e892/R/summarize_with_margins.R)、[ADR 0016](https://github.com/sayuks/marginplyr/blob/6a5f611d41bd462f3c933edea1dd0b9861d6e892/design/adr/0016-delegate-result-class-and-attributes-to-dplyr.md) |
| `.id` 型 | package-created 列で、ゼロ行も integer | 同公開 reference の Grouping set identifiers |
| contextual share 型 | Parent/Total とも常に double。missing numerator/zero・missing denominator は missing double。SQLite の `.check_share_source = FALSE` は出力型の免除ではない | [share reference](https://github.com/sayuks/marginplyr/blob/6a5f611d41bd462f3c933edea1dd0b9861d6e892/R/share.R) |
| typed-missing dimension | live SQLite の直接結果と直接 materialization で source dimension の collected type を保つ | [ADR 0031 Decision](https://github.com/sayuks/marginplyr/blob/6a5f611d41bd462f3c933edea1dd0b9861d6e892/design/adr/0031-preserve-sqlite-typed-dimensions-under-margin-order.md) |
| Margin order | `.sort=first/last` で fixed key missingness/value、dimension bit/missingness/value、set-id tie break。直接 collect と直接 materialization に適用 | [ADR 0018 と amendments](https://github.com/sayuks/marginplyr/blob/6a5f611d41bd462f3c933edea1dd0b9861d6e892/design/adr/0018-order-margin-results-by-grouping-structure.md)、同公開 reference の Margin order、[CONTEXT Margin order](https://github.com/sayuks/marginplyr/blob/6a5f611d41bd462f3c933edea1dd0b9861d6e892/CONTEXT.md) |
| 有限 collect | ADR 0031 が通常の dbplyr finite-`n` behavior を保持すると明記。#654 はその互換性修正 | ADR 0031 Decision、[cbd745c](https://github.com/sayuks/marginplyr/commit/cbd745c9d376995bc2724b8535db60edad525d12) |
| compute 後の dplyr | ordinary dbplyr behavior。さらなる verb 後の元 Margin order は約束しない。しかし select/rename が内部 invariant エラーになるのは許容範囲ではない | ADR 0031 Decision の "A later dplyr verb"、ADR 0018 の ordering scope |
| result class/attributes | 基本は dplyr/vctrs。marginplyr が作らない任意 class/attribute や ordinary SQL aggregate 型の再構築を約束しない | ADR 0016、[empty driver control test](https://github.com/sayuks/marginplyr/blob/6a5f611d41bd462f3c933edea1dd0b9861d6e892/tests/testthat/test-sqlite-empty-declared-types.R) |
| lazy construction | caller に無断で入力を読む SQL は ADR 0020 の列挙例外のみ。#640 追加の staging/INSERT は explicit compute の内側に限る。ADR 0031 はさらに "No extra schema query" と明記 | [AGENTS Queries against a lazy input](https://github.com/sayuks/marginplyr/blob/6a5f611d41bd462f3c933edea1dd0b9861d6e892/AGENTS.md)、[ADR 0020 amendment #640](https://github.com/sayuks/marginplyr/blob/6a5f611d41bd462f3c933edea1dd0b9861d6e892/design/adr/0020-ask-before-reading-a-lazy-input.md)、ADR 0031 |

最後の行は B の比較で重要である。**実行時のゼロ行 metadata query に移す**案は、lazy construction の無断入力 read を維持できても、ADR 0031 の追加 schema query 禁止と同一ではない。必要な保証を維持する B として提案するなら、少なくとも追加クエリ費用とその文言変更は明示する。

## 仕組みと実際の責任

[sqlite-typed-order.R](https://github.com/sayuks/marginplyr/blob/6a5f611d41bd462f3c933edea1dd0b9861d6e892/R/sqlite-typed-order.R) は一つの公開 lazy object に public query、typed union、zero-row public anchor、order key names、declared types、original lazy query の対応を保持する。

- 直接 collect（sorted）は typed union に outer `ORDER BY` を加え、`db_collect()` 後に内部列を削る。finite `n` は通常 dbplyr の `head()` で検証し、自分で `LIMIT` を継ぎ足す。通常 dbplyr の `collect.tbl_sql` と二重に維持する互換面である。
- unsorted collect は public query の通常 collect を呼び、宣言型の復元だけを行う。ただし復元は `nrow(out)==0` に限られるため #666 の非空 all-NULL prefix に届かない。
- compute（sorted）は typed union を一時表へ CTAS し、public anchor を通常 compute で目的表へ CTAS、内部列を除いた ordered INSERT、任意の ANALYZE、stage cleanup、最後に raw SQL `rowid` arrange を行う。
- compute（unsorted）も目的表を空 anchor から作り、public query を INSERT する。#653 以降、単なる integer `.id` のためにもこの write orchestration が入る。
- downstream verb は `lazy_query` と original query の一致が失われると NextMethod() に戻る。ところが compute は ordinary tbl を返す際に raw SQL rowid の order metadata を載せ、通常 dbplyr の `rename_order()` が扱えない形を作っている。

これにより回帰箇所は、anchor の位置、declared-types 登録と適用、finite-limit parsing、destination identity、transaction ownership、stage lifetime、index/analyze sequencing、rowid availability、downstream query metadata の複数面に分散した。A で個別に正せる一方、型の保証だけが欲しいケースにも書き込みの責任が付いてくるのが B を検討する理由となる。

## 履歴: 専用処理はどこまで広がったか

| 修正 | 確認した変更 | 専用処理・回帰の広がり |
|---|---|---|
| #640 / PR #650 | [e5a14e8](https://github.com/sayuks/marginplyr/commit/e5a14e85bebe0dfc0a4612cebff2a520e60e3d96)、[c18b159](https://github.com/sayuks/marginplyr/commit/c18b159d8f01dcbc2428a931e900dd19f60e2d86) が dedicated class/collect/compute と ADR 0031 を導入 | summary は typed-missing dimension を持つ ordered case。expansion は ordered multi-set の全 `data_vars` を anchor にし、**text label でも**専用経路。#661/#662/#663 の sorted typed case はこの段階の compute 責任に由来する |
| #651 / PR #656 | [ba2e0cc](https://github.com/sayuks/marginplyr/commit/ba2e0cceb4add6cef6bc29be7b425b03e13091f8)、[463accd](https://github.com/sayuks/marginplyr/commit/463accd) が unsorted `.id` に final source anchor を広げる | この段階は unsorted anchor を追加するが、専用 typed-order class 自体の eligibility は sorted に限定 |
| #652 / PR #657 | [d5e9cce](https://github.com/sayuks/marginplyr/commit/d5e9cce) | dtplyr の Total share typed keys。SQLite compute の拡大原因ではない |
| #653 / PR #658 | [c224845](https://github.com/sayuks/marginplyr/commit/c224845e7b02d0e86b0c452ad303550c89a5f7d8)、[965f2a8](https://github.com/sayuks/marginplyr/commit/965f2a8d2286cfa52c2c3a2f2727aeae8881cafd) が `declared_types` を execution に追加、class 名を typed_result に変更、unsorted collect/compute を追加 | live SQLite summary の **share または `.id` あり**に、text label、`.sort=none`、one-set を含め専用 compute を拡大。#661/#662 は ordinary unsorted `.id` でも発現。expansion は `.id` 宣言の登録がなく #665 が残る |
| #654 / PR #659 | [cbd745c](https://github.com/sayuks/marginplyr/commit/cbd745c9d376995bc2724b8535db60edad525d12) | sorted collect の `n` 検証と SQL LIMIT。型復元はゼロ行限定のまま。#666 はこの修正が作った regression と断定できず、問題の組合せが残っている |
| #655 / PR #660 | [7338822](https://github.com/sayuks/marginplyr/commit/7338822a81d954c1b74becd243b3188bd618b391) | `render_sent_query_sql()` が `rlib_warning_verbosity=quiet` を一時設定し `warn=2` 時の本来 render の失敗を監査が消費しないよう修正。SQLite の対象拡大ではないが、代替 renderer も監査 on/off の warning/error 同等性を維持する必要 |

ADR 0031・公開 class explanation は依然 "typed-missing + Margin order" を例外と説明していた。#653 は実装の境界を広げたが、その ADR 本文に対応する改訂は含まれていない。どの案でも決定した対象を再記述する必要がある。

## 実行証拠: class scope と audit

[contract-probes.R](contract-probes.R) と [出力](contract-probes.txt) を保存した。R 4.6.1、dplyr 1.2.1、dbplyr 2.6.0、DBI 1.3.0、RSQLite 3.53.3、base archive を `pkgload::load_all()` で実行。2 行 fixture `g=c("a","b"), v=c(2,5)`、rollup(g)。以下の結果だった（TRUE は専用 class）。

| operation | label | none / no id | none / id | last / no id | last / id |
|---|---|---:|---:|---:|---:|
| summary | text | FALSE | TRUE | FALSE | TRUE |
| summary | typed | FALSE | TRUE | TRUE | TRUE |
| expand | text | FALSE | FALSE | TRUE | TRUE |
| expand | typed | FALSE | FALSE | TRUE | TRUE |

同 probe で `last_sent_queries()` は finite collect 後も compute 後も変わらず、構築時 `result` は full `sql_render(query)` に一致した。これは単独の不具合判定ではなく、既存監査の実際の境界を示す。

## SQL 監査との整合性

[公開 last_sent_queries](https://github.com/sayuks/marginplyr/blob/6a5f611d41bd462f3c933edea1dd0b9861d6e892/R/sent-queries.R) は、直近の Margin verb/inspect の構築中に記録する 1-call record である。`result` は返す未実行 query の render。これは全 DBI statement の実行ログとは書かれていない。一方 [CONTEXT Sent query](https://github.com/sayuks/marginplyr/blob/6a5f611d41bd462f3c933edea1dd0b9861d6e892/CONTEXT.md) の「executing the Margin operation」、[ADR 0027](https://github.com/sayuks/marginplyr/blob/6a5f611d41bd462f3c933edea1dd0b9861d6e892/design/adr/0027-record-the-sql-marginplyr-sends.md) の「every query marginplyr sends」、ADR 0031 の「SQL that actually runs」はより広く読める。

採択時に次を明記する案が妥当である。

1. `sql_render(q)` と構築時 `result` は direct full result の同じ SQL を示す。内部列を含む SQL を隠して別の SQL が走る形にしない。
2. finite collect の LIMIT、compute の DDL/INSERT/ANALYZE、実行時型照会を構築時ログが列挙しない場合、その境界を ADR 0027/公開 reference に明示する。
3. 既存 global record へ後から compute SQL を append しない。`q1` を作り、`q2` を作り、その後 `compute(q1)` したとき、q2 の記録へ誤帰属する。実行監査まで約束するなら query ごとの record identity と失敗時読出しを別途設計する必要があり、このレビューで「小さくした」ことにはならない。
4. audit の有無が値・DB I/O・warning（特に `warn=2`）・元エラーを変えない。#655 の fresh-process tests を再利用する。

## DBI/dbplyr/SQLite 一次資料から確認した境界

- SQLite `BEGIN` は既存 transaction/savepoint 内で失敗する。`SAVEPOINT` は内外どちらでも開始でき、`ROLLBACK TO` はその区間だけ戻し、`RELEASE` は外側 transaction があれば commit しない。したがって atomic multi-statement compute を残す A/B は **自分の savepoint だけを所有**する設計にできる。独立した `BEGIN` を無条件に重ねる現方式は避ける。[SQLite transactions](https://www.sqlite.org/lang_transaction.html)、[savepoints](https://www.sqlite.org/lang_savepoint.html)
- DBI generic は nested transaction を約束せず、RSQLite の named transaction を拡張として説明している。SQLite 以外にその方式を流用してよい根拠ではない。[DBI transactions](https://dbi.r-dbi.org/reference/transactions.html)
- CTAS は expression affinity から型を宣言し、出力順に連続 rowid を割り当てる。SQLite の special `rowid/oid/_rowid_` は同名の public 列に shadow される。型だけを保証する casts/anchor と、順序を読出し時に再指定する責任は別である。[SQLite CREATE TABLE](https://www.sqlite.org/lang_createtable.html)
- SQLite の `sqlite3_column_decltype()` は式や subquery では基底宣言を返さない。dynamic values と column declaration は別の情報であり、outer cast だけで RSQLite の all-NULL R type が直るとはいえない。[SQLite column_decltype](https://www.sqlite.org/c3ref/column_decltype.html)
- dbplyr `remote_table()` は引用済 table identifier の SQL を返す公開 API。`remote_name()` の返す末尾名で目的地を組み直す必要はない。[dbplyr remote metadata](https://dbplyr.tidyverse.org/reference/remote_name.html)。installed source の確認では `remote_name()` は `remote_table_path()`→`table_path_name()`、`remote_table()` は同 path の SQL を保持していた。
- installed dbplyr 2.6.0 の `compute.tbl_sql()` は name を table path へ正規化し、index を検証し、`db_compute()` に name/overwrite/index/analyze を委譲する。`db_compute.DBIConnection()` の `in_transaction` default は FALSE。TRUE は内部 transaction を求めるため、外側 savepoint を所有する wrapper では TRUE をそのまま内側に送ると nested BEGIN になる。prototype では境界で取り込み、内側 FALSE とする選択の互換性を確認する必要がある。ソースを [probe output](contract-probes.txt) に記録した。

## #661 の最小対応案と採択前の分離

安全上、#661 は別の設計が固まるまで放置しない。最小差分候補は compute 内の

```r
table_name <- quote(dbplyr::remote_name(result))
```

を

```r
table_name <- as.character(dbplyr::remote_table(result))
```

に置き換え、INSERT と ANALYZE の両方で同じ完全な引用済 destination を使うことである。これは root agent の試作担当へ渡した候補で、この note 単独では safety patch の実証済みを主張しない。両 schema に同名 compatible table、`in_schema`/`DBI::Id`、sorted/unsorted、analyze TRUE、overwrite/index failure を実測してから採用する。安全なパッチの確認を待つ間の保守的な利用停止策は、対象 release の dedicated SQLite result への direct compute 全体を一時停止して direct collect を使うこと。schema-qualified 名だけでなく unqualified persistent name も TEMP/main の同名衝突を考慮する必要がある。

#664 は `margin_structured_sort_columns()` が data.table に data.frame の one-index subset を適用する通常のバグである。SQLite の guarantee 選択で受け入れ条件を変更せず、別修正として扱う。入力保全・structured sort・plain data.frame/immutable dtplyr control を維持する。

## 比較時に落としてはいけない受け入れ条件

- #661 の destination/input preservation、#662 の caller-owned transaction preservation、#663 の通常 select/rename compatibility は C を選んでも縮小する理由にならない。型や Margin order の optional 上乗せと、誤書込み/正常 API の拒否は別問題である。
- #665 integer `.id` と #666 double share は package-created 型なので、ADR 0016 の任意属性を復元しない原則では外せない。C が削る対象にするなら public semantic change が必要で、専用 compute を減らすために直接 collect の既知型まで失う必然性はない。
- compute から型や order の上乗せを外す C は、それが public physical schema、direct computed collect の型、direct order のどれを失うか別々に記載する。普通の dbplyr に任せることは、atomic overwrite/index failure を現方式と同等に保つことを自動的には意味しない。
- A/B の downstream で特別な型/order を保ち続ける必要はない。通常 tbl を返し、direct materialization だけの order を metadata の raw-SQL expression ではなく直接 SQL 境界に載せるような代替は、この狭い境界を試験する価値がある。

## 未検証

この note の実行は class scope、audit record と installed dbplyr source の確認に限定した。#661〜#666 の再現、A/B/C prototype の成功/失敗、index failure の rollback、caller transaction の success/failure、rowid/collation/extended SQLite type の互換性は親 task の別の実行証拠へ委ねる。SQLite docs は通常エラー時にもディスク不足/IO error 等で transaction 全体が自動 rollback され得ると述べるため、SAVEPOINT がプロセス障害等でも caller work を必ず守るという拡大保証はしない。
