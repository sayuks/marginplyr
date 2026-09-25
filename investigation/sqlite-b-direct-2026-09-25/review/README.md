# SQLite 型・順序保証と専用実装の設計レビュー

Investigated: 2026-09-25
Base: `6a5f611d41bd462f3c933edea1dd0b9861d6e892`
Status: 設計判断用の隔離試作。採択・本実装・リリースは行っていない。

**推奨は B：型付きの空の保存先へ、整列済み公開 SQL を直接 INSERT する方式（B-direct）。** 直接 collect の型アンカーは残すが、compute の中間表をなくす。今回必要な型・順序保証を縮小せず、結果を明示的に書き出す表を sorted compute で二つから一つにできた。型の保持に必要な操作と、直接収集のための SQL 構造を分けることが削減の根拠である。

ただし、Issue の再現だけを直すことは安全な完了ではなかった。名前が省略修飾されていると、通常 dbplyr 自体にも別 schema の表を INDEX/DROP の対象にする場合があった。B-direct は保存先の完全識別子を維持し、危険な同名衝突を**変更前に拒否する**。この拒否と compute 時だけの宛先 metadata 照会は既存 ADR に明示的な変更が必要であり、「何も変わらない B」とは扱わない。結果の型・順序・公開列の保証を残す B として推奨する。

#661 は設計決定を待たず、専用 direct compute の一時停止を先行する案を提示する。[停止patch](A/661-disable-direct-compute.patch)と8ケースの[安全確認](A/safety.log)を隔離領域に置いた。元の作業フォルダと GitHub の Issue・PR は変更していない。

## 1. 維持する保証と、通常 dbplyr に委ねる範囲

| 分類 | 判断 |
|---|---|
| 入力と無関係な表の保全、指定した保存先、caller transaction の所有権 | 必須。Cでも縮小対象にしない |
| grouping values・行数・重複・公開列名と順番 | 維持。実表へ内部 order 列を漏らさない |
| package-created `.id` の integer、Parent/Total/across share の double | 空、全欠損の非空 prefix、通常値のすべてで維持。型が既知なので通常集約式の推測とは分ける |
| typed-missing input dimension の character/integer/double | Margin result の直接 collect、有限 collect、直接 materialization で維持。Bで維持できたためCの縮小を選ばない |
| Margin order | first/last、固定キーを含むmissing-last、Grouping bit、必要なset-id tie breakを維持。直接 materialization の直接読出しにも適用 |
| compute後のselect/rename/filter/mutate/arrange | 通常の公開操作として成功させる。その操作後まで元 Margin order を保証しない |
| 任意の通常 SQL 集約・ユーザー式・下流変換のR型、任意属性 | 既存どおりdriver/dbplyrへ委譲。ゼロ行の通常集約までdoubleへ変換しない |
| sourceの型を新たに調べるquery、lazy構築での実行 | 追加しない。既存のADR0020の例外は変更しない |
| 宛先照会 | B-directでは明示compute時に限り同名のtemp/main tableを確認する。ADR0031の追加schema query禁止の文言はその例外を定める必要がある |
| rowid名が三つとも公開列にあるsorted compute | 既存の変更前拒否を維持。直接collectは可能。無理に新しい永続順序列を加えない |

根拠は公開 `summarize_with_margins` / `share` / `last_sent_queries`、適用AGENTS、CONTEXT、ADR0016/0018/0020/0027/0031。文書の箇所、SHA固定のソース参照、一次資料は[契約・履歴調査](contracts-history.md)に整理した。

## 2. なぜ専用実装が広がり、どこに費用が生じたか

ADR0031は、型アンカーを最外のUNIONに保つ直接収集と、型付きの実表を作るmaterializationを一つの専用classで扱っていた。直接収集ではSQLiteのcompound ORDER BYの制約から内部キーを投影し、Rへ返す時に隠す。この仕組み自体には型保持の根拠がある。

computeは同じcompanion queryをいったん一時表に書き、その後、型付き空表へ公開列を再INSERTしていた。しかし、**保存先に型宣言が既にあれば、そのINSERT元は公開列だけの整列済みqueryでよい**。直接collectで必要な「型アンカーを最外に残す」という制約を、INSERTの入力にも持ち込む必要はなかった。B-directの正常系・再現例はこの点を確かめた。

履歴の区別も重要だった。

- #640 / #650：専用class/collect/computeを導入。sorted typed summaryに加え、sorted expansionはtext labelでも専用対象だった。#661〜#663のsorted経路はここからの回帰。
- #651 / #656：unsorted `.id` のfinal source anchorを拡張。この段階では専用classの対象はまだsorted。
- #653 / #658：宣言型修復のため、shareまたは`.id`を持つSQLite summaryのunsorted、text label、one-setまで専用computeに拡張。#661/#662もその範囲へ広がった。ADR0031本文の対象説明は未追従。
- #654 / #659：有限nの検証とLIMITを修正。非空all-NULL shareは修復対象外のまま。
- #655 / #660：SQL監査によるwarning消費を修正。専用computeの拡大原因ではないが、方式変更で維持すべき正常系。
- #664/#665/#666は直近修正が新たに作った回帰とは確認されていない。#664は#640以前からの別問題。

実測したclass適用範囲と各commitは[履歴表](contracts-history.md)を参照。

## 3. A/B/Cを利用境界ごとに比較

| 境界 | A：現行機構を修正 | B-direct：推奨 | C：computeの上乗せを限定 |
|---|---|---|---|
| 直接collect | typed outer UNION、内部order列の除去を保持。宣言型修復を全欠損にも適用 | Aと同じ。ここは削除すると既存型保証を失う | この試作ではAと同じ保証を残した |
| 有限collect | 現行LIMIT実装を維持し、#666を修正 | Aと同じ。n検証/警告互換面は残る | Aと同じ。computeの縮小からfiniteの型縮小は導かない |
| compute | sortedではcompound stage→typed targetへ2回書出し。共有修正で5Issueは通る | zero-row anchorでtyped target作成→ordered public SELECTを1回INSERT。stage/その命名/cleanup/内部key投影をcomputeから除去 | 通常dbplyr CTASへ委譲。小さなsavepointと直接collect型修復wrapperは残す。sourceの全欠損型とmaterialization orderを失う |
| compute後の通常dplyr | raw rowid ORDER BYのwindow metadataを消せばselect/rename成功 | ordinary tblを返し、rowid ORDER BYは保持、window_orderは空。select/rename等成功 | 普通の操作は成功。型修復wrapperもquery変更後は解除。物理schemaや再接続読出しは型修復対象外 |
| 保存先/transaction | 元方式の所有責任をすべて残す。A試作はtemp/main衝突をまだ誤書込みする | 完全識別子、危険なbare衝突の事前拒否、所有savepointのみrollback/release | 委譲だけではtemp/mainのDROP/INDEX問題もatomicityも解消しない。安全に採用するならBと同等の入口保護が必要 |
| 保守・互換性 | 既存に近いが二つの表の寿命と内部key→公開列の対応が残る | 一つのtargetと一つのsavepointへ縮小。型情報・有限collect・rowid・identifier互換性の責任は残る | custom SQLは最も減るが、型とorderの文書/API互換性を破る。今回Bで保持できた保証を手放す利点が小さい |

C試作は「ただclassを外す」だけにはしていない。package-created型とatomicityを残したうえで、computeを通常dbplyrへ委譲した。それでもall-NULL character/integer/doubleはlogicalになり、`PRAGMA reverse_unordered_selects=ON`で直接materializationの順序が逆転した。[実測CSV](C/cross-check.csv)、[physical schema](C/physical-schema.log)、[試作patch](C/prototype.patch)。見た目の順序が一度合っただけでは、ORDER BYの保証を保ったと判定していない。

Bの補助案として、**typed temporary tableを先に作り、最終保存をdbplyr computeへ委ねるB-stage**も実行した。型・順序・Issue例を維持し、入力と保存先が同じoverwriteまで成功する利点があった。ただしsortedの2回書出しが残り、unsortedも1回から2回へ増える。stageの後始末も残る。B-directで現行保証を維持できたため、今回は採らない。[B詳細](B/report.md)

## 4. 実行証拠

環境はR 4.6.1、dplyr 1.2.1、dbplyr 2.6.0、DBI 1.3.0、RSQLite/SQLite 3.53.3。HEADのarchiveを隔離コピーし、各方式を別R processでロードした。DBは合成データのインメモリ、caller commit確認だけ隔離した使い捨てSQLiteファイル。既存ユーザーDBは使用していない。

共通98ケースは[同一スクリプト](cross-check.R)で実行した。#661両identifier表記×sort、#662 transaction有無×flag、#663五つの下流verb、#665空expansion、#666 Parent/Total×sum/mean×0/1/2/Inf、scalar source型、意図した順序、atomic overwrite/index failure、finite引数、普通dbplyr controlを含む。各fixtureの入力保全も照合した。

| 方式 | 共通ケース | 結果の読み方 |
|---|---:|---|
| 無修正HEAD | 62/98 | 今回のSQLite問題を再現。既存の成功ケースも比較対象にした |
| A | 98/98 | Issue例は通る。ただし共通matrix外のtemp/main同名に誤書込みが残るため、安全完成とは判定しない |
| B-stage | 98/98 | 型とorderを維持。copy/stage費用が残る |
| B-direct | 98/98 | 型とorderを維持。追加の宛先保護は別のsafety matrixで確認 |
| C | 90/98 | compute後のsource all-NULL型6ケースと直接order2ケースを失う。FAILを仕様変更として隠していない |

[baseline](baseline/cross-check.csv) / [A](A/cross-check.csv) / [B-stage](B/cross-check.csv) / [B-direct](B-direct/cross-check.csv) / [C](C/cross-check.csv)

追加の証拠：

- A：独立した再現matrix69/69、同一scriptの無修正HEADは17/69。既存SQLite21テスト・550 assertions成功。[A報告](A/README.md)
- B-direct：既存SQLiteとMargin orderの計64テスト・990 assertions成功、失敗/エラー/skipなし。[結果](B-direct/regressions.log)
- B-stage/B-direct：各77/77 public cases、各27/27 destination safety cases。default `.id`のみ、全sort/三つのlabel、Parent/Total/across、sum/mean、missing input/zero denominator、0行/全欠損/混在/無制限、qualified overwrite/analyze、失敗後の再実行を実行。[B詳細とログ](B/report.md)
- #655：B-stageとB-directそれぞれ16 fresh R processes・40照合成功。audit on/offの`warn=2`による同一失敗、明示`na.rm=TRUE`の正常値、設定復元を確認。[独立報告](independent/README.md)
- caller commit：B-directの結果とcaller markerをcommitし、別接続から残存を確認。rollbackだけの確認ではない。[結果](B-direct/commit-check.log)
- #664：28照合。ここでの成功数には「既知のdata.table拒否を再現した」ことを含む。修正成功数ではない。data.frame/immutable dtplyr正常系とsource serialization不変を確認。[独立報告](independent/README.md)

試験作成中、data.frame/tibble class差、通常集約の0行型、`reverse_unordered_selects`を戻す前のsource比較を誤って保証差として判定した箇所は修正して最終結果を再取得した。通常集約への新しい型保証や、unordered readの物理順序保証を紛れ込ませていない。`collect(n=1.9)`は現行marginplyrの既存成功を維持したが、この依存versionの普通dbplyrはDBIのwhole-number検証で拒否したため、両者が全引数で同じという説明は採らない。

## 5. 保存先、安全性、transaction、失敗処理

#661の原例では`other.report`作成後に`remote_name()`でschemaを捨て、mainの同名表へINSERTしていた。`remote_table()`への一行置換は原例を直したが、次の反例が残った。

1. **temp.reportあり、bare reportへtemporary=FALSE**：Aはmain.reportを空で作り、temp.reportへ結果を追加した。普通dbplyrも返すtblがtempを指し、indexをtemp側へ作った。
2. **main.reportあり、temp.reportなし、bare reportへtemporary=TRUE / overwrite=TRUE**：普通dbplyrはmain.reportをDROPしてからtemp.reportを作った。

したがって保存先の責任はINSERTだけではない。CREATE、DROP/overwrite、INDEX、INSERT、ANALYZE、返却tblを同一の識別子として扱う。

B-directの試作は、dbplyrの公開`as_table_path()` / `table_path_components()`で名前を正規化する。明示schemaはそのまま保持し、bareの後続SQL/返却はmainまたはtempを明示する。上の危険なbare組合せは照会して変更前に拒否する。文字列だけでなくtable-only `DBI::Id()`、`ident()`、`I()`、literal dot名を含む表記差も試験した。これは「別schemaへ書いてよい」という既存挙動を保持するための例外ではなく、安全性を確保する拒否である。利用者の回避策は衝突しない保存先名、または対応する明示schema名を使うこと。

attached databaseにだけ同名表があるbare overwriteも、temporary TRUE/FALSEで独立に実行し、既存の別schema sentinelを保全できた（[追加確認](B-direct/attached-overwrite.log)）。このdriverではbare名への`dbExistsTable()`がFALSEを返し、上流のDROPは実行されなかった。任意のschema構成全体へ一般化してはいない。

qualified indexはdbplyr 2.6.0がSQLiteで拒否されるSQLを生成する上流制約を残す。marginplyrが全index実装を所有する案は今回採らない。**普通のbare名でのindex成功を維持し、qualified index失敗ではsavepointで既存target/source/caller作業を保全する**。この制約を「qualified destinationは全面非対応」と取り違えない。indexなしの`in_schema()`/`DBI::Id()`、overwrite、analyzeは成功した。

A/Bともmulti-statement区間は名前付きSAVEPOINTを所有する。外側transactionがあるときのRELEASEはcallerのcommitではない。失敗は自分のsavepointへ戻し、callerが先に書いたmarkerは残す。外側がない場合は成功RELEASEまでatomicとする。`in_transaction=TRUE/FALSE`を内側computeへそのまま渡さず、nested BEGINを避ける。この契約は「FALSEなら一切transaction命令を送らない」という意味とは異なるため、#662の文言を明確にする必要がある。

B-directは明示stageを作らないのでstage cleanup自体を必要としない。target作成後のINSERT/index失敗はrollbackで既存targetを復元する。入力自身をoverwriteするケースは、無修正方式/Aと同じくanchorが参照できず拒否され、入力はrollbackで保全された。B-stageでは成功したが、その追加成功を既存保証とみなしてB-directの失点にはしない。

SAVEPOINTの根拠は[SQLite公式](https://www.sqlite.org/lang_savepoint.html)、CTASとrowidの根拠は[CREATE TABLE](https://www.sqlite.org/lang_createtable.html)。接続切断・ディスク不足等でSQLiteがtransaction全体をrollbackする場合までcaller作業を守るとは約束しない。

## 6. SQL監査とクエリ費用

[実送信ledger](B-direct/runtime-ledger.log)は、監査ONのquery構築で送信なし、direct/finiteでそれぞれ対応するSELECT、computeで宛先metadata・SAVEPOINT・zero-row CTAS・一回のINSERT・ANALYZE・RELEASEを確認した。[無修正比較](baseline/runtime-ledger.log)ではsorted computeに結果stageのCTASとDROPがあった。B-directは**明示stageとその全行コピーを一つ除去**する。ソートそのものやSQLite内部のtemporary B-treeが消えるとは主張しない。時間・最大メモリ・実I/Oのbenchmarkは行っていない。

`DBI::dbExistsTable()`はAPIを一度呼んでもSQL一文とは限らない。今回persistent bare名の確認では、database-list照会二つとtemp.sqlite_master照会一つが記録された。temp overwriteの保護は状況によりDBI照会が二回になる。これは追加の**保存先metadata**費用であり、source型を調べる新しいreadではないが、ADR0031の「追加schema queryなし」を無変更のまま満たしたとは主張しない。

`last_sent_queries()`は直近Margin verb/inspectの構築時記録だった。direct full resultの`sql_render(q)`と`purpose=result`は一致する。finiteのLIMIT、computeのDDL/INSERT/ANALYZEまで列挙する実行ログではなく、試作後もcomputeで記録を追記しない。後からglobal recordへ追加すると、q2を構築後にcompute(q1)したときq2へ誤帰属するためである。

ADR0027/CONTEXTの「every query marginplyr sends」と読める広い文言は、公開referenceの構築時境界へ揃える案を提案する。もし全実行SQL監査を要件にするなら、queryごとの記録identityと失敗時読出しを別設計する必要がある。**本試作がその完全監査を実現したとは扱わない。** B-directのcomputeがpublic queryをINSERTし、collectがtyped companionを実行する違いも明示する。

## 7. 各Issueの処理と受け入れ条件

| Issue | 決定可能な処理 | 受け入れ条件 |
|---|---|---|
| #661 | 最優先で一時停止策。その後B-directの宛先正規化・保護と共に解除 | 既存条件を維持。temp/main双方の同名衝突、table-only Id等、DROP/INDEX/返却先、変更前の安全拒否を追加。原例二つだけ通っても閉じない |
| #662 | 共通SQLite materializerのSAVEPOINT境界として実装 | atomicity/caller所有/input/cleanup/再試行は維持。flag TRUE/FALSEの意味を「outer transactionを所有せず、内側BEGINを重ねず、atomic savepointは使う」と明確化。無効値は本実装で公開条件に沿って検証 |
| #663 | rowid ORDER BY後のwindow_orderを空にする | 既存条件を維持。後続verbの成功を直し、新たな下流order保証は作らない |
| #664 | SQLite設計と独立した通常バグ修正として別扱い | 全条件維持。data.tableの一引数subsetを避ける列アクセスへ修正し、structuredキー・入力保全・data.frame/dtplyrを実装時に検証 |
| #665 | expansion executorで公開`.id`のintegerを登録 | 全条件維持。defaultの.idのみ、空filter、全sort/label、直接とcomputeを横断。source型を縮小しない |
| #666 | 宣言型の修復を0行だけでなく全欠損の非空列にも適用 | 全条件維持。Parent/Total/across、sum/mean、missing/zero、prefix/full/materialized、#654のlimit/警告を維持。通常集約型へ拡張しない |

#665/#666は異なる漏れである。前者は宣言の登録、後者は既存宣言の適用範囲。共通の型境界を使って直せるが、片方が片方の代用にはならない。#661〜#663は同じmaterializerへ触れるため、Issue単位の機構追加を並行に積むより、Bの一つの実装で各受け入れ条件を検証する。

## 8. 決定後の実装順序とADR更新案

1. **#661の封鎖**：専用direct computeだけを変更前に拒否。8ケースの停止証拠を元に最小patchを本実装化し、必要なpackage gateを通す。停止中はdirect collectを案内する。独自classを外したcomputeへの誘導は型/順序を暗黙に失うので代替策にしない。
2. **Bの採択と契約文章の確定**：型/orderの維持、bare衝突拒否、宛先metadata例外、savepoint/flag意味、監査範囲を先に固定する。
3. **#661/#662/#663を一つのmaterializerで実装**：target identity、single INSERT、savepoint、index/analyze委譲境界、rowid order/window metadataを整理。安全matrixを満たして停止を解除する。
4. **#665/#666の型境界を実装**：executorの宣言登録とall-NA復元。直接収集とmaterializationを同じ期待値に照合する。機構変更と別commitにもできるが、release時の保証は一式で検証する。
5. **#664を独立修正**：SQLiteのADR変更を待つ必要はない。今回のSQLite試作には混ぜていない。
6. **正式検証**：隔離prototypeを製品コードへそのまま移さず、validation、コメント、snapshot、generated docsを整える。`Rscript tools/review-ready-check.R`、必要なquery-policy/runtime gate、release matrixで確認してからreviewへ進める。

更新対象と提案する内容：

- **ADR0031**：専用結果境界をsource-column anchorとpackage-created declared typesに分け、#653以降のunsorted/text/.id/shareも記載。computeの「ordered compoundをstageに作る」記述を「public anchor CTAS→ordered public query INSERT」へ置換するamendment。保存先全操作のidentity、危険なbare拒否、実行時metadata費用、SAVEPOINT、入力保全を明記。
- **ADR0018**：Margin orderのキー/直接結果・直接materializationのscopeは維持。rowid ORDER BYと空window orderingを明記。通常dplyr変換後の順序を追加保証しない。
- **ADR0020**：lazy構築の例外は増やさない。#640 amendmentのexecution entry説明を新しいcomputeへ更新。destination照会はcallerの明示computeの内部で、source schema/type probeではないと記載。実行入口snapshot/runtime gateも同期する。
- **ADR0027 / CONTEXT Sent query / 公開last_sent_queries**：構築時result renderとcompute内部SQLを区別する。全実行監査を今回の機能であるかのように述べない。監査ON/OFFの意味・warning独立性は保持する。
- **ADR0016 / 公開結果説明**：class例外がsorted typed-missingだけという説明を実際の型境界へ更新。任意aggregate型/任意属性の再構築は追加しない。
- **公開summarize/expand/share・database guide**：直接/有限/compute/その後のverbを分け、当面の安全拒否とqualified index上流制約を必要な場所に明記。roxygen/README生成は本実装時に行う。

## 9. 残るリスクと未検証

- これは設計を選べる証拠であり、出荷承認ではない。full suite、strict coverage、lintr/jarl、source tarball、release matrixは未実施。isolated prototypeに対して正式PRのreview-ready境界を通ったとは言わない。
- 別OS/別SQLite・dbplyr version、接続切断、ディスク不足、rollback/release自体の失敗、同時writer、大規模データの時間/メモリは未検証。
- quoted-name全SQL文法、schema/view/virtual tableの全組合せ、trigger、extended type（Date/POSIXct/integer64等）の一般的復元、任意の`...`/deprecated `cte`互換性は網羅していない。prototypeのargument validationは本実装品質にしていない。
- rowidは返されたmaterializationを直接読むための根拠。利用者が後から表を書き換える、VACUUMする、再接続からplain tblを作り直す場合まで同じ順序を保証する案ではない。
- Cの保証損失は検証済み。B-directは今回対象の保証を失わなかったが、宛先metadataの費用と安全拒否という変更はある。qualified indexの上流制約は残る。
- #664のstructured-key追加正常系と本修正は未実施。既存data.table拒否を再現したことを解決と混同しない。

## 10. 成果物と再実行

すべて `/private/tmp/marginplyr-sqlite-design-20260925` 内。ルートは無修正HEAD archive、`variants/A`、`variants/B`（stage）、`variants/B-direct`、`variants/C`、`variants/A661-stop` が独立した試作コピー。Git branch/worktree登録やGitHubの変更はしていない。

```sh
cd /private/tmp/marginplyr-sqlite-design-20260925
Rscript review/cross-check.R . review/baseline
Rscript review/cross-check.R variants/A review/A
Rscript review/cross-check.R variants/B review/B
Rscript review/cross-check.R variants/B-direct review/B-direct
Rscript review/cross-check.R variants/C review/C
Rscript review/B-direct/run-regressions.R variants/B-direct
Rscript review/A/safety.R variants/A661-stop
Rscript review/B-direct/commit-check.R
```

詳細： [契約と履歴](contracts-history.md)、[A/安全封鎖](A/README.md)、[Bの二方式](B/report.md)、[独立した#664/#655確認](independent/README.md)。GitHubから取得したIssue本文は隔離rootの`issue-661.json`〜`issue-666.json`に保存した。
