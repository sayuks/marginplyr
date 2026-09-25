# 案Aの隔離試作と#661の即時停止策

調査日: 2026-09-25。対象HEAD: `6a5f611d41bd462f3c933edea1dd0b9861d6e892`。
原本とGitHubは変更せず、HEAD archiveに対してのみ試作した。R 4.6.1、dplyr 1.2.1、dbplyr 2.6.0、DBI 1.3.0、RSQLite/SQLite 3.53.3、testthat 3.3.2、pkgload 1.5.3。

## 結論

現行方式を維持した5か所の修正で#661〜#666のSQLite再現（#664除外）と既存SQLite正常系は通る。しかし、同名のTEMP表がある場合にbareな永続保存先へ`compute()`する追加反例では誤書き込みが残る。したがって本A試作は**安全性が未完了で、導入可能な実装ではない**。Issueの再現だけを個別に直して完了とするのは不十分である。

最優先の#661には、最終方式の決定まで専用クラスのdirect `compute()`を明示的に拒否する停止patchを用意した。8ケースで拒否がテーブル作成・変更より前に起きることと、direct `collect()`および通常dbplyrの`compute()`が利用できることを確認した。これは一時的な機能停止であり、恒久的な保証縮小案ではない。

## Aで変更した責任

- #661: `remote_name()`から`remote_table()`へ。callerが渡した`other.report`をINSERT/ANALYZEまで保存する。**bare persistent名のschemaはこれだけでは確定しない**。
- #662: stage作成前からSAVEPOINTを開始し、成功時RELEASE、失敗時ROLLBACK TO/RELEASE。outer transactionがあれば、その所有権をcallerに残す。`in_transaction`は引数として消費しinner dbplyrへ渡さない。
- #663: raw rowidによるarrangeの後に`window_order()`を空にする。queryのORDER BYは維持し、select/renameが書き換えるwindow metadataからraw SQLを除く。ADR 0018の既存方針と一致する。
- #665: `execute_margin_expand()`が明示された公開`.id`をintegerとして宣言する。
- #666: 宣言済みの列について、0行または全欠損なら宣言されたR型へ修復する。通常のaggregateの型は変更しない。

`R/sqlite-typed-order.R`は239行から254行になる。展開側の型宣言を除けば、public query/typed companion query/zero-row anchor/hidden sort columns/stage表/target表/rowidという現行構造をすべて残す。行数差は小さいが、所有責任は減らない。

## 境界別の実行結果

| 境界 | Aの確認結果 | 維持する責任・注意点 |
|---|---|---|
| 直接collect | scalar source types、型付き欠損、first/last順序、公開列、fixed partitions、rowid名衝突時のcollectを維持 | companion queryを実行しhidden列をR側で除去。all-NA宣言型のみ修復 |
| 有限collect | 0/1/2/3/4/Inf、Parent/Total/across、sum/mean、欠損/ゼロ分母、text/NULL/NAラベルを確認 | n validation/既存警告動作は既存テストで確認。通常aggregateがlogicalになる場合まで保証を広げない |
| compute | qualified destination、overwrite、caller transaction、失敗rollback、rowid direct order、physical public-only schemaを確認 | sortedは全結果をstage→targetへ二度書く。bare persistent/temp同名の安全性は未完。qualified indexは通常dbplyr自体が失敗 |
| compute後のdplyr | first/lastでselect、rename、filter、mutate、arrangeを実行し期待値を確認 | 追加の順序保証は導入しない。window orderingを消すのでADR0018の方針に戻る |

既存の`tests/testthat/test-sqlite-*.R`は21テスト・550 assertions、失敗0、エラー0、警告0、skip0（`existing.log`）。既存正常系にはscalar source types、固定キー欠損、内部名のcase-insensitive衝突、rowid部分衝突/全衝突、永続表overwrite/index、insert失敗cleanup、empty summary share/idが含まれる。

追加再現matrixは`repros.R`を無修正HEADとAに同条件で実行した。Aは69ケースを通過。同じ条件の無修正HEADは52失敗・17成功だった。内訳は#661が8、#662が16、#663が2、#665が18、#666が24、source自己overwriteのatomic refusalが1。CSVはcaseごとの成否を保持する。#662の失敗ケースは単なるエラーではなく、意図した`UNIQUE constraint failed`まで到達したことを確認する。

## #661の追加反例と停止策

追加反例（`extra.log`）:

```r
# temp.reportには(g="sentinel", id=99L, total=-1)が存在する。
compute(query, name = "report", temporary = FALSE, analyze = TRUE)
```

Aでも`main.report`は空のままで、`temp.report`に3行が追加された。sorted/unsorted両方。`remote_table()`がbare `` `report` ``を返し、作成先のmainと後続の名前解決先のtempが分かれる。完全修正では**作成、DROP/overwrite、INDEX、INSERT、ANALYZE、返却tblが同じ保存先を指す**ことが必要になる。

bare名を常に`main`に修飾して通常dbplyrへ渡すだけでは、従来のbare-name index正常系を失う。dbplyr 2.6.0のqualified index SQLは`CREATE INDEX report_g ON other.report (g)`となりSQLiteに拒否される。普通のdbplyrでも同じ結果を確認した。この点は上流バグの修復をmarginplyrが全部引き受ける理由にはならないが、Aの完全な保存先設計には処理境界の判断が要る。

停止patchは`661-disable-direct-compute.patch`、実装コピーは`variants/A661-stop`。direct判定の直後にPackage conditionで拒否し、stage/savepoint/targetへ進まない。`safety.R`はsorted/unsorted × qualified/bare × outer transaction有無の8ケースを実行し、main/temp sentinel、表一覧、caller markerの保持とrollback権限を確認する。通常dbplyrの`compute()`そのものにある同名解決リスクを修復するpatchではない。

`661-incomplete-identifier.patch`は比較用の不完全な1行修正。**これだけを安全修正として採用してはならない**。

## 失敗処理・入力保全・未検証点

SAVEPOINT試作では、outer transactionがある場合も、なかった場合も、overwrite先の既存内容をunique-index失敗で復元し、caller markerを保持し、stage表を残さず、後続のcomputeが成功した。qualified index失敗でも同じ性質を確認し、その後qualified overwrite/analyzeが成功して`other.sqlite_stat1`に3行の統計が記録された（`qualified-failure.log`）。

入力表自身への`overwrite=TRUE`は、sourceをdropした後のanchor参照が失敗する。Aはrollbackでsourceを復元する。普通dbplyr（既定in_transaction=FALSE）は同じ失敗の後sourceが消えることをcontrolで確認した。この自己overwriteの成功は本試作で追加保証しない。

SQL生成/renderingを変更していないため、直接collectで表示・記録されるSQLと実行するcompanion SQLは現行機構を維持する。savepointと型修復はlazy constructionのsource readを追加しない。SQL監査の横断runtime gateと全バックエンドは本担当では未実行（親担当が横断検証）。

未検証・未完了:

- bare persistent/temp衝突の完全修正、qualified indexesの上流限界を含む保存先normalization方針。
- 無効な`in_transaction`値のvalidationと、API上これをどう説明するか。AはTRUE/FALSEともに独自atomicity用savepointを持つので、普通dbplyrの「FALSEならtransactionなし」を文字通り実装するわけではない。
- 接続切断、ROLLBACK/RELEASE自体の失敗、OS/SQLite別version、concurrent writer。
- full suite、lintr/jarl、coverage、source tarball check。実装を公開するPR/commitではなく隔離prototypeなのでreview-ready境界は未実施。
- レンダリング/ドキュメント生成は未変更・未実行。

## Issueの受け入れ条件への提案

#661は既存条件を維持し、TEMP同名＋bare永続名、および返却tbl/INDEX/DROPも同じ宛先という条件を加える。全体設計の結論まで停止策を先行できる。

#662/#663/#665/#666は既存条件を維持できる実行証拠がある。#662のsavepoint所有権と`in_transaction`の説明はADR0031で明文化する。#663は保証変更を要さず、ADR0018のwindow ordering方針を適用する。#665/#666はpackage-created型の宣言/修復漏れとして小さく独立できる。

Aを選ぶなら、#661停止→保存先ownership→SAVEPOINT境界→window metadata→型宣言/修復→全gateの順。ADR0031はqualified/bare保存先の所有、failure/overwrite atomicity、caller transaction、declared package-created columnsへ広がった適用範囲を追記する必要がある。ADR0020の明示compute境界とADR0027の実行SQL記録との整合を再確認する。

## 再実行

```sh
Rscript review/A/run-existing.R variants/A
Rscript review/A/repros.R variants/A review/A/repros.csv
Rscript review/A/repros.R . review/A/baseline-repros.csv
Rscript review/A/safety.R variants/A661-stop
Rscript review/A/extra.R variants/A
Rscript review/A/qualified-failure.R
```

上のworking directoryは`/private/tmp/marginplyr-sqlite-design-20260925`。ログ、CSV、スクリプト、patchはすべて`review/A/`にある。
