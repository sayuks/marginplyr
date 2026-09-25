# B案: 現行保証を保つ実装の縮小（隔離試作）

2026-09-25。元リポジトリとGitHubは変更していない。以下の変更は `variants/B` と `variants/B-direct` のみ。production-ready patchではなく、設計判断の実行証拠である。

## 結論

Bの中では **B-direct（型付き宛先を0行anchorで作成 → ordered public queryを1回INSERT）** を推す。直接collectに必要な外側typed UNIONとhidden order columnsは維持する一方、compute専用のfull-row temporary stageを削除できた。今回の問題が出るdirect/finite collectとcompute後操作の保証を小さく保つには、collect側と保存側の型保持方法を同一SQLへ押し込む必要はない。

**B-stage（型付きtemp stage → 通常dbplyr compute）** も成立する。最終宛先のCREATE/INDEX/ANALYZEをdbplyrへ全面委譲できる利点はあるが、ソートなし結果にまで2回の全行書き込みを増やす。型stageの作成・INSERT・cleanup・savepointと、dbplyr側の宛先曖昧性に対する入口ガードは依然必要である。B-directより少ない所有責任になるとは判定しない。

## 試作した機構

| 項目 | B-direct | B-stage |
|---|---|---|
| 直接collect | 現行typed UNION、hidden order keys、公開列投影を維持 | 同左 |
| 有限collect | 現行n検証/LIMITを維持。package宣言列だけall-missingを宣言型へ復元 | 同左 |
| compute行コピー | ordered public SELECTを型付き宛先へ1回INSERT | public SELECTを型付きtempへINSERT、そのtableから最終CTAS。2回 |
| compute用hidden key columns | 不要 | 不要 |
| compute用stage table | なし | public-onlyのtyped temp table 1個 |
| 型 | zero-row source anchor/宣言CASTが宛先の列型を作る | typed tempの列宣言を最終CTASが引き継ぐ |
| 順序 | ordered INSERTで割り当てたrowidを直接返却queryで明示ORDER BY | tempをrowid順に最終CTASし、最終rowidで明示ORDER BY |
| 後続dplyr | `window_order()`でraw SQL rowidがwindow metadataへ伝播することを止める | 同左 |
| 宛先/index | 0行anchorの通常computeが作る。INSERT/ANALYZEは完全修飾名で実行 | 完成したtyped tempの通常computeへ委譲 |
| atomicity | RSQLite named savepoint | 同左 |
| 入力と同名overwrite | 現行同様に失敗しrollbackで元入力を保全 | stageに先に全行を読んでいるため成功可能（追加能力） |

直接collectの実装責任そのものは消していない。外側projectionではall-NULL source dimensionのR型を失う、というADR0031の理由は残っている。B-directの削減対象は、そのcollect用SQLをcomputeにも一度materializeする必然性がない、という点である。

型修正は2点のみ。#665は `execute_margin_expand()` から `.id` のinteger宣言を登録する。#666は既存宣言の復元条件を「0行」から「その列がall-missing」へ変え、integer ID/character label/double shareを扱う。通常のSQL集約列へ新しい型保証を加えていない。

## 実行証拠

環境はログのsessionInfoに記録。R 4.6.1、dplyr 1.2.1、dbplyr 2.6.0、DBI 1.3.0、RSQLite 3.53.3。

- `public-cases.R`: B-direct、B-stageとも **77ケースPASS**。#661のin_schema/DBI::Id、sorted/unsorted、analyze、overwrite、別schema sentinel保全。#662のcaller transaction有無、in_transaction TRUE/FALSE、成功/unique index失敗、caller markerと元宛先rollback、後続再実行。#663のselect/rename/filter/mutate/arrange、physical public schema。#665のdefault/explicit grouping・全sort・全label。#666のParent/Total/across・sum/mean・固定partition・0件/有限/全件、元のmissing numerator例を実行。
- 既存 `test-sqlite-typed-order.R` と `test-sqlite-empty-declared-types.R`: B-stage/B-directそれぞれ **21 tests / 550 assertions PASS、警告・skipなし**。all-NULL char/int/double source dimensions、有限件数検証、normal order、fixed keys、rowid alias、持続表/index、失敗cleanupを含む。
- `destination-safety.R`: B-direct **27ケースPASS**。bare string、ident、DBI::Id(table=...)、AsIs、ドットを含むliteral名。main/tempの逆方向も含む危険overwrite拒否、通常bare index/analyze、明示main+temp sentinel、qualified indexの通常上流失敗時rollback。
- 親担当の共通横断試験（`review/B-direct/`）は独立期待値、reverse_unordered_selects、SQL ledgerを持つ。そちらが最終統合結果の正本。
- 独立担当の `review/independent/B-direct` は#655 audit/warningsを16 fresh processes/40照合で検証。B-directの今回の最後の変更はcomputeの宛先guardのみで、collect/構築はその検証時から不変。

初回の `public-cases.initial.log` の失敗には、現行custom direct collectがdata.frame・通常collectがtibbleであることを表全体のidenticalで誤判定したもの、default expansionの公開列順を誤指定したもの、通常SQL集約の0件型まで期待したものが含まれる。最終ハーネスは公開列と値/保証された型で検証するよう訂正した。qualified index failureは訂正で隠しておらず別の上流対照・rollback検証として残した。

## 名前解決と安全性に関する追加条件

完全修飾の `other.report` はそのまま用いる。bare名は `dbplyr::as_table_path()`/`table_path_components()`で正規化し、INSERT/ANALYZEと返却tableはtemporaryならtemp、それ以外mainを明示する。

しかし、通常dbplyrのbare名によるoverwrite/index自体が他schemaを選ぶ場合がある。返却tableだけを修飾しても安全にはならない。試作は次を**書き込み前に拒否**する。

1. persistentなbare宛先をtempの同名表が隠す場合。
2. temporaryなbare宛先をoverwriteしようとしたがtempにはなく、mainに同名表がある場合。

どちらも既存の危険ケースの停止であり、型/順序保証を撤回したものではない。明示的なschema指定は通常経路へ通す。`DBI::Id(schema="temp",...)`とtemporary=TRUEの組み合わせはSQLiteで成功した。`other`等をtemporaryにする指定とは異なる。

拒否判定は要求されたcomputeの開始時にdestination metadataを調べる。DBI::dbExistsTableは条件により **1〜2回**。実送信SQLはdriver依存で、親のledgerでは1回が3 statementsになる。lazyなMargin query構築、直接/有限collectに新しいinput readやschema readは加えない。それでもADR0031の現在の「No extra schema query」を無限定に維持したとは記載できず、**compute時の安全なdestination解決に限るmetadata確認**を明記する更新が必要。

明示schemaにindexesを要求すると、dbplyr 2.6.0の `CREATE INDEX index ON schema.table` がSQLiteでsyntax errorになる。通常dbplyrの同じ条件も失敗し、試作はその制限を拡大していない。savepointで既存宛先と他schema sentinelを守る。schema対応index SQLまで独自実装することは、このB案には含めない。

## 保証・リスク・未検証

維持: typed-missing source dimensions、package integer ID/double shares、直接・有限collectの公開列、要求Margin order、直接materialization後の同保証、physical public-only schema、rowid alias全部衝突時の事前拒否、後続通常dplyr操作、失敗時入力保全、caller transaction所有権。

限定しない: parent/total sharesのdouble、empty expansion ID、materialized order。後続dplyr操作の結果に元のMargin orderを新たに約束しないことは既存ADR0018のまま。

残る責任: custom collectの有限件数追従、typed union構築、型宣言登録、savepoint、宛先の識別とambiguous name拒否、INSERT/ANALYZE、rowid実装、dbplyr window metadataへの依存。B-directを「すべてdbplyr委譲」とは呼べない。

SQL監査: 構築時のresult recordはdirect `sql_render()`と一致し続ける。computeのDDL/INSERTやmetadata照会がaudit recordへ追加されるわけではない。現在のSent query記録を実行全SQLの監査だと読むことはできない。computeがpublic SELECTを使うことはledgerでは見えるが、last_sent_queriesの新しい保証にはしていない。

未検証: SQLite/RSQLite/dbplyrの別バージョン、DB接続断時rollback自体が失敗する場合、同一接続を別実行主体が同時変更する場合、大規模の時間/メモリ実測、特殊なschema/catalog仕様、下流全verbの網羅。RSQLiteのsavepointが正常な接続でどう動くかは成功/失敗/外側transactionのmatrixで確認済みだが、災害耐性を証明したものではない。

`in_transaction`のTRUE/FALSEは成功/失敗とも検証済み。prototypeの不正値診断はstopifnotなので、productionではdbplyr同様の引数validation/contextを確認する。full package review-ready check、全lint、site regenerationは非productionの設計試作として未実施。採用後に必要な通常gateは免除されない。

## ADRとIssueへの提案

ADR0031の機構段落を「public typed destinationを0行anchorで作成し、その後ordered public SELECTをINSERT」に置換する。stage table/hidden order keyによる再ソートの記述を削り、typed collect用compound queryとcompute用public SELECTの役割を分ける。named savepointの所有権、2種類の危険bare名停止、compute限定metadata照会、window_order metadata clear、rowid alias拒否を説明する。ADR0018の順序キー/適用範囲は変更不要。ADR0020はlazy構築のexemption追加不要。ADR0027はrecordの範囲をcompute ledgerと混同しないよう点検する。

#661は既存受け入れ条件を維持し、main/temp逆方向と名表記を追加。#662/#663/#665/#666は維持できる。#663はcompute後select/rename実行を必ず置く。#665/#666は宣言登録/復元という独立責任のまま、B-direct導入でまとめて曖昧にしない。#664はこの方式と独立した通常bug fixとして別実装する。

最小安全策を先行するなら、完全な識別子をINSERT/ANALYZE/返却まで保持し、危険bare衝突を変更前に拒否する。Bを採用するまでstaging方式を全部再設計する必要はない。単にremote_nameをremote_tableに置き換えるだけではmain/tempのbare衝突を防げない。

## 再実行と成果物

```
Rscript review/B/public-cases.R variants/B-direct
Rscript review/B/destination-safety.R variants/B-direct
```

上の相対パスは隔離rootをcwdにした場合。`public-cases.R`の出力CSVは隔離root内へ固定されている。`B-direct.patch`と`B.patch`は隔離rootの元HEADからの差分。`*-R-sha256.txt`は最終sourceのhash。`build-*.initial.py`は初期探索の記録であり、最終prototypeの再生成スクリプトではない。最終sourceはvariants配下とpatchが正本。
