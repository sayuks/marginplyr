# 独立した追加検証

Investigated: 2026-09-25

## #664 の分離

`664-probe.R` を fresh `Rscript --vanilla` で HEAD archive に対して実行した。原本・variant の編集はない。[結果](664-results.csv)、[log](664.log)。

- 1 行 character-key の data.table は `.sort="first"/"last"` で Issue の column-join diagnostic を再現した。
- `.sort="none"` は成功。data.frame と immutable dtplyr の first/last/none controls は独立した `c("Total","a")` / `c("a","Total")` と一致した。
- fixed `.by=year`、`.id="set"`、missing `g` の 3 行 fixture の data.frame/dtplyr controls も public columns、6 行、値、first/last order を確認した。
- すべてで original data.table の serialized bytes が不変だった。
- 既知の拒否再現を成功条件として含むため、CSV が全 PASS であっても #664 を修正した意味ではない。SQLite 設計とは切り離して受け入れ条件を維持できる根拠である。structured-key の網羅検証・修正は今回実施していない。

## B の #655 監査互換

既存回帰は `tests/testthat/test-sent-queries-process.R` と `tests/testthat/fixtures/sent-query-warning.R` にある。前者は installed package を subprocess で読むため、B を read-only のまま検証する小実験 `655-fresh.R` を作った。各 case は fresh `Rscript --vanilla`、`pkgload::load_all(variants/B)` 後に `Sys.unsetenv("TESTTHAT")`、`options(warn=2)`。`run-655.py` は 16 case を各別プロセスで走らせた。

次の直積で実行した：監査 on/off、implicit/explicit `na.rm`、sorted typed/unsorted id、direct collect/compute 後 collect。[40 件の照合結果](655-results.csv)、[log](655-assert.log)、[照合 script](655-assert.R)。

- implicit `na.rm` の 4 組は監査 on/off とも同一の `Missing values are always removed` warning-derived error を返した。
- explicit `na.rm=TRUE` の 4 組はともに成功し値が一致した。
- すべてで `rlib_warning_verbosity` が復元され、source table が不変だった。
- 監査 on の全 case に non-NA `result` SQL が記録された。
- 各実行の RDS と log も同 directory に保存した。

この実験は SQLite の direct result と B の compute boundary を検証した。既存 #655 の DuckDB selection-proxy case は今回再実行していない。`warn=2` error が audit によって消費されないことを確認しており、DBI 全 SQL の実行監査を提供する意味ではない。

## 追加比較: B-direct

B-direct の宛先 preflight 整備後、同じ `655-fresh.R` を `variants/B-direct` に向け、同じ 16 fresh process / 40 照合を再実行した。結果は **40/40 PASS**。B-stage の log/RDS はそのまま残し、[B-direct の結果](B-direct/655-results.csv)・[log](B-direct/655-assert.log)・各 case の RDS/log を別 directory に保存した。B-direct の全 `R/*.R` SHA-256 を [source-hashes.json](B-direct/source-hashes.json) に保存し、試験中に source が変わらなかったことも確認した。

| 確認事項 | B-stage | B-direct |
|---|---:|---:|
| fresh R process | 16 完了 | 16 完了 |
| audit on/off の照合 | 40/40 PASS | 40/40 PASS |
| implicit `na.rm`、`warn=2` | 両 audit 設定で同一 error | 両 audit 設定で同一 error |
| explicit `na.rm=TRUE` | 両 audit 設定で同一値 | 両 audit 設定で同一値 |
| verbosity 復元、source 不変、non-NA result SQL | 維持 | 維持 |

この比較が確認するのは #655 の SQLite result/compute 境界である。両案間の全エラー文が完全に同じであることや、未実行の DuckDB selection-proxy case まで維持されたことを主張しない。
