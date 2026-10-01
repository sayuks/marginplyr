# Grouping plan の幅・出現位置・実データ対応の境界調査

Investigated: 2026-10-01
Target: `3390e24420dc58add63ab0fb4d6584190d125403`
Outcome: 検証範囲内で marginplyr の契約違反は確認されなかった。

## 結果と作業境界

2026-10-01 に、小さい合成データを用いて、Grouping plan、識別情報、
集計・展開・ネストの元行対応、Parent/Total share、Margin order を検証した。
比較対象は列名ベクトルからなる順序付き set 一覧と元行 slice から独立に
生成した。`inspect_grouping()` は検査対象に含め、期待値生成には使用しなかった。
公開関数だけで到達する経路を使い、private field の変更は行わなかった。

31 列超の plan は受理され、set ごとの全ビットと出現位置、行・値の対応が
保持された。引数なし `grouping_id()` の 32 列以上での拒否は契約どおりだった。
31 列以下を明示した helper と `grouping_bit()` は同じ多列 plan で成功した。
確認済みの修正対象がなかったため Issue は作成しなかった。

対象コード・恒久 tests・仕様は変更しなかった。この調査のリポジトリ成果物は
この Markdown 1 ファイルだけである。一時コード、依存コピー、DB、ログ、CSV、
SQL はリポジトリ外の `/private/tmp/grouping-identity.qdhaII` に置いた。
そこにあるファイルを失っても、下記コードから主要な結果を再生成できる。

## 対象と環境

開始時の branch は `main`、HEAD は上記 SHA、`git status --short` は空だった。
`git archive` で同 SHA を外部ディレクトリへ展開し、依存のコピーを作り、
その source を専用 library に `R CMD INSTALL` した。
調査コードはその library と R 標準 library だけを `.libPaths()` に設定した。
最後に各 backend で実際にロードされた namespace の版とパスを記録し、
正規化したパスがこの二つの library に属することを確認した。

R は 4.6.1 (2026-06-24)、Darwin 25.6.0 / arm64 だった。
DB は DuckDB v1.5.5、SQLite 3.53.3、PostgreSQL 17.11 (Homebrew) だった。
DuckDB と SQLite は使い捨ての in-memory DB、PostgreSQL は専用 `pgdata`、
loopback の port 55473、専用 socket を持つ cluster を使用した。
PostgreSQL の起動と接続は sandbox の共有メモリ・接続制限により
承認済みの sandbox 外実行を使用した。既存 DB には接続しなかった。

主要な依存は dplyr 1.2.1、dbplyr 2.6.0、dtplyr 1.3.3、
data.table 1.18.6.1、DBI 1.3.0、RSQLite 3.53.3、duckdb 1.5.5、
arrow 25.0.1、RPostgres 1.4.10、testthat 3.3.2、pkgload 1.5.3、
lintr 3.4.0 だった。jarl CLI は 0.6.0 だった。
実際にロードされた依存の全一覧は後掲する。
外部コードの整形にだけ formatR/styler を専用 library に追加した。
これらは調査対象の依存や DESCRIPTION には追加しなかった。

## 契約・既存 assertion・今回の検証の対応

読んだ入口は [AGENTS.md](../AGENTS.md)、[CLAUDE.md](../CLAUDE.md)、
[CONTEXT.md](../CONTEXT.md)、[local checks](../design/agents/local-checks.md)、
[調査ノート規約](README.md) だった。公開仕様は
[Grouping identity](../vignettes/grouping_identity.qmd)、
[Grouping specification](../R/grouping-spec.R)、
[contextual helpers](../R/grouping-context.R)、[shares](../R/share.R) を確認した。
以下の表は読んだ時点の契約と assertion を表す。

| 重点仮説 | 契約・実装 | 既存 assertion | 今回追加した条件と結果 |
| --- | --- | --- | --- |
| set の出現位置と bit pattern が混同される | [ADR 0009](../design/adr/0009-distinguish-grouping-set-identifiers-from-grouping-identifiers.md)、[plan](../R/grouping-plan.R)、[inspection](../R/inspect-grouping.R)。`.id` は policy 適用後の 1-based occurrence、同一 set でも keep は別 ID | [margin ID](../tests/testthat/test-margin-id.R) は重複 ID と DuckDB native/union を検証。[inspection](../tests/testthat/test-inspect-grouping.R) は set 一覧と policy を検証 | 隣接・離れた重複、product 由来の重複、drop の再番号付け、`.id` 有無、元データの完全重複を独立 multiset 比較。保持された |
| 高位 bit・31 列境界で識別が壊れる | [helpers](../R/grouping-context.R)、[ADR 0013](../design/adr/0013-inspect-grouping-plans-as-ordinary-tibbles.md)。明示 helper の最終引数が LSB、最大 31 列。inspection は超過時だけ numeric ID が `NA_integer_` | inspect の 31 列 rollup は 0 と `.Machine$integer.max`、bit 長を検証。32 列は ID が NA と bit 長を検証。[interface](../tests/testthat/test-grouping-interface.R) の bare helper 境界は単一 set の ID=0 と固定キー、32 列拒否 | 30/31/32/33 列の複数 set、先頭・中間・末尾、高位・全省略・交互、helper 引数の逆順・固定キー挿入。64/65 列の少数 set も列名付き bits と実行結果が一致 |
| 入力幅・dimension 幅・constructor 項数を混同する | [plan](../R/grouping-plan.R) は解決済み列の最初の出現順、composite は一緒に inclusion/omission。固定キーは全 set に残る | [grammar/plan](../tests/testthat/test-grouping-plan.R)、[interface](../tests/testthat/test-grouping-interface.R) は構文展開、composite の masks、固定キーを検証 | composite cube、product、rollup、反復項 rollup、selector rollup。入力列順・set 内列順・set 順を変更して独立に再計算。成功 |
| native と portable で値・多重度が変わる | [native adapter](../R/grouping-adapter-native.R)、[union adapter](../R/grouping-adapter-union.R)。公開 duplicate policy と ID/share の必要性から経路選択 | margin ID/backend tests の通常サイズ、既存 native/union 対照には内部 plan を共有するものもある | DuckDB/PostgreSQL の duplicate-free 31 列 plan を公開 drop/keep で両経路へ到達。同じ意味の結果を参照値と手書き SQL に照合。成功 |
| 正しい値を別 set・固定区分へ付ける | native/union の branch、[shares](../R/share.R)、[ADR 0010](../design/adr/0010-compute-parent-shares-as-a-contextual-summary.md)、[ADR 0017](../design/adr/0017-calculate-total-shares-against-the-grand-total-set.md) | [share tests](../tests/testthat/test-share.R) は composite rollup と `rollup(region, region)` も検証 | 各 set 内の元行 slice、n・sum・source checksum・各 bit を同時比較。Parent は認められた pure rollup の次の異なる coarser set、Total は同固定区分の空 variable set。成功 |
| 展開・nest の所属やコピー数が欠落する | expansion は occurrence ごとの元行コピー。nest の keep duplicate は拒否、`.keep` は payload keys の保存を決める | 直前の [nesting 調査](nesting-data-integrity-2026-10-01.md) は 1,334 判定、指摘なし | expansion に完全重複元行を追加して multiset 比較。local/dtplyr nest の幅 31/32/65、欠損・空・重複元行、`.keep`/`.id`/by variants を追加。成功 |
| sort が表示値だけで異なる set を混同する | [ADR 0018](../design/adr/0018-order-margin-results-by-grouping-structure.md)、[Margin order](../tests/testthat/test-margin-order.R)。固定キー後に dimension ごとの bit・missingness・value、最後に occurrence | 通常幅の構造的順序・SQL collect/compute 対照 | first/last、全キー NA、重複 occurrence、31/32 列で独立構造キーと比較。直接 compute 後も一致 |
| empty/all-missing の helper 型が失われる | [ADR 0033](../design/adr/0033-preserve-declared-types-in-empty-margin-results.md)、[helper types](../tests/testthat/test-sqlite-grouping-helper-types.R)。`.id` integer、直接 helper numeric、local/dt integer、share double | SQLite empty collect/compute、Arrow helper、空 Grand total などを検証 | 空固定区分、非空全欠損、空非 partitioned Grand total、31/32 列で型と値を確認。成功。普通の summary 型は backend に委譲される範囲を区別 |
| 遅延構築中に入力データを読む | [ADR 0020](../design/adr/0020-ask-before-reading-a-lazy-input.md)、AGENTS の query 契約 | query policy の実行 entry point snapshots | SQL backend の audit で inspection は result read なし、summary 構築は result-purpose SQL 1 件。その他は LIMIT 0 等の metadata。collect/compute は別段階として成功 |

五つの幅を別々に扱った。基本 fixture の入力幅は `w + 4`
（variable `w`、fixed、rid、value、code）、解決済み variable 幅は `w`、
`cube(grouping_set(d01, ..., d(w-1)), d(w))` は constructor 2 項・4 occurrence、
反復 composite rollup は 3 項・keep 時 4 occurrence だった。
helper の指定幅は `min(w, 31)`、選択した 3 列、逆順、または固定キーを
含む最大 31 列とした。31 variable + fixed の bare helper は成功し、
32 variable の bare helper と固定キー込みの 32 明示列は正常に拒否された。

`.by` の固定キーと全 set に現れる `.grouping` 列は、default label の
型変換まで同一とは扱わなかった。数値固定キーの対照では `.by` は numeric、
variable 扱いでは default label により character になり、値・多重度は
変換を明示した対照と一致した。`.margin_label = NULL` の主検証では
元キーの型と欠損を保存して比較した。

## 過去の証拠との区別

[Property-Based 調査](property-based-testing-margin-semantics.md) と
[コード](property-based-testing-margin-semantics.R)、
[Metamorphic 調査](metamorphic-testing-margin-semantics.md) と
[tests](../tests/testthat/test-metamorphic-margin-semantics.R) を照合した。
その少数 dimension の constructor 展開・値・変換の範囲を未調査とは扱わず、
今回の小さい pilot は独立 oracle の校正と位置・幅境界への橋渡しに使った。
直前の nesting 調査の正常例は既存証拠として扱い、多列と occurrence の
組み合わせを追加した。

先行数値調査の外部 report
`/private/tmp/marginplyr-hunt.lkgXyQ/report.md` も読んだ。
nonadditive summary、固定区分 share、少数列の direct compute はそこで
実行済みだった。今回の主目的はそれらの全面再調査ではなく、多列の
識別と denominator の対応だった。先行 report の保存場所自体は
再実行の前提にしていない。今回使用した oracle と全 fixture は後掲する。

## ケース族と独立期待値

`D` を順序付き variable 列一覧、`a/m/z` を先頭・中間・末尾とする。
基本データは 8 行、固定区分 P が 6 行、Q が 2 行、measure は
`2, 5, 11, 17, 23, 31, 101, 211`、source checksum は `2^(0:7)` とした。
P total=89、Q total=312。三つの位置だけ値・通常 NA を変え、他の列は
constant とした。全列 constant だけの対照にはしなかった。
完全重複行を足した variant では rid/checksum も重複させ、多重度を保存した。
その他は全キー NA、0 行、measure 全 NA の variants を使用した。

| case family | ordered set の期待値 | 狙った誤り | 判定 |
| --- | --- | --- | --- |
| boundary | `D, D-a, D-m, D-z, {a}, {m}, {z}, {}, odd(D), even(D)`（少数幅では位置重複を除く） | bit shift、末尾欠落、NA numeric ID による統合 | 一致 |
| duplicates | `D, D, {a}, {}, D, {}` | 隣接・離れた occurrence の消失、drop 再番号付け | keep/drop/error が契約どおり |
| overlapping product | `rollup(a) × rollup(a) × grouping_set(D-a)` → `D,D,D,D-a` | 異なる構文から生じた同じ set の統合 | 一致 |
| composite cube/product | `D, D-z, {z}, {}` | composite 構成列の分離、constructor 項数を bit 幅に使う | 一致 |
| composite rollup | `D, D-z, {}` | 親の元行・分母・bit のずれ | 一致 |
| repeated rollup | `D, D, D-z, {}` | 同一 grain を親にする、重複により値を変更 | 一致 |
| scalar selector rollup | `D, D-last, ..., {first}, {}` | selector の解決列を一つの項と扱う | 一致 |
| symbolic width | 64/65 列で `D,D-first,D-last,{}` | 31/32/64 bit 付近で plan を数値 ID に依存させる | 一致 |

代表手書き plan は 3 列の composite cube/product の
`[abc, ab, c, {}]`、repeated rollup の `[abc, abc, ab, {}]` とした。
省略 flags を列名ごとに `0/1` で保持し、許容範囲だけ
`sum(bit[j] * 2^(k-j))` を独立に計算した。
例えば `[abc, ab, c, {}]` の ID は `0,1,6,7`、
31 列では先頭だけ省略=1,073,741,824、末尾だけ省略=1、
全省略=2,147,483,647 だった。逆順 helper の期待値は引数順から再計算した。
32 列以上の inspection ID は NA としつつ、named bits と occurrence は
捨てずに比較した。64/65 列も R integer 一個へ符号化しなかった。

各 set のグループは fixed と included columns の等値・通常 NA による
元行 slice として求めた。集計値だけでなく n、checksum、キー、bit、ID、
occurrence の組を比較した。同じ n でも違う元行を持つグループを区別した。
例えば P 内の `ab=A/U` の sum=7、該当 `c=Y` 行の Parent share=5/7、
Total share=5/89 であり、別の固定区分 Q=312 は分母にならなかった。
純粋 rollup の親は同じ set の反復を飛ばした次の異なる coarser set とした。
任意 set 集合へ新しい親規則は要求しなかった。
重複 Total set は同じ独立 slice を持つので、未規定の occurrence 選択を
新しい公開保証として扱わなかった。

SQL summary の全欠損 sum は普通の SQL と同じ NA、local/dt の
`sum(..., na.rm=TRUE)` は 0 とした。各 backend の普通の GROUP BY を
set ごとに別に実行して対照とした。native 用の手書き SQL は独立の
列一覧と weight を使い、`GROUPING()` の bit と total を裏取りした。
元データの値はこの SQL 対照だけに依存せず、source slice の手計算とも比べた。

`.id` がない結果では、同じに見える行を deduplicate しなかった。
sort は比較用の並べ替えだけで、行数と同じ行の多重度を保持した。
nest cell は元行内容の multiset、expand は occurrence ごとの元行コピーと
省略キーを比較した。order の確認は別に行い、同順位 payload の順序を
新しい保証として要求しなかった。

## 校正・経路の実測・候補分類

最初に 2/3 列の手書き plan と参照 model を照合した。
比較器に高低 bit の交換、異なる set の ID 交換、duplicate occurrence の
削除、32 列の NA ID を理由にした pattern 統合、fixed bit=1、
正しい sum の別 set への移動を合成して与え、6 種すべてを検出した。
この校正に通った同じ比較器を多列へ展開した。

local、dtplyr、実 DuckDB、SQLite、Arrow、実 PostgreSQL を実行した。
各 backend の最終判定数・終了状態は後掲の表に記録した。
判定 1 件は複数 assertion や direct collect/compute の比較を含むので、
判定数を SQL 実行回数や独立保証数とは扱わない。
回数を終了条件にはせず、最後の対応表見直しから次を追加した。

- total-first/set 内列順/入力列順の反転、nested union、列位置の循環変更。
- 64/65 列の少数 set、fixed-only の zero variable plan。
- 空の nonpartitioned Grand total、empty/all-NA の sort と helper 型。
- 数値 `.by` と全 set の variable 列の label 型対照。
- local/dtplyr の幅 31/32/65 における nest variants。
- 公開オプションで到達した native/portable 対照と構築時 SQL audit。

DuckDB/PostgreSQL で duplicate-free plan に drop と keep+ID を与えると、
取得した SQL にそれぞれ `GROUP BY GROUPING SETS` と `UNION ALL` が現れた。
公開意味が同じ対照で全結果が一致した。
SQL audit は入力準備・普通の GROUP BY の読み取り後にリセットし、
Margin の遅延構築を測った。inspection は result-purpose query=0、
summary は 1 の記録であり、直接 collect と直接 compute 後 collect は
構築とは別段階に実行した。

候補分類は次のとおりだった。

| 分類 | 証拠・判断 |
| --- | --- |
| 確認済み marginplyr 契約違反 | なし |
| 期待された制限・拒否 | >31 明示 helper / >31 variable の bare helper、duplicate error、nest keep の既存制限、Arrow share 非対応。Issue 化しなかった |
| 上流/backend の挙動 | Arrow 25.0.1 の `.data[["value"]]` を含む summary は通常の Arrow summary でも未対応で R への移行を促した。bare symbol injection に改めて実行。SQLite の普通の empty/all-missing summary は logical になり得た。宣言された helper/share/ID 型とは区別した |
| 仕様判断が必要 | 本調査から新規の判断候補は出なかった。任意 set の Parent、重複 Total の occurrence 選択を追加保証にしなかった |
| 参照 model/fixture/比較器・運用の誤り | SQL ordinary summary 全 NA に numeric 型を要求した初期比較、実行中ファイルの編集、R base namespace のパス照会、標準 library symlink の文字列比較を修正。影響した途中実行を完了証拠から除外し、固定コードの再実行で置き換えた |
| 検証範囲内で問題なし | 下記の最終固定コードによる全完了判定と関連 tests |
| 未検証・実行不能 | 下記の範囲。成功した範囲へ合算しなかった |

Arrow の旧ハーネス終了状態と実行中の固定版ログを取り違えた進捗報告も
あった。完了判定は最終実行の terminal exit とその CSV のみで行い、
別プロセスの 31 列 scalar rollup 対照も通過した。
一時的な実行・比較器の失敗を製品のバグ件数には含めなかった。

`to-tickets` の手順と repository の issue tracker/triage 方針を読んだ。
委任に従い、必要な候補は確認待ちにせず証拠で分割・優先順位・受け入れ条件を
決める方針としたが、今回確認できた拒否・上流挙動・ハーネス誤りは修正対象に
該当しなかった。GitHub の既存 Issue/PR の一覧も取得して照合した。
新規 Issue、既存 Issue への追記、仕様変更 Issue はいずれも不要と判断した。
各案がユーザーから個別承認されたとは扱っていない。

## 終了条件と未検証範囲

採用した仮説の plan・行・値・型・経路対照を完了し、対応表の見直しで
挙がった具体的な残存仮説も追試した。確認済み候補の再現・最小化を必要とする
製品違反は残らなかったため、この検証範囲の調査を終了した。
回数・発見件数・fixture 幅を便宜的な上限として終了していない。
実行基盤の不足で中断した範囲はなかった。

別版の依存・R、他 OS、他 DB dialect、PostgreSQL 以外の外部サービスは
実行していない。高次元の全面 cube や大量データの性能・query-size 上限、
DB 固有の最大列数/関数引数数までの探索はこの識別調査に含めなかった。
65 列より広い fixture を網羅したという主張もしていない。
Arrow は collect を確認し、直接 compute の対照、share、nest は
この対象経路として採用しなかった。nest は対応する local/dtplyr のみだった。
`.check_share_source = FALSE` は正常な既知 numeric sum を使う主検証で指定した。
share source の受理規則や任意 summary の型保証の全面調査ではなかった。
これらを再開する場合は、同じ named-bit/occurrence oracle を新しい backend・
環境へ適用し、必要な契約を先に確定してからケースを追加する。

## チェックと再実行

関連する `grouping-plan`、`grouping-interface`、`inspect-grouping`、
`margin-id`、`margin-order`、`share`、`sqlite-grouping-helper-types` の
既存 testthat tests は固定 source と隔離 library で成功した。
Markdown 内の実行可能 R コードは外部へ抽出し、setup とともに実行した。
最終埋め込みコードと実行・lint したコードの同一性は SHA-256 で確認した。
package-aware lintr は `pkgload::load_all()` 後に実行し、外部コードは
dplyr をロードした環境で個別に lint した。jarl は repository と外部 R
コード双方に repository の設定を適用した。
context budget と doc references の verifier も実行した。

この 1 ファイルは package/generation/check inputs を変えない
Repository-only change であり、[local checks](../design/agents/local-checks.md)
に従って `tools/review-ready-check.R` の package boundary は適用しなかった。
全 suite coverage、tarball `R CMD check`、site render を実施したとは主張しない。
code-review skill による review round は実行していない。

再実行は repository を変更せずに行える。後掲の四つの `r` block を、順に
`setup.R`, `hunt.R`, `followup.R`, `closing.R` として外部 code directory へ
抽出する。コード中の `#` コメントを含めてそのまま抽出する。
必要な依存を備えた library から setup を実行する。
版を変える場合は下記結果の追認とせず、新しい環境として記録する。
PostgreSQL だけは専用 cluster を起動して port 55473 が空いていることを
確認する。コードに port が明示されるため、別 port なら三つの block の
接続指定を同じように変更し、その変更を記録する。

```sh
export IDENTITY_TASK="$(mktemp -d /private/tmp/grouping-identity-replay.XXXXXX)"
mkdir -p "$IDENTITY_TASK/source" "$IDENTITY_TASK/code" "$IDENTITY_TASK/socket"
git archive 3390e24420dc58add63ab0fb4d6584190d125403 |
  tar -x -C "$IDENTITY_TASK/source"
# Extract the four R blocks below into $IDENTITY_TASK/code first.
Rscript "$IDENTITY_TASK/code/setup.R"
export IDENTITY_CODE_DIR="$IDENTITY_TASK/code"
IDENTITY_MODE=pilot Rscript "$IDENTITY_CODE_DIR/hunt.R"
for backend in local dtplyr duckdb sqlite arrow; do
  IDENTITY_MODE="$backend" Rscript "$IDENTITY_CODE_DIR/hunt.R"
  IDENTITY_BACKEND="$backend" Rscript "$IDENTITY_CODE_DIR/followup.R"
  IDENTITY_BACKEND="$backend" Rscript "$IDENTITY_CODE_DIR/closing.R"
done
# Executables from the tested PostgreSQL installation:
PG_BIN=/opt/homebrew/opt/postgresql@17/bin
"$PG_BIN/initdb" -D "$IDENTITY_TASK/pgdata" -A trust --no-locale
"$PG_BIN/pg_ctl" -D "$IDENTITY_TASK/pgdata" \
  -l "$IDENTITY_TASK/server.log" \
  -o "-k $IDENTITY_TASK/socket -p 55473 -h 127.0.0.1" start
IDENTITY_MODE=postgres Rscript "$IDENTITY_CODE_DIR/hunt.R"
IDENTITY_BACKEND=postgres Rscript "$IDENTITY_CODE_DIR/followup.R"
IDENTITY_BACKEND=postgres Rscript "$IDENTITY_CODE_DIR/closing.R"
"$PG_BIN/pg_ctl" -D "$IDENTITY_TASK/pgdata" -m fast stop
```

CSV/SQL は外部 task directory に生成される。`*-results.csv` の PASS
だけでなく各 Rscript の exit=0 を確認する。失敗時は対象コードを変えずに
列幅・set 数・構文・helper 幅を縮小する。高位 bit の失敗なら必要な幅を
消して別の拒否に変えない。実行回数を理由に打ち切らず、候補を契約違反・
拒否・上流・仕様判断・oracle 誤り・正常・未検証へ分類する。

<!-- GENERATED EVIDENCE AND EXECUTED CODE FOLLOW -->

## 最終実行の完了証拠

| backend | 主検証 | 追試 | 最終見直し | FAIL | terminal exit |
| --- | ---: | ---: | ---: | ---: | --- |
| local | 669 | 84 | 79 | 0 | 0（各 Rscript） |
| dtplyr | 669 | 84 | 79 | 0 | 0（各 Rscript） |
| duckdb | 549 | 86 | 7 | 0 | 0（各 Rscript） |
| sqlite | 549 | 85 | 7 | 0 | 0（各 Rscript） |
| arrow | 474 | 84 | 7 | 0 | 0（各 Rscript） |
| postgres | 549 | 86 | 7 | 0 | 0（各 Rscript） |

校正 pilot は 166 判定、exit=0。全最終判定は 4,320 件、FAIL=0。
SQL aggregation の missing-value warning は普通の backend semantics として記録し、値対照も通過した。

| check | 結果 |
| --- | --- |
| 7 関連 testthat files | PASS / exit 0 |
| package-aware lintr | 0 lints |
| 抽出した 4 R blocks の lintr | 0 lints |
| repository / 抽出 R の jarl | PASS |
| context budget | PASS: 20,008 / 22,005 bytes |
| doc references | PASS |

## ロードされた依存とコードの同一性

| package | version | ロードした backend |
| --- | --- | --- |
| DBI | 1.3.0 | duckdb, postgres, sqlite |
| R6 | 2.6.1 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| RPostgres | 1.4.10 | postgres |
| RSQLite | 3.53.3 | sqlite |
| arrow | 25.0.1 | arrow |
| assertthat | 0.2.1 | arrow |
| bit | 4.6.0 | arrow, postgres, sqlite |
| bit64 | 4.8.6 | arrow, postgres, sqlite |
| blob | 1.3.0 | duckdb, postgres, sqlite |
| cachem | 1.1.0 | sqlite |
| cli | 3.6.6 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| compiler | 4.6.1 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| data.table | 1.18.6.1 | dtplyr |
| datasets | 4.6.1 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| dbplyr | 2.6.0 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| dplyr | 1.2.1 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| dtplyr | 1.3.3 | dtplyr |
| duckdb | 1.5.5 | duckdb |
| fastmap | 1.2.0 | sqlite |
| generics | 0.1.4 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| glue | 1.8.1 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| grDevices | 4.6.1 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| graphics | 4.6.1 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| hms | 1.1.4 | postgres |
| lifecycle | 1.0.5 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| lubridate | 1.9.5 | postgres |
| magrittr | 2.0.5 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| marginplyr | 0.1.0 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| memoise | 2.0.1 | sqlite |
| methods | 4.6.1 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| otel | 0.2.0 | duckdb, postgres, sqlite |
| pillar | 1.11.1 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| pkgconfig | 2.0.3 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| purrr | 1.2.2 | arrow, duckdb, postgres, sqlite |
| rlang | 1.3.0 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| stats | 4.6.1 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| tibble | 3.3.1 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| tidyselect | 1.2.1 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| timechange | 0.4.0 | postgres |
| tools | 4.6.1 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| utils | 4.6.1 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| vctrs | 0.7.3 | arrow, dtplyr, duckdb, local, postgres, sqlite |
| withr | 3.0.3 | arrow, dtplyr, duckdb, local, postgres, sqlite |

全パスは外部専用 library または正規化した R 標準 library 内だった。

| block | SHA-256 |
| --- | --- |
| setup.R | `9879dbfe682dba7071c04b6d38fa8cf50579a2090328739c5c715b4945d77f1a` |
| hunt.R | `9d72f639286d26827da830d78981a47017fecd4e92c11fd40411c20ddbac48a9` |
| followup.R | `74b1f3f00612f16dc742cf5c6ea9b1b7f1f2dc16cc627ac93e62175d8713c066` |
| closing.R | `369737017b7c0d99c2e8084dec8a6ac72a026c0fd54dd46439f00d250179f95b` |

## 実行コード: setup.R — 隔離 source のインストールと依存記録

```r
task <- Sys.getenv("IDENTITY_TASK")
stopifnot(nzchar(task), dir.exists(file.path(task, "source")))
library_dir <- file.path(task, "library")
dir.create(library_dir, recursive = TRUE, showWarnings = FALSE)
roots <- c(
  "dplyr", "tidyr", "dbplyr", "dtplyr", "data.table", "DBI", "RSQLite",
  "duckdb", "arrow", "RPostgres", "testthat", "pkgload", "lintr"
)
installed <- utils::installed.packages()
dependencies <- tools::package_dependencies(
  roots, db = installed, which = c("Depends", "Imports", "LinkingTo"),
  recursive = TRUE
)
packages <- unique(c(roots, unlist(dependencies)))
packages <- setdiff(packages, c("R", rownames(installed)[
  installed[, "Priority"] %in% c("base", "recommended")
]))
manifest <- lapply(packages, function(package) {
  path <- find.package(package)
  destination <- file.path(library_dir, package)
  if (!dir.exists(destination)) {
    stopifnot(file.copy(path, library_dir, recursive = TRUE))
  }
  data.frame(
    package = package, version = as.character(utils::packageVersion(package)),
    source = dirname(path)
  )
})
utils::write.csv(
  do.call(rbind, manifest), file.path(task, "dependencies.csv"),
  row.names = FALSE
)
status <- system2(file.path(R.home("bin"), "R"), c(
  "CMD", "INSTALL", paste0("--library=", shQuote(library_dir)),
  shQuote(file.path(task, "source"))
))
stopifnot(status == 0L)
```

## 実行コード: hunt.R — 独立参照 model・pilot・主検証

```r
task <- Sys.getenv("IDENTITY_TASK")
stopifnot(nzchar(task))
.libPaths(c(file.path(task, "library"), .Library))
library(marginplyr)
library(dplyr)
stopifnot(startsWith(
  find.package("marginplyr"),
  task
))

# The reference model manipulates ordered
# column sets, never package plans.
node <- function(kind, ...) {
  list(
    kind = kind,
    terms = list(...)
  )
}
model <- function(x) {
  if (is.character(x)) {
    return(list(x))
  }
  families <- lapply(x$terms, model)
  if (x$kind == "set") {
    return(list(as.character(unique(unlist(x$terms)))))
  }
  if (x$kind == "sets") {
    return(unlist(families, recursive = FALSE))
  }
  if (x$kind == "product") {
    out <- list(character())
    for (family in families) {
      next_out <- list()
      for (left in out) {
        for (right in family) {
          next_out[[length(next_out) + 1L]] <- unique(c(
            left,
            right
          ))
        }
      }
      out <- next_out
    }
    return(out)
  }
  units <- unlist(lapply(x$terms, function(term) {
    if (is.character(term)) {
      as.list(term)
    } else {
      model(term)
    }
  }), recursive = FALSE)
  if (x$kind == "rollup") {
    return(lapply(length(units):0L, function(k) {
      if (k == 0L) character() else unique(unlist(units[seq_len(k)]))
    }))
  }
  stopifnot(x$kind == "cube")
  out <- list()
  for (k in length(units):0L) {
    indices <- if (k == 0L) {
      list(integer())
    } else {
      combn(seq_along(units), k, simplify = FALSE)
    }
    for (index in indices) {
      out[[length(out) + 1L]] <- as.character(unique(unlist(units[index])))
    }
  }
  out
}

public_spec <- function(x) {
  if (is.character(x)) {
    return(rlang::expr(all_of(!!x)))
  }
  constructors <- c(
    set = "grouping_set", sets = "grouping_sets",
    product = "grouping_spec", rollup = "rollup",
    cube = "cube"
  )
  rlang::call2(constructors[[x$kind]], !!!lapply(
    x$terms,
    public_spec
  ))
}

set_key <- function(x) paste(sort(x), collapse = "\034")
reference_plan <- function(ast, fixed, policy) {
  sets <- model(ast)
  dimensions <- unique(unlist(sets))
  dimensions <- as.character(dimensions)
  keys <- vapply(sets, set_key, character(1))
  if (policy == "drop") {
    sets <- sets[!duplicated(keys)]
  }
  sets <- lapply(sets, function(x) {
    dimensions[dimensions %in%
                 x]
  })
  list(
    sets = sets, dimensions = dimensions,
    fixed = fixed, duplicate = anyDuplicated(keys) >
      0L
  )
}

mask <- function(columns, set) {
  if (length(columns) == 0L) {
    return(0L)
  }
  stopifnot(length(columns) <= 31L)
  as.integer(sum((!columns %in% set) * 2^(length(columns) -
                                            seq_along(columns))))
}

row_keys <- function(data, columns) {
  if (length(columns) == 0L) {
    return(rep("", nrow(data)))
  }
  parts <- lapply(data[columns], function(x) {
    ifelse(is.na(x), "N", paste0(
      "V", nchar(as.character(x)),
      ":", x
    ))
  })
  do.call(paste, c(parts, sep = "\035"))
}

groups <- function(data, columns, empty_total = TRUE) {
  keys <- row_keys(data, columns)
  if (nrow(data) == 0L) {
    if (length(columns) == 0L && empty_total) {
      return(list(integer()))
    }
    return(list())
  }
  lapply(unique(keys), function(key) {
    which(keys ==
            key)
  })
}

fixture <- function(width, variant = "base") {
  dimensions <- sprintf("d%02d", seq_len(width))
  data <- data.frame(fixed = c(
    rep("P", 6L),
    "Q", "Q"
  ), rid = seq_len(8L), value = c(
    2,
    5, 11, 17, 23, 31, 101, 211
  ), code = 2^(0:7))
  for (dimension in dimensions) data[[dimension]] <- "constant"
  positions <- unique(c(
    1L, ceiling(width / 2),
    width
  ))
  values <- list(c(
    "A", "A", "A", "B", NA, NA,
    "A", "A"
  ), c(
    "U", "U", "V", "U", "U",
    NA, "U", "U"
  ), c(
    "X", "Y", "X", "X", "X",
    NA, "X", "Y"
  ))
  for (i in seq_along(positions)) {
    data[[dimensions[[positions[[i]]]]]] <- values[[i]]
  }
  if (variant == "all_na") {
    data[dimensions] <- NA_character_
  }
  if (variant == "empty") {
    data <- data[FALSE, ]
  }
  if (variant == "missing_measure") {
    data$value <- NA_real_
  }
  if (variant == "duplicate_rows") {
    data <- rbind(data, data[1L, ])
  }
  data
}

families <- function(width) {
  ds <- sprintf("d%02d", seq_len(width))
  p <- unique(c(1L, ceiling(width / 2), width))
  sets <- c(
    list(ds), lapply(p, function(i) ds[-i]),
    lapply(p, function(i) ds[i]), list(character()),
    list(ds[seq_along(ds) %% 2L == 1L], ds[seq_along(ds) %% 2L ==
                                             0L])
  )
  a <- ds[[1L]]
  c <- ds[[width]]
  composite <- ds[-width]
  list(boundary = list(kind = "sets", terms = lapply(
    sets,
    function(s) node("set", s)
  )), duplicates = node(
    "sets",
    node("set", ds), node("set", ds), node(
      "set",
      a
    ), node("set"), node("set", ds),
    node("set")
  ), product_overlap = node(
    "product",
    node("rollup", a), node("rollup", a),
    node("set", setdiff(ds, a))
  ), composite_cube = node(
    "cube",
    node("set", composite), c
  ), composite_product = node(
    "product",
    node("rollup", node("set", composite)),
    node("rollup", c)
  ), composite_rollup = node(
    "rollup",
    node("set", composite), c
  ), repeated_rollup = node(
    "rollup",
    node("set", composite), c, c
  ), scalar_rollup = node(
    "rollup",
    ds
  ))
}

helper_columns <- function(plan) {
  ds <- plan$dimensions
  subset <- head(ds, 31L)
  list(
    gid = subset, reverse = rev(subset),
    selected = unique(ds[c(
      1L, ceiling(length(ds) / 2),
      length(ds)
    )]), fixed_middle = c(head(
      subset,
      1L
    ), plan$fixed, head(
      subset[-1L],
      29L
    ))
  )
}

oracle_summary <- function(
  data, plan, shares = FALSE,
  parent = FALSE, sql = FALSE
) {
  reference_sum <- function(x) {
    if (sql && all(is.na(x))) {
      NA_real_
    } else {
      sum(x, na.rm = TRUE)
    }
  }
  helpers <- helper_columns(plan)
  rows <- list()
  for (sid in seq_along(plan$sets)) {
    set <- plan$sets[[sid]]
    keys <- c(plan$fixed, set)
    for (slice in groups(data, keys)) {
      row <- data.frame(
        occurrence = as.integer(sid),
        n = length(slice), total = reference_sum(data$value[slice]),
        checksum = sum(data$code[slice])
      )
      for (key in c(plan$fixed, plan$dimensions)) {
        row[[key]] <- if (key %in% keys) {
          data[[key]][slice[[1L]]]
        } else {
          NA_character_
        }
      }
      for (dimension in plan$dimensions) {
        row[[paste0(
          "bit_",
          dimension
        )]] <- {
          as.integer(!dimension %in% set)
        }
      }
      row$fixed_bit <- 0L
      for (name in names(helpers)) {
        row[[name]] <- mask(
          helpers[[name]],
          keys
        )
      }
      rows[[length(rows) + 1L]] <- row
    }
  }
  if (!length(rows)) {
    proto <- oracle_summary(
      data[1L, , drop = FALSE],
      plan
    )
    out <- proto[FALSE, ]
  } else {
    out <- bind_rows(rows)
  }
  if (shares) {
    out$total_share <- numeric(nrow(out))
    if (parent) {
      out$parent_share <- numeric(nrow(out))
    }
    for (i in seq_len(nrow(out))) {
      sid <- out$occurrence[[i]]
      own <- plan$sets[[sid]]
      partition <- row_keys(
        out[i, , drop = FALSE],
        plan$fixed
      )
      source_rows <- which(row_keys(
        data,
        plan$fixed
      ) == partition)
      denominator <- reference_sum(data$value[source_rows])
      out$total_share[[i]] <- if (length(own) ==
                                    0L) {
        1
      } else {
        if (is.na(denominator) || denominator ==
              0) {
          NA_real_
        } else {
          out$total[[i]] / denominator
        }
      }
      if (parent) {
        later <- seq_along(plan$sets)
        candidates <- later[later > sid &
                              lengths(plan$sets) < length(own)]
        if (!length(own)) {
          out$parent_share[[i]] <- 1
        } else {
          target <- plan$sets[[candidates[[1L]]]]
          parent_keys <- c(
            plan$fixed,
            target
          )
          match_key <- row_keys(out[i, ,
                                  drop = FALSE
                                ], parent_keys)
          members <- which(row_keys(
            data,
            parent_keys
          ) == match_key)
          denominator <- reference_sum(data$value[members])
          out$parent_share[[i]] <- if (is.na(denominator) ||
                                         denominator == 0) {
            NA_real_
          } else {
            out$total[[i]] / denominator
          }
        }
      }
    }
  }
  out
}

oracle_expand <- function(data, plan, id = TRUE) {
  branches <- lapply(seq_along(plan$sets), function(sid) {
    out <- data
    for (dimension in setdiff(
      plan$dimensions,
      plan$sets[[sid]]
    )) {
      out[[dimension]] <- rep(
        NA_character_,
        nrow(out)
      )
    }
    if (id) {
      out$occurrence <- rep(
        as.integer(sid),
        nrow(out)
      )
    }
    out
  })
  bind_rows(branches)
}

# Canonical sorting preserves every row and
# every duplicate.
compare <- function(actual, expected, keys, local = FALSE) {
  actual <- as.data.frame(actual)
  expected <- as.data.frame(expected)
  stopifnot(
    setequal(names(actual), names(expected)),
    nrow(actual) == nrow(expected)
  )
  actual <- actual[names(expected)]
  ordering <- function(data) {
    order(row_keys(
      data,
      keys
    ), row_keys(data, names(data)))
  }
  actual <- actual[ordering(actual), , drop = FALSE]
  expected <- expected[ordering(expected), ,
    drop = FALSE
  ]
  rownames(actual) <- NULL
  rownames(expected) <- NULL
  for (name in names(expected)) {
    x <- actual[[name]]
    y <- expected[[name]]
    if (is.numeric(y)) {
      if (all(is.na(y)) && name %in% c(
        "n",
        "total", "checksum"
      )) {
        stopifnot(identical(
          is.na(x),
          is.na(y)
        ))
        next
      }
      stopifnot(is.numeric(x), identical(
        is.na(x),
        is.na(y)
      ))
      stopifnot(all(abs(x[!is.na(y)] - y[!is.na(y)]) <=
                      1e-12 * pmax(1, abs(y[!is.na(y)]))))
    } else {
      stopifnot(identical(x, y))
    }
  }
  if ("occurrence" %in% names(actual)) {
    stopifnot(is.integer(actual$occurrence))
  }
  if (local) {
    helper_pattern <- paste0("^(bit_|fixed_bit$|gid$|reverse$|",
                             "selected$|fixed_middle$)")
    for (name in grep(helper_pattern,
      names(actual),
      value = TRUE
    )) {
      stopifnot(is.integer(actual[[name]]))
    }
  }
  for (name in intersect(
    c("total_share", "parent_share"),
    names(actual)
  )) {
    stopifnot(is.double(actual[[name]]))
  }
  invisible(TRUE)
}

ledger <- new.env(parent = emptyenv())
ledger$results <- list()
record <- function(name, body) {
  result <- tryCatch(
    {
      force(body)
      "PASS"
    },
    error = function(e) {
      paste("FAIL", conditionMessage(e))
    }
  )
  ledger$results[[length(ledger$results) + 1L]] <- data.frame(
    case = name,
    result = result
  )
  cat(name, result, "\n")
  flush.console()
}

inspect_check <- function(input, ast, plan, policy) {
  actual <- inspect_grouping(input,
    .by = all_of(plan$fixed),
    .grouping = !!public_spec(ast), .duplicates = policy,
    .format = "list"
  )
  stopifnot(
    identical(actual$set_id, seq_along(plan$sets)),
    identical(actual$fixed, rep(
      list(plan$fixed),
      length(plan$sets)
    )), identical(
      actual$included,
      plan$sets
    )
  )
  for (i in seq_along(plan$sets)) {
    bits <- setNames(as.integer(!plan$dimensions %in%
                                  plan$sets[[i]]), plan$dimensions)
    stopifnot(identical(
      actual$grouping_bits[[i]],
      bits
    ), identical(
      actual$omitted[[i]],
      plan$dimensions[bits == 1L]
    ))
    expected <- if (length(bits) > 31L) {
      NA_integer_
    } else {
      mask(plan$dimensions, plan$sets[[i]])
    }
    stopifnot(identical(
      actual$grouping_id[[i]],
      expected
    ))
  }
}

summary_call <- function(
  input, ast, plan, policy,
  id, sort, shares, parent
) {
  dots <- rlang::quos(n = n(), total = sum(!!rlang::sym("value"),
                        na.rm = TRUE
                      ), checksum = sum(!!rlang::sym("code")))
  for (dimension in plan$dimensions) {
    dots[[paste0("bit_", dimension)]] <- rlang::new_quosure(rlang::call2(
      "grouping_bit",
      rlang::sym(dimension)
    ))
  }
  dots$fixed_bit <- rlang::quo(grouping_bit(!!rlang::sym("fixed")))
  for (name in names(helper_columns(plan))) {
    dots[[name]] <- rlang::new_quosure(rlang::call2(
      "grouping_id",
      !!!rlang::syms(helper_columns(plan)[[name]])
    ))
  }
  if (shares) {
    dots$total_share <- rlang::quo(share_of_total(!!rlang::sym("total")))
  }
  if (parent) {
    dots$parent_share <- rlang::quo(share_of_parent(!!rlang::sym("total")))
  }
  summarize_with_margins(input, !!!dots,
    .by = all_of(plan$fixed),
    .grouping = !!public_spec(ast), .duplicates = policy,
    .id = if (id) {
      "occurrence"
    } else {
      NULL
    }, .sort = sort, .margin_label = NULL,
    .check_share_source = FALSE
  )
}

fetch <- function(query, backend, compute = FALSE) {
  if (backend == "local") {
    return(query)
  }
  if (compute) {
    query <- dplyr::compute(query)
  }
  dplyr::collect(query)
}

run_case <- function(
  data, ast, backend, input,
  name, policy = "keep", shares = FALSE
) {
  plan <- reference_plan(ast, "fixed", policy)
  parent <- shares && ast$kind == "rollup"
  record(paste(name, "inspect"), inspect_check(
    input,
    ast, plan, policy
  ))
  expected <- oracle_summary(data, plan, shares,
    parent,
    sql = backend %in% c(
      "duckdb",
      "sqlite", "postgres"
    )
  )
  for (id in c(TRUE, FALSE)) {
    record(paste(name, "summary", id), {
      query <- summary_call(
        input, ast,
        plan, policy, id, "none", shares,
        parent
      )
      reference <- expected
      if (!id) {
        reference$occurrence <- NULL
      }
      actual <- fetch(query, backend)
      compare(actual, reference, c(
        plan$fixed,
        plan$dimensions, if (id) "occurrence"
      ),
      local = backend %in% c(
        "local",
        "dtplyr"
      )
      )
      if (backend %in% c(
        "duckdb", "sqlite",
        "postgres", "dtplyr"
      )) {
        compare(
          fetch(
            query, backend,
            TRUE
          ), reference, c(
            plan$fixed,
            plan$dimensions, if (id) "occurrence"
          ),
          local = backend == "dtplyr"
        )
      }
    })
    record(paste(name, "expand", id), {
      query <- expand_with_margins(input,
        .by = all_of("fixed"), .grouping = !!public_spec(ast),
        .duplicates = policy, .id = if (id) {
          "occurrence"
        } else {
          NULL
        }, .margin_label = NULL
      )
      reference <- oracle_expand(
        data, plan,
        id
      )
      compare(
        fetch(query, backend), reference,
        c("rid", if (id) "occurrence")
      )
      if (backend %in% c(
        "duckdb", "sqlite",
        "postgres", "dtplyr"
      )) {
        compare(fetch(
          query, backend,
          TRUE
        ), reference, c("rid", if (id) "occurrence"))
      }
    })
  }
}

ordinary_check <- function(
  input, data, plan,
  backend
) {
  expected <- oracle_summary(data, plan, sql = backend %in%
                               c("duckdb", "sqlite", "postgres"))
  for (sid in seq_along(plan$sets)) {
    keys <- c(plan$fixed, plan$sets[[sid]])
    query <- input |>
      group_by(across(all_of(keys))) |>
      summarise(
        n = n(), total = sum(!!rlang::sym("value"),
          na.rm = TRUE
        ), checksum = sum(!!rlang::sym("code")),
        .groups = "drop"
      )
    reference <- expected[
      expected$occurrence ==
        sid, c(keys, "n", "total", "checksum"),
      drop = FALSE
    ]
    compare(
      fetch(query, backend), reference,
      keys
    )
  }
}

order_check <- function(actual, plan, sort) {
  actual <- as.data.frame(actual)
  terms <- list()
  for (key in plan$fixed) {
    terms <- c(
      terms,
      list(is.na(actual[[key]]), actual[[key]])
    )
  }
  for (dimension in plan$dimensions) {
    bits <- vapply(actual$occurrence, function(sid) {
      as.integer(!dimension %in% plan$sets[[sid]])
    }, integer(1))
    if (sort == "first") {
      bits <- -bits
    }
    terms <- c(terms, list(
      bits, is.na(actual[[dimension]]),
      actual[[dimension]]
    ))
  }
  terms <- c(terms, list(actual$occurrence))
  ordering <- do.call(order, c(terms, list(na.last = TRUE)))
  keys <- c(plan$fixed, plan$dimensions, "occurrence")
  stopifnot(identical(
    row_keys(actual, keys),
    row_keys(
      actual[ordering, , drop = FALSE],
      keys
    )
  ))
}

nest_check <- function(
  input, data, ast, plan,
  backend, policy, keep, id, by
) {
  verb <- if (by) {
    nest_by_with_margins
  } else {
    nest_with_margins
  }
  query <- verb(input,
    .by = all_of("fixed"),
    .grouping = !!public_spec(ast), .duplicates = policy,
    .keep = keep, .id = if (id) {
      "occurrence"
    } else {
      NULL
    }, .margin_label = NULL
  )
  actual <- fetch(query, if (by) {
    "local"
  } else {
    backend
  })
  if (by) {
    stopifnot(inherits(actual, "rowwise_df"))
  }
  actual <- as.data.frame(actual)
  keys <- c(plan$fixed, plan$dimensions, if (id) "occurrence")
  payload <- if (keep) {
    names(data)
  } else {
    setdiff(names(data), c(plan$fixed, plan$dimensions))
  }
  signature <- function(row, cell) {
    stopifnot(is.data.frame(cell), setequal(
      names(cell),
      payload
    ), !"occurrence" %in% names(cell))
    cell_keys <- sort(row_keys(
      as.data.frame(cell),
      payload
    ))
    paste(row_keys(row, keys), paste(cell_keys,
            collapse = "\033"
          ), sep = "\032")
  }
  actual_sig <- vapply(
    seq_len(nrow(actual)),
    function(i) {
      signature(
        actual[i, , drop = FALSE],
        actual$data[[i]]
      )
    }, character(1)
  )
  expected_sig <- character()
  for (sid in seq_along(plan$sets)) {
    set <- plan$sets[[sid]]
    for (slice in groups(data, c(
      plan$fixed,
      set
    ), empty_total = FALSE)) {
      row <- data[slice[[1L]], c(
        plan$fixed,
        plan$dimensions
      ), drop = FALSE]
      for (dimension in setdiff(
        plan$dimensions,
        set
      )) {
        row[[dimension]] <- NA_character_
      }
      if (id) {
        row$occurrence <- as.integer(sid)
      }
      expected_sig <- c(expected_sig, signature(
        row,
        data[slice, payload, drop = FALSE]
      ))
    }
  }
  stopifnot(identical(sort(actual_sig), sort(expected_sig)))
  if (id) {
    stopifnot(is.integer(actual$occurrence))
  }
}

boundary_controls <- function(
  input, data, ast,
  plan, backend
) {
  width <- length(plan$dimensions)
  call_with <- function(columns) {
    expr <- rlang::call2("grouping_id", !!!rlang::syms(columns))
    summarize_with_margins(input,
      answer = !!expr,
      .by = all_of("fixed"), .grouping = !!public_spec(ast),
      .duplicates = "keep", .margin_label = NULL,
      .id = "occurrence"
    )
  }
  if (width > 31L) {
    error <- tryCatch(call_with(character()),
      error = identity
    )
    stopifnot(
      inherits(error, "marginplyr_error"),
      grepl("at most 31", conditionMessage(error),
        fixed = TRUE
      )
    )
  } else {
    result <- fetch(
      call_with(character()),
      backend
    )
    expected <- vapply(
      result$occurrence,
      function(sid) {
        mask(plan$dimensions, plan$sets[[sid]])
      }, integer(1)
    )
    stopifnot(is.numeric(result$answer), all(result$answer ==
                                               expected))
  }
  if (width >= 31L) {
    columns <- c("fixed", head(
      plan$dimensions,
      31L
    ))
    error <- tryCatch(call_with(columns),
      error = identity
    )
    stopifnot(
      inherits(error, "marginplyr_error"),
      grepl("at most 31", conditionMessage(error),
        fixed = TRUE
      )
    )
  }
}

mode <- Sys.getenv("IDENTITY_MODE", "pilot")
if (mode == "pilot") {
  a <- "d01"
  b <- "d02"
  c <- "d03"
  hand <- list(c(a, b, c), c(a, b), c, character())
  stopifnot(identical(model(node("cube", node(
    "set",
    a, b
  ), c)), hand))
  stopifnot(identical(model(node(
    "product",
    node("rollup", node("set", a, b)), node(
      "rollup",
      c
    )
  )), hand))
  stopifnot(identical(model(node("rollup", node(
    "set",
    a, b
  ), c, c)), list(c(a, b, c), c(
    a, b,
    c
  ), c(a, b), character())))
  data <- fixture(3L)
  ast <- families(3L)$duplicates
  plan <- reference_plan(ast, "fixed", "keep")
  expected <- oracle_summary(data, plan)
  corruptions <- list(bits = function(x) {
    old <- x$bit_d01
    x$bit_d01 <- x$bit_d03
    x$bit_d03 <- old
    x
  }, ids = function(x) {
    x$occurrence <- as.integer(ifelse(x$occurrence ==
                                        1L, 3L, ifelse(x$occurrence == 3L,
                                        1L, x$occurrence
                                      )))
    x
  }, duplicate = function(x) {
    x[x$occurrence !=
        2L, ]
  }, fixed = function(x) {
    x$fixed_bit <- rep(1L, nrow(x))
    x
  }, assignment = function(x) {
    x$total[1:2] <- rev(x$total[1:2])
    x
  })
  for (name in names(corruptions)) {
    record(paste("calibration", name), {
      bad <- corruptions[[name]](expected)
      rejected <- inherits(
        try(compare(
          bad,
          expected, c(
            "fixed", "occurrence",
            plan$dimensions
          )
        ), silent = TRUE),
        "try-error"
      )
      stopifnot(rejected)
    })
  }
  wide <- reference_plan(
    families(32L)$boundary,
    "fixed", "keep"
  )
  record("calibration NA pattern collapse", {
    expected_wide <- oracle_expand(
      fixture(32L),
      wide
    )
    bad <- expected_wide[expected_wide$occurrence ==
                           1L, ]
    stopifnot(inherits(try(
      compare(
        bad,
        expected_wide, c("rid", "occurrence")
      ),
      silent = TRUE
    ), "try-error"))
  })
  for (width in c(2L, 3L, 31L, 32L)) {
    data <- fixture(width)
    for (name in names(families(width))) {
      ast <- families(width)[[name]]
      run_case(data, ast, "local", data,
        paste("pilot", width, name),
        shares = name %in%
          c("composite_rollup", "repeated_rollup")
      )
    }
  }
}
if (mode != "pilot" && mode != "definitions") {
  backend <- mode
  con <- NULL
  if (backend == "duckdb") {
    con <- DBI::dbConnect(duckdb::duckdb(),
      dbdir = ":memory:"
    )
  }
  if (backend == "sqlite") {
    con <- DBI::dbConnect(
      RSQLite::SQLite(),
      ":memory:"
    )
  }
  if (backend == "postgres") {
    con <- DBI::dbConnect(RPostgres::Postgres(),
      host = "127.0.0.1", port = 55473L,
      dbname = "postgres"
    )
  }
  prepare <- function(data) {
    if (backend == "local") {
      return(data)
    }
    if (backend == "dtplyr") {
      return(dtplyr::lazy_dt(data, immutable = TRUE))
    }
    if (backend == "arrow") {
      return(arrow::arrow_table(data))
    }
    dplyr::copy_to(con, data, "identity_input",
      overwrite = TRUE, temporary = TRUE
    )
  }
  widths <- c(3L, 30L, 31L, 32L, 33L)
  for (width in widths) {
    data <- fixture(width)
    input <- prepare(data)
    specs <- families(width)
    for (name in names(specs)) {
      ast <- specs[[name]]
      run_case(
        data, ast, backend, input,
        paste(backend, width, name)
      )
    }
    ast <- specs$boundary
    plan <- reference_plan(ast, "fixed", "keep")
    record(
      paste(backend, width, "ordinary"),
      ordinary_check(
        input, data, plan,
        backend
      )
    )
    record(
      paste(backend, width, "cap controls"),
      boundary_controls(
        input, data, ast,
        plan, backend
      )
    )
    for (name in c(
      "duplicates", "product_overlap",
      "repeated_rollup"
    )) {
      ast <- specs[[name]]
      run_case(data, ast, backend, input,
        paste(backend, width, name, "drop"),
        policy = "drop"
      )
      record(paste(
        backend, width, name,
        "error"
      ), {
        error <- tryCatch(
          inspect_grouping(input,
            .by = all_of("fixed"), .grouping = !!public_spec(ast)
          ),
          error = identity
        )
        stopifnot(
          inherits(error, "marginplyr_error"),
          grepl("Duplicate grouping sets",
            conditionMessage(error),
            fixed = TRUE
          )
        )
      })
    }
    if (backend != "arrow") {
      for (name in c(
        "boundary", "composite_rollup",
        "repeated_rollup"
      )) {
        run_case(data, specs[[name]],
          backend, input, paste(
            backend,
            width, name, "shares"
          ),
          shares = TRUE
        )
      }
    }
    for (sort in c("first", "last")) {
      for (name in c(
        "boundary",
        "composite_cube", "duplicates"
      )) {
        ast <- specs[[name]]
        plan <- reference_plan(
          ast, "fixed",
          "keep"
        )
        record(paste(
          backend, width, name,
          sort
        ), {
          query <- summary_call(
            input, ast,
            plan, "keep", TRUE, sort, FALSE,
            FALSE
          )
          result <- fetch(query, backend)
          compare(result, oracle_summary(
            data,
            plan
          ), c(
            "fixed", plan$dimensions,
            "occurrence"
          ), local = backend %in%
            c("local", "dtplyr"))
          order_check(result, plan, sort)
          if (backend %in% c(
            "duckdb", "sqlite",
            "postgres", "dtplyr"
          )) {
            materialized <- fetch(
              query,
              backend, TRUE
            )
            compare(
              materialized, result,
              c(
                "fixed", plan$dimensions,
                "occurrence"
              )
            )
            order_check(
              materialized, plan,
              sort
            )
          }
          query <- expand_with_margins(input,
            .by = all_of("fixed"), .grouping = !!public_spec(ast),
            .duplicates = "keep", .id = "occurrence",
            .sort = sort, .margin_label = NULL
          )
          result <- fetch(query, backend)
          compare(result, oracle_expand(
            data,
            plan
          ), c("rid", "occurrence"))
          order_check(result, plan, sort)
          if (backend %in% c(
            "duckdb", "sqlite",
            "postgres", "dtplyr"
          )) {
            materialized <- fetch(
              query,
              backend, TRUE
            )
            compare(
              materialized, result,
              c("rid", "occurrence")
            )
            order_check(
              materialized, plan,
              sort
            )
          }
        })
      }
    }
    if (backend %in% c("local", "dtplyr")) {
      for (name in c(
        "boundary", "composite_rollup",
        "duplicates"
      )) {
        ast <- specs[[name]]
        plan <- reference_plan(
          ast, "fixed",
          "drop"
        )
        for (keep in c(TRUE, FALSE)) {
          for (id in c(
            TRUE,
            FALSE
          )) {
            for (by in c(TRUE, FALSE)) {
              record(
                paste(
                  backend, width,
                  name, "nest", keep, id, by
                ),
                nest_check(
                  input, data, ast,
                  plan, backend, "drop", keep,
                  id, by
                )
              )
            }
          }
        }
      }
    }
  }
  for (variant in c(
    "duplicate_rows", "all_na",
    "empty", "missing_measure"
  )) {
    for (width in c(3L, 32L)) {
      data <- fixture(width, variant)
      input <- prepare(data)
      for (name in c(
        "boundary", "duplicates",
        "repeated_rollup"
      )) {
        ast <- families(width)[[name]]
        run_case(data, ast, backend, input,
          paste(
            backend, width, variant,
            name
          ),
          shares = backend !=
            "arrow"
        )
        plan <- reference_plan(
          ast, "fixed",
          "keep"
        )
        record(paste(
          backend, width, variant,
          name, "ordinary"
        ), ordinary_check(
          input,
          data, plan, backend
        ))
      }
    }
  }
  if (!is.null(con)) {
    DBI::dbDisconnect(con)
  }
}
if (mode != "definitions") {
  write.csv(bind_rows(ledger$results), file.path(
    task,
    paste0(mode, "-results.csv")
  ), row.names = FALSE)
  stopifnot(all(bind_rows(ledger$results)$result ==
                  "PASS"))
}
```

## 実行コード: followup.R — 列順・公開経路・広い symbolic plan の追試

```r
backend <- Sys.getenv("IDENTITY_BACKEND", "local")
Sys.setenv(IDENTITY_MODE = "definitions")
source(file.path(
  Sys.getenv("IDENTITY_CODE_DIR", Sys.getenv("IDENTITY_TASK")),
  "hunt.R"
))
con <- NULL
if (backend == "duckdb") {
  con <- DBI::dbConnect(duckdb::duckdb(),
    dbdir = ":memory:"
  )
}
if (backend == "sqlite") {
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    ":memory:"
  )
}
if (backend == "postgres") {
  con <- DBI::dbConnect(RPostgres::Postgres(),
    host = "127.0.0.1", port = 55473L, dbname = "postgres"
  )
}
prepare <- function(data) {
  if (backend == "local") {
    return(data)
  }
  if (backend == "dtplyr") {
    return(dtplyr::lazy_dt(data, immutable = TRUE))
  }
  if (backend == "arrow") {
    return(arrow::arrow_table(data))
  }
  dplyr::copy_to(con, data, "followup_input",
    overwrite = TRUE, temporary = TRUE
  )
}

for (width in c(3L, 31L, 32L, 33L)) {
  data <- fixture(width)
  ds <- sprintf("d%02d", seq_len(width))
  original <- families(width)$boundary
  variants <- list(total_first = list(
    kind = "sets",
    terms = rev(original$terms)
  ), reverse_columns = list(
    kind = "sets",
    terms = lapply(model(original), function(s) {
      node(
        "set",
        rev(s)
      )
    })
  ), nested_union = node(
    "sets",
    node("sets", node("set", ds), node(
      "set",
      ds[-1L]
    )), node("sets", node(
      "set",
      ds[[width]]
    ), node("set"))
  ))
  for (name in names(variants)) {
    input <- prepare(data[rev(names(data))])
    run_case(
      data, variants[[name]], backend,
      input, paste(
        "followup", backend,
        width, name
      )
    )
  }
  record(paste("followup", backend, width, "fixed vs variable"), {
    input <- prepare(data)
    fixed_query <- summarize_with_margins(input,
      total = sum(!!rlang::sym("value"),
        na.rm = TRUE
      ), fixed_bit = grouping_bit(!!rlang::sym("fixed")),
      .by = all_of("fixed"), .grouping = !!public_spec(original),
      .duplicates = "keep", .id = "occurrence",
      .margin_label = NULL
    )
    variable_spec <- node("product", node(
      "set",
      "fixed"
    ), original)
    variable_query <- summarize_with_margins(input,
      total = sum(!!rlang::sym("value"),
        na.rm = TRUE
      ), fixed_bit = grouping_bit(!!rlang::sym("fixed")),
      .grouping = !!public_spec(variable_spec),
      .duplicates = "keep", .id = "occurrence",
      .margin_label = NULL
    )
    compare(
      fetch(variable_query, backend),
      fetch(fixed_query, backend), c(
        "fixed",
        ds, "occurrence"
      )
    )
    plan <- inspect_grouping(input,
      .grouping = !!public_spec(variable_spec),
      .duplicates = "keep", .format = "list"
    )
    stopifnot(all(lengths(plan$fixed) ==
                    0L), all(vapply(
                plan$grouping_bits,
                function(bits) {
                  bits[["fixed"]] ==
                    0L
                }, logical(1)
              )))
    if (width == 31L) {
      fixed_result <- summarize_with_margins(input,
        gid = grouping_id(), .by = all_of("fixed"),
        .grouping = !!public_spec(original),
        .duplicates = "keep"
      )
      stopifnot(is.numeric(fetch(
        fixed_result,
        backend
      )$gid))
      error <- tryCatch(summarize_with_margins(input,
                          gid = grouping_id(),
                          .grouping = !!public_spec(variable_spec),
                          .duplicates = "keep"
                        ), error = identity)
      stopifnot(
        inherits(error, "marginplyr_error"),
        grepl("at most 31", conditionMessage(error),
          fixed = TRUE
        )
      )
    }
  })
  if (width %in% c(31L, 32L)) {
    for (variant in c("empty", "all_na")) {
      data <- fixture(width, variant)
      input <- prepare(data)
      plan <- reference_plan(
        original, "fixed",
        "keep"
      )
      for (sort in c("first", "last")) {
        record(paste(
          "followup",
          backend, width, variant, sort
        ), {
          query <- summary_call(
            input,
            original, plan, "keep", TRUE,
            sort, FALSE, FALSE
          )
          expected <- oracle_summary(
            data,
            plan
          )
          actual <- fetch(query, backend)
          compare(actual, expected, c(
            "fixed",
            ds, "occurrence"
          ), local = backend %in%
            c("local", "dtplyr"))
          order_check(actual, plan, sort)
          if (backend %in% c(
            "duckdb",
            "sqlite", "postgres", "dtplyr"
          )) {
            materialized <- fetch(
              query,
              backend, TRUE
            )
            compare(materialized, expected,
              c("fixed", ds, "occurrence"),
              local = backend == "dtplyr"
            )
            order_check(
              materialized,
              plan, sort
            )
          }
        })
      }
    }
  }
}

# Wider symbolic plans have few sets and
# never request a full numeric mask.
for (width in c(64L, 65L)) {
  data <- fixture(width)
  ds <- sprintf("d%02d", seq_len(width))
  ast <- node("sets", node("set", ds), node(
    "set",
    ds[-1L]
  ), node("set", ds[-width]), node("set"))
  input <- prepare(data)
  run_case(data, ast, backend, input, paste(
    "followup",
    backend, width, "symbolic"
  ))
}

record(paste("followup", backend, "by only"), {
  data <- fixture(3L)
  input <- prepare(data)
  query <- summarize_with_margins(input,
    total = sum(!!rlang::sym("value"), na.rm = TRUE),
    bit = grouping_bit(!!rlang::sym("fixed")),
    gid = grouping_id(), .by = all_of("fixed"),
    .id = "occurrence"
  )
  expected <- data.frame(fixed = c(
    "P",
    "Q"
  ), total = c(89, 312), bit = c(
    0L,
    0L
  ), gid = c(0L, 0L), occurrence = c(
    1L,
    1L
  ))
  compare(
    fetch(query, backend), expected,
    "fixed"
  )
})

record(paste("followup", backend, "empty grand total"), {
  data <- fixture(3L, "empty")
  input <- prepare(data)
  ds <- c("d01", "d02", "d03")
  dots <- rlang::quos(
    n = n(), total = sum(!!rlang::sym("value"),
      na.rm = TRUE
    ), bit = grouping_bit(d01),
    gid = grouping_id()
  )
  if (backend != "arrow") {
    dots$parent_share <- rlang::quo(share_of_parent(!!rlang::sym("total")))
    dots$total_share <- rlang::quo(share_of_total(!!rlang::sym("total")))
  }
  query <- summarize_with_margins(input,
    !!!dots,
    .grouping = rollup(all_of(ds)),
    .id = "occurrence", .margin_label = NULL,
    .check_share_source = FALSE
  )
  actual <- fetch(query, backend)
  ordinary <- fetch(
    summarise(input,
      n = n(),
      total = sum(!!rlang::sym("value"), na.rm = TRUE)
    ),
    backend
  )
  stopifnot(nrow(actual) == 1L, actual$occurrence ==
              4L, actual$bit == 1L, actual$gid ==
              7L)
  compare(
    as.data.frame(actual)[c("n", "total")],
    ordinary, character()
  )
  if (backend != "arrow") {
    stopifnot(
      actual$parent_share == 1,
      actual$total_share == 1
    )
  }
})

if (backend %in% c("duckdb", "postgres")) {
  record(paste(
    "followup",
    backend, "native grouping SQL"
  ), {
    data <- fixture(31L)
    input <- prepare(data)
    ast <- families(31L)$boundary
    plan <- reference_plan(ast, "fixed", "drop")
    native <- summary_call(
      input, ast, plan, "drop",
      TRUE, "none", FALSE, FALSE
    )
    portable <- summary_call(
      input, ast, plan,
      "keep", TRUE, "none", FALSE, FALSE
    )
    native_sql <- as.character(dbplyr::sql_render(native))
    portable_sql <- as.character(dbplyr::sql_render(portable))
    stopifnot(grepl("GROUP BY GROUPING SETS",
                native_sql,
                fixed = TRUE
              ), grepl("UNION ALL",
                portable_sql,
                fixed = TRUE
              ))
    native_result <- fetch(native, backend)
    compare(
      native_result, fetch(portable, backend),
      c("fixed", plan$dimensions, "occurrence")
    )
    quoted <- function(columns) {
      paste(DBI::dbQuoteIdentifier(
        con,
        columns
      ), collapse = ", ")
    }
    grouping_sets_sql <- paste(vapply(
      plan$sets,
      function(set) {
        paste0(
          "(", quoted(c("fixed", set)),
          ")"
        )
      }, character(1)
    ), collapse = ", ")
    terms <- vapply(
      seq_along(plan$dimensions),
      function(i) {
        paste0(
          "GROUPING(", quoted(plan$dimensions[[i]]),
          ") * ", format(2^(31L - i), scientific = FALSE)
        )
      }, character(1)
    )
    sql <- paste0(
      "SELECT ", quoted(c(
        "fixed",
        plan$dimensions
      )), ", SUM(value) AS total, ",
      paste(terms, collapse = " + "),
      " AS gid FROM followup_input ",
      "GROUP BY GROUPING SETS (",
      grouping_sets_sql, ")"
    )
    independent <- DBI::dbGetQuery(con, sql)
    compare(independent, as.data.frame(native_result)[c(
      "fixed",
      plan$dimensions, "total", "gid"
    )], c(
      "fixed",
      plan$dimensions, "gid"
    ))
    writeLines(
      c(native_sql, portable_sql, sql),
      file.path(task, paste0(backend, "-route-control.sql"))
    )
  })
}

if (!is.null(con)) {
  record(paste("followup", backend, "construction audit"), {
    data <- fixture(32L)
    input <- prepare(data)
    options(marginplyr.audit_sql = TRUE)
    ast <- families(32L)$boundary
    plan <- reference_plan(ast, "fixed", "drop")
    inspected <- inspect_grouping(
      input, .by = all_of("fixed"), .grouping = !!public_spec(ast),
      .duplicates = "drop"
    )
    inspection_audit <- last_sent_queries()
    query <- summary_call(
      input, ast, plan, "drop", TRUE, "none", TRUE, FALSE
    )
    audit <- last_sent_queries()
    stopifnot(nrow(inspected) == length(plan$sets))
    stopifnot(sum(audit$purpose == "result") == 1L)
    for (sql in audit$sql[audit$purpose != "result"]) {
      stopifnot(grepl("LIMIT 0", sql, fixed = TRUE) ||
                  !grepl("followup_input", sql, fixed = TRUE))
    }
    collected <- fetch(query, backend)
    stopifnot(identical(last_sent_queries(), audit))
    materialized <- fetch(query, backend, TRUE)
    compare(materialized, collected, c("fixed", plan$dimensions, "occurrence"))
    stopifnot(identical(last_sent_queries(), audit))
    write.csv(
      bind_rows(
        mutate(inspection_audit, stage = "inspection"),
        mutate(audit, stage = "summary")
      ), file.path(task, paste0(backend, "-construction-audit.csv")),
      row.names = FALSE
    )
  })
  DBI::dbDisconnect(con)
}
write.csv(bind_rows(ledger$results), file.path(
  task,
  paste0("followup-", backend, "-results.csv")
),
row.names = FALSE
)
stopifnot(all(bind_rows(ledger$results)$result == "PASS"))
```

## 実行コード: closing.R — 最終見直しから追加した nest・型・列位置・環境確認

```r
backend <- Sys.getenv("IDENTITY_BACKEND", "local")
Sys.setenv(IDENTITY_MODE = "definitions")
source(file.path(Sys.getenv("IDENTITY_CODE_DIR"), "hunt.R"))
con <- NULL
if (backend == "duckdb") {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
}
if (backend == "sqlite") {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
}
if (backend == "postgres") {
  con <- DBI::dbConnect(
    RPostgres::Postgres(), host = "127.0.0.1", port = 55473L,
    dbname = "postgres"
  )
}
prepare <- function(data) {
  if (backend == "local") return(data)
  if (backend == "dtplyr") return(dtplyr::lazy_dt(data, immutable = TRUE))
  if (backend == "arrow") return(arrow::arrow_table(data))
  dplyr::copy_to(con, data, "closing_input", overwrite = TRUE, temporary = TRUE)
}

if (backend %in% c("local", "dtplyr")) {
  for (width in c(31L, 32L, 65L)) {
    for (variant in c("all_na", "empty", "duplicate_rows")) {
      data <- fixture(width, variant)
      input <- prepare(data)
      ast <- families(width)$duplicates
      plan <- reference_plan(ast, "fixed", "drop")
      for (keep in c(TRUE, FALSE)) {
        for (id in c(TRUE, FALSE)) {
          for (by in c(TRUE, FALSE)) {
            record(paste(backend, width, variant, "nest", keep, id, by), {
              nest_check(
                input, data, ast, plan, backend, "drop", keep, id, by
              )
            })
          }
        }
      }
    }
  }
}

record(paste(backend, "fixed numeric display"), {
  data <- fixture(3L)
  data$fixed <- match(data$fixed, c("P", "Q"))
  input <- prepare(data)
  ast <- families(3L)$boundary
  variable_spec <- node("product", node("set", "fixed"), ast)
  fixed <- summarize_with_margins(
    input, total = sum(!!rlang::sym("value"), na.rm = TRUE),
    .by = all_of("fixed"), .grouping = !!public_spec(ast),
    .duplicates = "keep", .id = "occurrence"
  )
  variable <- summarize_with_margins(
    input, total = sum(!!rlang::sym("value"), na.rm = TRUE),
    .grouping = !!public_spec(variable_spec),
    .duplicates = "keep", .id = "occurrence"
  )
  x <- fetch(fixed, backend)
  y <- fetch(variable, backend)
  stopifnot(is.numeric(x$fixed), is.character(y$fixed))
  x$fixed <- as.character(x$fixed)
  compare(y, x, c("fixed", "d01", "d02", "d03", "occurrence"))
})

record(paste(backend, "rotated positions"), {
  data <- fixture(31L)
  ds <- sprintf("d%02d", 1:31)
  rotated <- c(ds[-1L], ds[[1L]])
  names(data)[match(ds, names(data))] <- rotated
  input <- prepare(data)
  ast <- families(31L)$boundary
  run_case(data, ast, backend, input, paste(backend, "rotation boundary"))
})

# This records actual loaded paths, including lazy-loaded backend namespaces.
loaded <- setdiff(loadedNamespaces(), "base")
environment <- data.frame(
  package = loaded,
  version = vapply(loaded, function(package) {
    as.character(utils::packageVersion(package))
  }, character(1)),
  path = vapply(loaded, function(package) {
    getNamespaceInfo(asNamespace(package), "path")
  }, character(1))
)
outside <- !startsWith(
  normalizePath(environment$path), normalizePath(file.path(task, "library"))
) & !startsWith(normalizePath(environment$path), normalizePath(.Library))
stopifnot(!any(outside))
write.csv(
  environment, file.path(task, paste0(backend, "-loaded.csv")),
  row.names = FALSE
)
if (!is.null(con)) DBI::dbDisconnect(con)
write.csv(
  bind_rows(ledger$results),
  file.path(task, paste0("closing-", backend, "-results.csv")),
  row.names = FALSE
)
stopifnot(all(bind_rows(ledger$results)$result == "PASS"))
```
