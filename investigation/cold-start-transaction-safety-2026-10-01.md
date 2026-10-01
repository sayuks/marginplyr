# Cold-start share判定とcaller transactionの安全性

Investigated: 2026-10-01
Target HEAD: 8b57a86a9846fb92053eb1ae1990219d4f294944
Outcome: 正常なPostgreSQL caller transactionを初回share判定が中断する契約違反を確認。
Issue: https://github.com/sayuks/marginplyr/issues/774

## 結果と判断

正常なnumeric sourceの公開share呼び出しが、初回利用の履歴だけで
成功から構築時の拒否へ変わった。Cold／autocommitでは構築・collectが成功したが、
Cold／caller transactionでは内部probeがtransactionを中断した。
同じ接続または別接続をautocommitで事前利用したWarmでは同じ操作が成功した。

失敗直後、被検証接続へ観測SQLを送る前に、独立した監視接続が
idle in transaction (aborted) を確認した。続く安全なSELECTも失敗した。
callerがcommitを試みた場合、DBIの戻り値はTRUEだったが、呼び出し前のmarker変更は
永続化されなかった。packageが明示的に全体rollbackしたという結果ではない。
内部SQLのエラーでcommitできない状態となり、PostgreSQL側の終了動作で
以前の作業も破棄された。

拒否、後続SELECT失敗、commitability喪失は同じ原因の症状として
[Issue #774](https://github.com/sayuks/marginplyr/issues/774)へまとめた。
優先度はhigh、bug／ready-for-agent、blockerはなしとした。
Issue分割・投稿判断はユーザーの事前委任に基づき行い、個別承認されたとは扱わなかった。
savepoint追加、キャッシュ変更、一般的接続復旧を修正方式として決めなかった。

製品コード、恒久tests、公開契約、依存宣言、CI、過去の調査資料は変更しなかった。
専用クラスタ、隔離ライブラリ、観測JSON／CSV／ログ／一時スクリプトは
リポジトリ外に置いた。このノートが今回の唯一のリポジトリ追加成果物である。

## 対象・固定環境

開始時の作業ツリーに差分はなかった。固定HEADをgit archiveで展開し、
そのソースをprivate libraryへR CMD INSTALLした。
source archive SHA-256:
11826640978c3a9ab5d270c19d7f3fc5c8e44866977ef68930fa89a78345cf9a

OSはDarwin 25.6.0 arm64、Rは4.6.1 (2026-06-24)、
marginplyrは0.1.0。実サーバーはPostgreSQL 17.11 (Homebrew)、
aarch64-apple-darwin25.6.0、Apple clang 21.0.0だった。
DBI::dbGetInfo(RPostgres::Postgres())が示したclient/libpq版は16.8であり、
サーバー版とは区別した。RPostgres共有ライブラリのSHA-256:
db5759bab8f3dc9b291dc17f636fdb24d22339b18b8fa9aebac7f529942cd57c

実験準備では通常libraryから依存物の版確認とread-onlyコピーを行い、
そこへinstall／updateしなかった。公開用package-aware lintは通常の開発依存環境で行った。
workerはvanillaの新しいRプロセスとprivate library＋R base libraryのみを使用した。
主系列はロード済みnamespaceの版・パスを記録し、通常libraryが混入しないとassertした。
依存物は途中で更新しなかった。主系列のbigint mappingはnumeric、
最小再現の追加対照では標準mappingのinteger64を使った。

専用PostgreSQLは外部TCP listenerを持たず、一時ディレクトリのUnix socketだけで
接続した。hunt_adminがfixture／権限を準備し、hunt_userが主操作を実行した。
hunt_readerには対象テーブルのSELECTのみを与えた。
監視は別接続のhunt_adminで行い、被検証接続とtransactionを共有しなかった。
業務DB、既存Rプロセスには接続しなかった。

コピーした非base依存物の版は次のとおり。baseのmethods／graphics／stats／utils／
grDevices／toolsはR 4.6.1に付属した。

| Package | Version |
|---|---|
| DBI | 1.3.0 |
| RPostgres | 1.4.10 |
| bit64 | 4.8.6 |
| bit | 4.6.0 |
| blob | 1.3.0 |
| rlang | 1.3.0 |
| vctrs | 0.7.3 |
| cli | 3.6.6 |
| glue | 1.8.1 |
| lifecycle | 1.0.5 |
| hms | 1.1.4 |
| pkgconfig | 2.0.3 |
| lubridate | 1.9.5 |
| generics | 0.1.4 |
| timechange | 0.4.0 |
| cpp11 | 0.5.5 |
| withr | 3.0.3 |
| dplyr | 1.2.1 |
| magrittr | 2.0.5 |
| pillar | 1.11.1 |
| utf8 | 1.2.6 |
| R6 | 2.6.1 |
| tibble | 3.3.1 |
| tidyselect | 1.2.1 |
| dbplyr | 2.6.0 |
| purrr | 1.2.2 |
| tidyr | 1.3.2 |
| stringr | 1.6.0 |
| stringi | 1.8.9 |
| duckdb | 1.5.5 |
| RSQLite | 3.53.3 |
| memoise | 2.0.1 |
| cachem | 1.1.0 |
| fastmap | 1.2.0 |
| jsonlite | 2.0.0 |

## 経路・契約・既存証拠との差分

読んだ現行経路は、公開summarize_with_margins()からexecute_margin_summary()、
execute_shares()、check_share_sources()、check_dialect_share_sources()、
share_dialect_verdict()、probe_share_dialect()、probe_share_dialect_answer()、
dplyr::collect()、dbplyr／DBI／RPostgres、PostgreSQLへ至るものだった。
share helpersを受け取る他の独立公開入口は見つからなかった。

[share module](../R/share.R)はquery構築失敗をunanswerable、
collect失敗をraisedへ変換した。文字列SUMがraisedならcontrolを送り、
controlが答えなければunknownとして公開呼び出しを拒否した。
内部condition自体はその捕捉で保持されなかった。
公開呼び出し元のerror wrapperはPackage conditionのcallを調整して再送出するが、
DB transactionの復旧は行わなかった。

キャッシュキーは方言クラス列で、この実接続では
sql_dialect_postgresとsql_dialectの組だった。
namespace内の環境で同じRプロセスの接続間に共有された。
refuses／convertsだけが保持され、unknownは保持されなかった。
接続Aを閉じても接続Bは測定済みのrefusesを再利用した。

dbplyr 2.6.0のdb_collect.DBIConnectionはdbSendQuery→dbFetchと
on.exitのdbClearResultを持った。
[RPostgres 1.4.10 source](https://github.com/r-dbi/RPostgres/tree/v1.4.10)の
dbSendQuery_PqConnection、PqResultImpl、DbResult、DbConnectionを読んだ。
prepareで失敗し、result cleanupはqueryの解放／drainを扱ったが、
このprobeの周囲にsavepoint／transaction復旧は見つからなかった。
DBI transaction方法の使用は別の明示的処理であり、result cleanupと同一視しなかった。
RPostgresのtransaction trackingは独自のフラグで、
接続validityやそのフラグをDB側の健全性の証明には使わなかった。

根拠は[ADR 0010](../design/adr/0010-compute-parent-shares-as-a-contextual-summary.md)、
[ADR 0014](../design/adr/0014-select-parent-share-adapters-from-prepared-backend-kind.md)、
[ADR 0017](../design/adr/0017-calculate-total-shares-against-the-grand-total-set.md)、
[ADR 0020](../design/adr/0020-ask-before-reading-a-lazy-input.md)、
[ADR 0027](../design/adr/0027-record-the-sql-marginplyr-sends.md)、
[公開share説明](../R/share.R)、[CONTEXT](../CONTEXT.md)である。
正常なnumeric sourceのshare受理とlazy構築を期待し、
内部の判定SQLをcallerのtransaction中断要求とは扱わなかった。
ADR 0020が許すtable-free queryと、その副作用の安全性は区別した。
[ADR 0031](../design/adr/0031-preserve-sqlite-typed-dimensions-under-margin-order.md)の
SQLite compute保証をPostgreSQL構築の一般的復旧保証へ拡張しなかった。
失敗一般・既に壊れたtransactionを自動修復する保証を新設した判断ではない。

[share-backends tests](../tests/testthat/test-share-backends.R)はmockで
測定結果、driver mapping、unknown再試行、キャッシュ再利用を扱った。
[query-policy tests](../tests/testthat/test-query-policy.R)は実行入口と
補助処理の契約を可視化するが、この実DB状態遷移を検証しなかった。
[Sent query tests](../tests/testthat/test-sent-queries.R)のCold再現は
テスト用cache初期化を使い、今回の新規プロセス＋caller transactionとは異なった。

[実PostgreSQL調査](2026-09-30-postgres-near-floor.md)は既定checkでの値と
bigint mappingを確認したが、Cold判定をcaller transaction内で行わなかった。
[例外安全性調査](exception-safety-recovery-2026-09-30.md)のshare control失敗は
DuckDBでの制御された失敗であり、自然発生した今回のエラーとは異なった。
[セッション状態調査](session-state-and-last-call-accessors.md)は
package-owned environmentとlast-call accessorの証拠で、
DB transaction復旧の証拠ではなかった。
[直前のGrouping plan調査](grouping-plan-identity-boundaries-2026-10-01.md)は
share主検証に.check_share_source = FALSEを指定したため、この既定経路を通らなかった。

[閉じた#198](https://github.com/sayuks/marginplyr/issues/198)はunknown非保持、
[閉じた#440](https://github.com/sayuks/marginplyr/issues/440)と
[merged #445](https://github.com/sayuks/marginplyr/pull/445)はbigint controlの
成功値読み取り修正だった。今回はcontrolそのものが25P02で失敗した。
[閉じた#745](https://github.com/sayuks/marginplyr/issues/745)は
実PostgreSQLの依存下限対照である。open Issueと既存関連Issue／PRを確認し、
同じtransaction境界の未解決Issueは見つからなかった。

## fixtureと独立oracle

6行のfixtureは次のとおり。キーはtext、値はdouble precision、
正常な正の有限値であり、型不適格・NULL・ゼロ分母を含まなかった。

| period | region | store | value |
|---|---|---|---:|
| P | N | a | 2 |
| P | N | a | 4 |
| P | N | b | 6 |
| P | S | c | 8 |
| Q | N | a | 3 |
| Q | S | c | 9 |

固定キーperiod内でrollup(region, store)、count／sum、Parent／Total shares、
set_idを要求した。主条件では.check_share_sourceを省略した。
通常SQL、通常dbplyr、shareなしMarginを独立した新規workerで比較した。
uncheckedは型の適格性を独立確認した原因切り分け対照であり、修正方針ではない。

手計算oracleのPは詳細(n,amount)=(2,6),(1,6),(1,8)、
地域=(3,12),(1,8)、Grand total=(4,20)。
Qは詳細／地域=(1,3),(1,9)、Grand total=(2,12)。
計画の「12行」は手計算の誤りで、正しい結果はP 6行＋Q 5行＝11行だった。
最終コードはこの11行を定数で定義した。
shareは対応する親／固定partitionのGrand total定数から計算し、
比較は1e-12、share列がdoubleであることもassertした。
通常summaryのdriver classにはshare列の型保証を拡張しなかった。

## 観測と状態遷移

各ケースは独立したvanilla Rプロセスだった。fixture／oracle準備でMargin shareを使わず、
namespace cacheを変更しなかった。Warmは公開shareの構築・collect成功から作った。
初回の結果記録と本呼び出しの記録は分けた。

主系列はbefore_transaction、before_margin、after_construction、
after_collect（構築成功時のみ）、after_caller_finishを保存した。
各段階で最初に監視接続が対象PIDのstate／xact_start／queryを読み、
別接続のmarkerを確認した。その後に被検証接続でSELECT 1、設定、source、
sentinel、markerを個別に観測した。失敗した観測は主conditionを上書きしなかった。
監視接続はautocommitで、古い統計snapshotを保持しなかった。

読み書き系列はcallerが先にmarkerを0→1へ変更した。
終了前の別接続は0、正常な同じ接続は1だった。
commit後は1、rollback後は0が正常対照だった。
Cold失敗では同じ接続のmarker読取は不能であり、この時点で「消失」と推定しなかった。
caller終了後の別接続の値が0であることを独立に観測した。
READ ONLYでは書き込まず、dbBegin直後、初回SELECTより前に
SET TRANSACTION READ ONLYした。禁止されたmaterializationを要求しなかった。

主系列50ケースのうち13ケースが構築で拒否され、すべてCold＋caller transactionの
既定share判定だった。他の37ケースは構築・collectと値のassertionに成功した。
件数は採用したケースの記録で、実行上限・停止条件ではなかった。
初期探索と同一コード再実行を含む全プロセス起動数をこの50と同一視しない。

| 系列 | 構築 | collect | 直後のDB状態 | 終了前のsafe SELECT | caller終了後marker |
|---|---|---|---|---|---|
| Cold／autocommit | 成功 | 成功 | idle | 成功 | 0 |
| Cold／read-write | 拒否 | 未実行 | aborted | 失敗 | commit試行／rollbackとも0 |
| Warm／同接続 read-write | 成功 | 成功 | 正常なtransaction | 成功 | commit=1／rollback=0 |
| Warm／別接続 read-write | 成功 | 成功 | 正常なtransaction | 成功 | commit=1／rollback=0 |
| Cold／READ ONLY | 拒否 | 未実行 | aborted | 失敗 | 変更なし |
| Warm／READ ONLY | 成功 | 成功 | 正常なtransaction | 成功 | 変更なし |
| 通常SQL／dbplyr／shareなし／unchecked | 成功 | 成功 | 正常 | 成功 | callerの選択どおり |
| Cold＋既存caller savepoint | 拒否 | 未実行 | aborted | 失敗 | 明示的rollback-to後のcommitで1 |

既存savepointはmarker変更の後に作った。Cold処理自体は回復しなかった。
生の事後観測を終えてからcallerがnamed rollback／releaseすると、
marker=1と安全なSELECTが回復し、その後のcommitで1が永続化された。
これはcallerによる回復であり、package cleanup成功の証拠ではない。

最初の判定不能後もcacheは空だった。callerが全体rollbackした後、
同じ接続のautocommit再試行はprobe/control/resultを送り、成功した。
さらに同じ接続の次transactionはresultだけで成功した。
READ ONLYからの回復系列も同じだった。
cache非保持／再試行の契約は検証範囲内で問題がなかった。

audit on/offは受理・値・transaction結果を変えなかった。
構築前後の対象optionsの値と存在有無は一致した。
正常な測定cacheが残ることと、次tracked callが監査記録を置換することは
契約どおりの変化として扱った。全ログの完全一致を要求しなかった。

### SQLとcondition

最初に送られた関連SQLは次の2件だった。
```sql
SELECT SUM('x') AS "p"
FROM (SELECT 1 AS z) AS "q01";

SELECT SUM("z") AS "p"
FROM (SELECT 1 AS z) AS "q01";
```
Cold／autocommitでは1件目のprepareで42725、
2件目はp=1として成功し、refusesを保持した。
Cold／caller transactionでは1件目の42725でabortedになり、
2件目は25P02で失敗した。その後の観測SELECTも25P02だった。
主系列の失敗ログで、probeより前に検証器のSQLエラーがないことを確認した。

公開conditionはmarginplyr_error／rlang_error／error／condition、
「could not be asked whether its SQL dialect converts」を含む拒否で、
parentとSQLSTATEはNULLだった。内部probeの元のDB conditionと同一ではない。
独立DBI対照ではprepareエラーを捕捉し、サーバーCSV error logで
42725と25P02を対象PID・SQLに対応付けた。
Sent queryは送信予定の構築記録なので、
実prepare／実行の確認はDBログと組み合わせた。
構築失敗時にresult entryはなく、明示的collectは実行しなかった。

構築中の方言判定はColdで2件、Warmで0件だった。
unknownの後の再試行では再び2件、測定成功後の次要求は0件だった。
ADR 0020の方言補助query件数の契約は、この観測範囲では守られた。
これを調査の実行回数上限に転用しなかった。

### 最小再現・独立対照

boundary.Rの正常fixtureはa=2、b=6の2行。
amount=2,6,8、Parent／Total=1/4,3/4,1を定数assertした。
Cold autocommit、Cold commit試行／rollback、Warm同接続／別接続の
5系列で主結果を再現した。追加3系列でinteger64 mappingでも
Cold autocommit成功、Cold caller失敗、Warm成功を確認した。

DBI::dbWithTransaction対照はhelper内部で構築失敗直後を記録した後、
保存した失敗messageを検証器が再送出してhelperのrollbackを起動した。
その自動後始末より前にはabortedだった。helper後にidle／marker=0となることを、
packageの復旧とは数えなかった。検証器のmessage再送出は
元conditionのclass保持を検証する実験ではない。

通常DBIで判定SQLだけを送る使い捨て対照でもabortedになった。
事後観測後にcaller savepointへ戻した対照ではnumeric controlが成功し、
markerをcommitできた。主実験へこの意図的SQL失敗を混ぜなかった。

負の対照は3種類を別プロセスで実施した。
利用者がSELECT 1/0で先に壊したtransactionはbeforeからaborted（22012）。
明示的disconnect後のinvalid接続はqueryを送らず拒否され、
disconnectのrollbackを製品動作と扱わなかった。
Warmでのcharacter sourceは構築時は正常、明示的collectで42883になり、
その後にabortedとなった。これは利用者が要求した不適格sourceの実行であり、
正常なCold構築の内部probeとは区別した。

### 他backend

DuckDB 1.5.5では同じ2行のCold autocommit／caller commit／rollbackとWarmを実施し、
構築・collect・値・安全なSELECT・caller marker・source／sentinelが正常だった。
測定時のcurrent_transaction_invalidation_policyはSTANDARD_POLICYだった。
[1.5.5のexception source](https://github.com/duckdb/duckdb/blob/v1.5.5/src/common/exception.cpp)と
[client context](https://github.com/duckdb/duckdb/blob/v1.5.5/src/main/client_context.cpp)は
通常のBinder errorをtransaction invalidation対象から除いていた。
このprobeでPostgreSQLと異なる結果になる理由を説明する証拠であり、
DuckDBの全エラーを安全とする結論ではない。

RSQLite／SQLite 3.53.3の同じ4系列は、既定shareをconvertsとして拒否した。
Warm欄は最初の公開呼び出しによる正常な測定拒否を事前履歴としたもので、
「正常share成功でWarmにする」というPostgreSQL系列とは異なる。
いずれもSELECTとmarkerのcommit／rollback、source／sentinelは正常だった。
Converting dialectの既定拒否は期待された制限で、新規Issueにしなかった。

## 調査対応表と終了判断

| 仮説／境界 | 結果 | 分類 |
|---|---|---|
| Cold正常shareがcaller transactionを中断 | 2行最小再現・正常対照・生状態・永続markerで確認 | marginplyr契約違反、#774 |
| 同接続／別接続の履歴で受理が変わる | Warmで成功、測定cache共有でprobe省略 | #774と同じ原因 |
| Parent／Total・count／sumで別原因か | 主系列の両source、単独kind、sumのみ最小再現で同じ境界 | #774の複数症状 |
| auditがtransactionを変える | on/offで同じ結果、options復元 | 範囲内で問題なし |
| unknownが永久に拒否を残す | rollback後に再判定、次transactionで再利用 | 範囲内で問題なし、#198修正を確認 |
| bigint control readingの再発 | numeric／integer64でautocommit成功 | #440の再発ではない |
| READ ONLY／SELECT権限が原因 | 同権限SQL対照とWarm成功、Coldのみaborted | #774と同じ原因 |
| 既存savepointだけで守られる | 自動回復なし、caller rollback-toで事前作業を保持 | #774、callerによる回復は正常 |
| helperが失敗状態を隠す | helper内部ではaborted、後始末後idle | helperの期待された動作 |
| PostgreSQL一般のSQL失敗 | DBI probe対照で再現 | DB仕様、packageの内部SQL責任と分離 |
| 既に壊れたtxn／invalid／不適格source | 独立負対照で拒否／取得時エラー | DB／driver／入力制限 |
| DuckDB・SQLiteへの同一結論の一般化 | DuckDB正常、SQLiteの拒否も状態保持 | 検証範囲内で追加問題なし |
| 観測器・fixtureの誤り | 下記の較正問題を除外し修正コードで再実行 | ハーネス誤り、製品指摘に数えず |

主行列50、最小／境界11、integer64追加3、他backend8の
合計72の最終ケース記録を採用した。探索・較正・同一コード再実行の件数ではない。
当初sandboxはshared memory初期化とUnix socket接続を拒否した。
専用クラスタの必要操作だけを承認されたsandbox escalationで実行した。
この接続不能をpackage failureと数えなかった。
最小再現の初回oracleはlocale依存のsortでTotalの位置を誤ったため、
assertion到達前のその試行を除外し、明示的match順で修正・再実行した。
計画の行数誤り、初期lint指摘も最終コードと区別した。
恒久的なlint抑制は追加しなかった。

終了前に全主ケースの値、構築／取得状態、caller marker、source／sentinel、
options、cache再試行、監査purpose、最初のDB errorのSQLSTATEを
保存記録とDBログから再assertした。
採用した状態境界と正常対照を処理し、単一原因の候補を最小化・Issue化したため終了した。
既定の実行件数や発見件数に達したことは停止理由ではない。
資源不足による未完了中断はなかった。

未検証は、他のR／依存／PostgreSQL版、別driver、他OS、
非default DuckDB invalidation policy、一般的なisolation-level／locking競合、
故障注入、接続故障復旧、interrupt／native cancellation、
materialization全般である。今回の正常利用の境界に別原因を示す証拠がなく、
無関係な設定cross-productは追加しなかった。
仕様判断が必要な独立の新規安全懸念は確認されなかった。

## 一次資料

- [PostgreSQL 17 Transactions](https://www.postgresql.org/docs/17/tutorial-transactions.html)：
  autocommit、失敗したtransaction、savepointと利用者workの境界。
- [ROLLBACK TO SAVEPOINT](https://www.postgresql.org/docs/17/sql-rollback-to.html)、
  [SET TRANSACTION](https://www.postgresql.org/docs/17/sql-set-transaction.html)：
  rollback-to後のsavepointとREAD ONLY設定の時点。
- [Monitoring](https://www.postgresql.org/docs/17/monitoring-stats.html)、
  [Error logging](https://www.postgresql.org/docs/17/runtime-config-logging.html)：
  aborted stateとSQLSTATEの独立観測。
- [libpq 16 status](https://www.postgresql.org/docs/16/libpq-status.html)、
  [execution](https://www.postgresql.org/docs/16/libpq-exec.html)、
  [PostgreSQL 17 protocol](https://www.postgresql.org/docs/17/protocol-flow.html)：
  connection statusとtransaction status、prepare／errorの区別。
- [DBI transactions](https://dbi.r-dbi.org/reference/transactions.html)と
  [DBI 1.3.0](https://github.com/r-dbi/DBI/tree/v1.3.0)：
  caller commit／rollback。実インストールのdbWithTransaction methodも読んだ。
- [RPostgres transactions](https://rpostgres.r-dbi.org/reference/postgres-transactions.html)と
  [1.4.10 transaction method](https://github.com/r-dbi/RPostgres/blob/v1.4.10/R/dbRollback_PqConnection.R)：
  named rollbackはrollback-toとreleaseを送る。
- [DuckDB transaction documentation](https://duckdb.org/docs/current/sql/statements/transactions.html)：
  使用した1.5系列。上記1.5.5 sourceでエラー種別の扱いも確認した。
- [SQLite transactions](https://www.sqlite.org/lang_transaction.html)、
  [savepoints](https://www.sqlite.org/lang_savepoint.html)：
  errorの範囲とcaller transaction／savepoint。

## 主ケース一覧

case列はworker.Rのhistory／mode／ending／kind／audit／role／extra引数。
rawは独立SQL比率、ordinaryは通常dbplyr詳細集計、noneはshareなしMargin、
uncheckedは適格numeric sourceの原因切り分け用。
collectの「—」は構築失敗のため未実行であり、取得時失敗ではない。

| case | 構築 | collect | 構築直後state | 終了後marker |
|---|---|---|---|---:|
| cold--auto--rollback--both--on--hunt_user--none | 成功 | 成功 | idle | 0 |
| cold--write--commit--both--on--hunt_user--none | 拒否 | — | idle in transaction (aborted) | 0 |
| cold--write--rollback--both--on--hunt_user--none | 拒否 | — | idle in transaction (aborted) | 0 |
| same--write--commit--both--on--hunt_user--none | 成功 | 成功 | idle in transaction | 1 |
| same--write--rollback--both--on--hunt_user--none | 成功 | 成功 | idle in transaction | 0 |
| other--write--commit--both--on--hunt_user--none | 成功 | 成功 | idle in transaction | 1 |
| other--write--rollback--both--on--hunt_user--none | 成功 | 成功 | idle in transaction | 0 |
| cold--readonly--commit--both--on--hunt_user--none | 拒否 | — | idle in transaction (aborted) | 0 |
| cold--readonly--rollback--both--on--hunt_user--none | 拒否 | — | idle in transaction (aborted) | 0 |
| same--readonly--commit--both--on--hunt_user--none | 成功 | 成功 | idle in transaction | 0 |
| other--readonly--rollback--both--on--hunt_user--none | 成功 | 成功 | idle in transaction | 0 |
| cold--auto--rollback--parent--on--hunt_user--none | 成功 | 成功 | idle | 0 |
| cold--write--commit--parent--on--hunt_user--none | 拒否 | — | idle in transaction (aborted) | 0 |
| same--write--commit--parent--on--hunt_user--none | 成功 | 成功 | idle in transaction | 1 |
| other--write--rollback--parent--on--hunt_user--none | 成功 | 成功 | idle in transaction | 0 |
| cold--auto--rollback--total--on--hunt_user--none | 成功 | 成功 | idle | 0 |
| cold--write--commit--total--on--hunt_user--none | 拒否 | — | idle in transaction (aborted) | 0 |
| same--write--commit--total--on--hunt_user--none | 成功 | 成功 | idle in transaction | 1 |
| other--write--rollback--total--on--hunt_user--none | 成功 | 成功 | idle in transaction | 0 |
| cold--auto--rollback--both--off--hunt_user--none | 成功 | 成功 | idle | 0 |
| cold--write--commit--both--off--hunt_user--none | 拒否 | — | idle in transaction (aborted) | 0 |
| same--write--commit--both--off--hunt_user--none | 成功 | 成功 | idle in transaction | 1 |
| other--write--commit--both--off--hunt_user--none | 成功 | 成功 | idle in transaction | 1 |
| cold--readonly--rollback--both--off--hunt_user--none | 拒否 | — | idle in transaction (aborted) | 0 |
| cold--auto--rollback--none--on--hunt_user--none | 成功 | 成功 | idle | 0 |
| cold--write--commit--none--on--hunt_user--none | 成功 | 成功 | idle in transaction | 1 |
| cold--write--rollback--none--on--hunt_user--none | 成功 | 成功 | idle in transaction | 0 |
| cold--readonly--rollback--none--on--hunt_user--none | 成功 | 成功 | idle in transaction | 0 |
| cold--auto--rollback--raw--on--hunt_user--none | 成功 | 成功 | idle | 0 |
| cold--write--commit--raw--on--hunt_user--none | 成功 | 成功 | idle in transaction | 1 |
| cold--write--rollback--raw--on--hunt_user--none | 成功 | 成功 | idle in transaction | 0 |
| cold--readonly--rollback--raw--on--hunt_user--none | 成功 | 成功 | idle in transaction | 0 |
| cold--auto--rollback--ordinary--on--hunt_user--none | 成功 | 成功 | idle | 0 |
| cold--write--commit--ordinary--on--hunt_user--none | 成功 | 成功 | idle in transaction | 1 |
| cold--write--rollback--ordinary--on--hunt_user--none | 成功 | 成功 | idle in transaction | 0 |
| cold--readonly--rollback--ordinary--on--hunt_user--none | 成功 | 成功 | idle in transaction | 0 |
| cold--auto--rollback--unchecked--on--hunt_user--none | 成功 | 成功 | idle | 0 |
| cold--write--commit--unchecked--on--hunt_user--none | 成功 | 成功 | idle in transaction | 1 |
| cold--write--rollback--unchecked--on--hunt_user--none | 成功 | 成功 | idle in transaction | 0 |
| cold--readonly--rollback--unchecked--on--hunt_user--none | 成功 | 成功 | idle in transaction | 0 |
| cold--readonly--commit--both--on--hunt_reader--none | 拒否 | — | idle in transaction (aborted) | 0 |
| same--readonly--commit--both--on--hunt_reader--none | 成功 | 成功 | idle in transaction | 0 |
| other--readonly--rollback--both--on--hunt_reader--none | 成功 | 成功 | idle in transaction | 0 |
| cold--readonly--rollback--raw--on--hunt_reader--none | 成功 | 成功 | idle in transaction | 0 |
| cold--readonly--rollback--ordinary--on--hunt_reader--none | 成功 | 成功 | idle in transaction | 0 |
| cold--write--commit--both--on--hunt_user--savepoint | 拒否 | — | idle in transaction (aborted) | 1 |
| same--write--commit--both--on--hunt_user--savepoint | 成功 | 成功 | idle in transaction | 1 |
| cold--write--rollback--both--on--hunt_user--retry | 拒否 | — | idle in transaction (aborted) | 0 |
| cold--readonly--rollback--both--on--hunt_user--retry | 拒否 | — | idle in transaction (aborted) | 0 |
| cold--write--commit--both--on--hunt_user--repeat | 拒否 | — | idle in transaction (aborted) | 0 |

## 再実行

新しい専用クラスタと隔離libraryだけで実行する。
以下3つのR blockをそれぞれworker.R、boundary.R、backend.Rとして
リポジトリ外へ抽出する。すべて最終的に実行・lintr／jarlで検証したコードである。
保存先はroot/results。root/libには上表の固定依存物と固定HEADのmarginplyrを置く。
通常libraryをworkerの探索pathへ追加しない。

準備手順の例（task_reference_libraryは同じ版を用意したread-only参照元）。
このクラスタ内のtrust認証はUnix socketの専用検証環境だけで使用した。
```sh
task_root=$(mktemp -d /private/tmp/marginplyr-cold-tx-replay-XXXXXX)
task_reference_library=/path/to/fixed/dependency-library
task_pg_bin=/path/to/postgresql-17.11/bin
mkdir -p "$task_root/lib" "$task_root/src" "$task_root/socket" "$task_root/results"
for task_pkg in DBI RPostgres bit64 bit blob rlang vctrs cli glue lifecycle hms pkgconfig lubridate generics timechange cpp11 withr dplyr magrittr pillar utf8 R6 tibble tidyselect dbplyr purrr tidyr stringr stringi duckdb RSQLite memoise cachem fastmap jsonlite; do
  cp -R "$task_reference_library/$task_pkg" "$task_root/lib/"
done
git archive 8b57a86a9846fb92053eb1ae1990219d4f294944 > "$task_root/source.tar"
tar -xf "$task_root/source.tar" -C "$task_root/src"
R_LIBS_USER="$task_root/lib" R_LIBS_SITE=''   R CMD INSTALL --library="$task_root/lib" "$task_root/src"
"$task_pg_bin/initdb" -D "$task_root/pgdata" -U hunt_admin   --auth=trust --no-locale --encoding=UTF8
cat >> "$task_root/pgdata/postgresql.conf" <<CONF
listen_addresses = ''
port = 55439
unix_socket_directories = '$task_root/socket'
logging_collector = on
log_destination = 'csvlog'
log_statement = 'all'
log_min_error_statement = 'error'
log_error_verbosity = 'verbose'
CONF
"$task_pg_bin/pg_ctl" -D "$task_root/pgdata" -l "$task_root/start.log" -w start
```

次のSQLをsetup.sqlとして一時rootへ保存し、専用DBへ投入する。

```sql
CREATE ROLE hunt_user LOGIN;
CREATE ROLE hunt_reader LOGIN;
CREATE TABLE source (
  period text NOT NULL, region text NOT NULL,
  store text NOT NULL, value double precision NOT NULL
);
INSERT INTO source VALUES
('P','N','a',2), ('P','N','a',4), ('P','N','b',6),
('P','S','c',8), ('Q','N','a',3), ('Q','S','c',9);
CREATE TABLE sentinel (value integer NOT NULL);
INSERT INTO sentinel VALUES (99);
CREATE TABLE marker (value integer NOT NULL);
INSERT INTO marker VALUES (0);
GRANT SELECT ON source, sentinel, marker TO hunt_user, hunt_reader;
GRANT UPDATE ON marker TO hunt_user;
```

```sh
"$task_pg_bin/psql" -h "$task_root/socket" -p 55439 -U hunt_admin   -d postgres -v ON_ERROR_STOP=1 -f "$task_root/setup.sql"

# Every command below starts a fresh R process.
Rscript --vanilla "$task_root/worker.R" "$task_root" cold auto rollback both on hunt_user none
Rscript --vanilla "$task_root/worker.R" "$task_root" cold write commit both on hunt_user none
Rscript --vanilla "$task_root/worker.R" "$task_root" same write commit both on hunt_user none
Rscript --vanilla "$task_root/worker.R" "$task_root" other write commit both on hunt_user none
# Run each remaining primary case using the seven fields of its case-table row.

for task_case in cold-auto cold-write cold-rollback warm-same warm-other   helper raw-probe raw-probe-savepoint prebroken character invalid; do
  Rscript --vanilla "$task_root/boundary.R" "$task_root" "$task_case"
done
for task_case in cold-auto cold-write warm-same; do
  Rscript --vanilla "$task_root/boundary.R" "$task_root" "$task_case" integer64
done
for task_backend in duckdb sqlite; do
  Rscript --vanilla "$task_root/backend.R" "$task_root" "$task_backend" auto rollback cold
  Rscript --vanilla "$task_root/backend.R" "$task_root" "$task_backend" write commit cold
  Rscript --vanilla "$task_root/backend.R" "$task_root" "$task_backend" write rollback cold
  Rscript --vanilla "$task_root/backend.R" "$task_root" "$task_backend" write commit warm
done

# Preserve observations before stopping only this dedicated cluster.
"$task_pg_bin/pg_ctl" -D "$task_root/pgdata" -m fast -w stop
```

### worker.R

```r
args <- commandArgs(TRUE)
root <- args[[1]]
history <- args[[2]]
mode <- args[[3]]
ending <- args[[4]]
kind <- args[[5]]
audit <- identical(args[[6]], "on")
role <- args[[7]]
extra <- args[[8]]
.libPaths(c(file.path(root, "lib"), .Library), include.site = FALSE)
library(marginplyr)
connect <- function(user) {
  DBI::dbConnect(
    RPostgres::Postgres(), host = file.path(root, "socket"),
    port = 55439L, dbname = "postgres", user = user, bigint = "numeric"
  )
}
admin <- connect("hunt_admin")
DBI::dbExecute(admin, "UPDATE marker SET value = 0")
con <- connect(role)
pid <- DBI::dbGetInfo(con)$pid
label <- paste(history, mode, ending, kind, args[[6]], role, extra, sep = "--")
options(marginplyr.audit_sql = audit)
DBI::dbExecute(con, paste0("SET application_name = '", label, "'"))
remote <- dplyr::tbl(
  con, "source", vars = c("period", "region", "store", "value")
)
condition_record <- function(cnd) {
  list(
    class = class(cnd), message = conditionMessage(cnd),
    call = paste(deparse(conditionCall(cnd)), collapse = " "),
    sqlstate = cnd$sqlstate,
    parent = if (is.null(cnd$parent)) NULL else condition_record(cnd$parent)
  )
}
attempt <- function(expr) {
  warnings <- list()
  ans <- withCallingHandlers(
    tryCatch(
      list(ok = TRUE, value = force(expr)),
      error = function(cnd) list(ok = FALSE, condition = condition_record(cnd))
    ),
    warning = function(cnd) {
      warnings[[length(warnings) + 1L]] <<- condition_record(cnd)
      invokeRestart("muffleWarning")
    }
  )
  ans$warnings <- warnings
  ans
}
plain <- function(ans) {
  ans$value <- NULL
  ans
}
cache <- function() {
  as.list(get("share_dialect_verdicts", asNamespace("marginplyr")),
          all.names = TRUE)
}
sent <- function() attempt(marginplyr::last_sent_queries())
option_names <- c(
  "warn", "marginplyr.audit_sql", "rlib_warning_verbosity",
  "lifecycle_verbosity", "rlang:::use_as_label_infix"
)
option_state <- function() {
  current <- options()
  current[intersect(option_names, names(current))]
}
observe <- function(stage) {
  state <- DBI::dbGetQuery(admin, paste0(
    "SELECT pid, state, xact_start::text, backend_xid::text, ",
    "query, application_name FROM pg_stat_activity WHERE pid = ", pid
  ))
  external <- DBI::dbGetQuery(admin, "SELECT value FROM marker")
  passive <- list(
    stage = stage, state = state, external_marker = external,
    valid = DBI::dbIsValid(con), options = option_state(), cache = cache(),
    audit = sent()
  )
  passive$select <- attempt(DBI::dbGetQuery(con, "SELECT 1 AS safe"))
  passive$settings <- attempt(DBI::dbGetQuery(con, paste0(
    "SELECT current_setting('transaction_read_only') AS read_only, ",
    "current_setting('transaction_isolation') AS isolation"
  )))
  passive$source <- attempt(DBI::dbGetQuery(con, "SELECT * FROM source"))
  passive$sentinel <- attempt(DBI::dbGetQuery(con, "SELECT * FROM sentinel"))
  passive$marker <- attempt(DBI::dbGetQuery(con, "SELECT * FROM marker"))
  passive
}
expected <- data.frame(
  period = c(rep("P", 6), rep("Q", 5)),
  region = c("N", "N", "S", "N", "S", "Total",
             "N", "S", "N", "S", "Total"),
  store = c("a", "b", "c", "Total", "Total", "Total",
            "a", "c", "Total", "Total", "Total"),
  set_id = c(1, 1, 1, 2, 2, 3, 1, 1, 2, 2, 3),
  n = c(2, 1, 1, 3, 1, 4, 1, 1, 1, 1, 2),
  amount = c(6, 6, 8, 12, 8, 20, 3, 9, 3, 9, 12)
)
expected$parent_n <- c(2 / 3, 1 / 3, 1, 3 / 4, 1 / 4, 1,
                       1, 1, 1 / 2, 1 / 2, 1)
expected$parent_amount <- c(1 / 2, 1 / 2, 1, 3 / 5, 2 / 5, 1,
                            1, 1, 1 / 4, 3 / 4, 1)
expected$total_n <- c(1 / 2, 1 / 4, 1 / 4, 3 / 4, 1 / 4, 1,
                      1 / 2, 1 / 2, 1 / 2, 1 / 2, 1)
expected$total_amount <- c(3 / 10, 3 / 10, 2 / 5, 3 / 5, 2 / 5, 1,
                           1 / 4, 3 / 4, 1 / 4, 3 / 4, 1)
canonical <- function(x) {
  x <- as.data.frame(x)
  x <- x[order(x$period, x$set_id, x$region, x$store), , drop = FALSE]
  rownames(x) <- NULL
  x
}
matches <- function(x) {
  cols <- names(x)
  isTRUE(all.equal(canonical(x), canonical(expected[cols]),
                   tolerance = 1e-12, check.attributes = FALSE))
}
margin <- function(data, shares = kind) {
  dots <- list(
    n = rlang::quo(dplyr::n()),
    amount = rlang::quo(sum(!!rlang::sym("value"), na.rm = TRUE))
  )
  if (shares %in% c("parent", "both", "unchecked")) {
    dots$parent_n <- rlang::quo(share_of_parent(!!rlang::sym("n")))
    dots$parent_amount <- rlang::quo(share_of_parent(!!rlang::sym("amount")))
  }
  if (shares %in% c("total", "both", "unchecked")) {
    dots$total_n <- rlang::quo(share_of_total(!!rlang::sym("n")))
    dots$total_amount <- rlang::quo(share_of_total(!!rlang::sym("amount")))
  }
  if (shares == "unchecked") {
    return(summarize_with_margins(
      data, !!!dots, .by = "period", .grouping = rollup("region", "store"),
      .id = "set_id", .sort = "none", .check_share_source = FALSE
    ))
  }
  summarize_with_margins(
    data, !!!dots, .by = "period", .grouping = rollup("region", "store"),
    .id = "set_id", .sort = "none"
  )
}
raw_sql <- paste(
  "WITH a AS (",
  " SELECT period, region, store, COUNT(*)::double precision AS n,",
  " SUM(value) AS amount,",
  " CASE WHEN GROUPING(store)=0 THEN 1",
  " WHEN GROUPING(region)=0 THEN 2 ELSE 3 END AS set_id",
  " FROM source GROUP BY period, GROUPING SETS ((region,store),(region),())",
  ") SELECT c.period, COALESCE(c.region,'Total') AS region,",
  "COALESCE(c.store,'Total') AS store, c.set_id, c.n, c.amount,",
  "c.n / p.n AS parent_n, c.amount / p.amount AS parent_amount,",
  "c.n / t.n AS total_n, c.amount / t.amount AS total_amount",
  "FROM a c JOIN a t ON c.period=t.period AND t.set_id=3",
  "JOIN a p ON c.period=p.period AND",
  "p.set_id=LEAST(c.set_id+1,3) AND",
  "(c.set_id<>1 OR c.region=p.region)"
)
warm <- NULL
if (history %in% c("same", "other")) {
  first_con <- if (history == "same") con else connect(role)
  first_remote <- dplyr::tbl(
    first_con, "source", vars = c("period", "region", "store", "value")
  )
  first <- margin(first_remote, "both")
  first_audit <- sent()
  first_values <- dplyr::collect(first)
  stopifnot(matches(first_values))
  warm <- list(audit = first_audit, cache = cache(), values = first_values)
  if (history == "other") DBI::dbDisconnect(first_con)
}
start <- observe("before_transaction")
if (mode != "auto") {
  DBI::dbBegin(con)
  if (mode == "readonly") DBI::dbExecute(con, "SET TRANSACTION READ ONLY")
  if (mode == "write") DBI::dbExecute(con, "UPDATE marker SET value = 1")
  if (extra == "savepoint") DBI::dbBegin(con, name = "caller_point")
}
before <- observe("before_margin")
construction <- attempt(
  if (kind == "raw") {
    structure(raw_sql, class = "hunt_raw")
  } else if (kind == "ordinary") {
    dplyr::summarise(
      dplyr::group_by(remote, period, region, store),
      n = dplyr::n(), amount = sum(value, na.rm = TRUE), .groups = "drop"
    )
  } else {
    margin(remote)
  }
)
after_construction <- observe("after_construction")
collected <- NULL
rendered <- NULL
if (construction$ok) {
  rendered <- if (kind == "raw") {
    raw_sql
  } else {
    as.character(dbplyr::sql_render(construction$value))
  }
  collected <- attempt(
    if (kind == "raw") {
      DBI::dbGetQuery(con, raw_sql)
    } else {
      dplyr::collect(construction$value)
    }
  )
  if (collected$ok) {
    if (kind == "ordinary") {
      expected_leaves <- expected[expected$set_id == 1, c(
        "period", "region", "store", "n", "amount"
      )]
      got <- as.data.frame(collected$value)
      sort_leaves <- function(x) {
        x <- x[order(x$period, x$region, x$store), ]
        rownames(x) <- NULL
        x
      }
      stopifnot(isTRUE(all.equal(
        sort_leaves(got), sort_leaves(expected_leaves),
        check.attributes = FALSE
      )))
    } else {
      stopifnot(matches(collected$value))
      share_cols <- grep(
        "^(parent|total)_", names(collected$value), value = TRUE
      )
      stopifnot(all(vapply(collected$value[share_cols], is.double, logical(1))))
    }
  }
}
after_collect <- if (construction$ok) observe("after_collect") else NULL
recovery <- NULL
if (extra == "savepoint") {
  recovery <- attempt(DBI::dbRollback(con, name = "caller_point"))
  recovery$after <- observe("after_caller_savepoint_rollback")
}
finish <- if (mode == "auto") list(ok = TRUE) else attempt(
  if (ending == "commit") DBI::dbCommit(con) else DBI::dbRollback(con)
)
after_finish <- observe("after_caller_finish")
retry <- NULL
if (extra == "retry") {
  again <- attempt(margin(remote, "both"))
  again_audit <- sent()
  again_values <- if (again$ok) attempt(dplyr::collect(again$value)) else NULL
  if (!is.null(again_values) && again_values$ok) {
    stopifnot(matches(again_values$value))
  }
  DBI::dbBegin(con)
  if (role == "hunt_user") DBI::dbExecute(con, "UPDATE marker SET value = 1")
  third <- attempt(margin(remote, "both"))
  third_audit <- sent()
  third_values <- if (third$ok) attempt(dplyr::collect(third$value)) else NULL
  if (!is.null(third_values) && third_values$ok) {
    stopifnot(matches(third_values$value))
  }
  third_after <- observe("after_retry_transaction")
  DBI::dbRollback(con)
  retry <- list(
    second = plain(again), second_audit = again_audit,
    second_values = again_values, third = plain(third),
    third_audit = third_audit, third_values = third_values,
    third_after = third_after
  )
}
loaded <- data.frame(
  package = loadedNamespaces(),
  version = vapply(loadedNamespaces(), function(p) {
    as.character(utils::packageVersion(p))
  }, character(1)),
  path = vapply(loadedNamespaces(), find.package, character(1))
)
stopifnot(all(startsWith(loaded$path, file.path(root, "lib")) |
                startsWith(loaded$path, .Library)))
record <- list(
  case = label, pid = pid, warm = warm, start = start, before = before,
  construction = plain(construction), rendered = rendered,
  after_construction = after_construction, collected = collected,
  after_collect = after_collect, recovery = recovery, finish = finish,
  after_finish = after_finish, retry = retry, loaded = loaded,
  connection = DBI::dbGetInfo(con), source_expected = expected
)
jsonlite::write_json(
  record, file.path(root, "results", paste0(label, ".json")),
  auto_unbox = TRUE, pretty = TRUE, null = "null", dataframe = "rows"
)
DBI::dbDisconnect(con)
DBI::dbDisconnect(admin)
cat(label, "construct=", construction$ok,
    "collect=", if (is.null(collected)) "not-run" else collected$ok,
    "state=", after_construction$state$state,
    "marker_after=", after_finish$external_marker$value, "\n")
```

### boundary.R

```r
args <- commandArgs(TRUE)
root <- args[[1]]
scenario <- args[[2]]
bigint <- if (length(args) >= 3L) args[[3]] else "numeric"
label <- if (length(args) >= 3L) {
  paste(scenario, bigint, sep = "--")
} else {
  scenario
}
.libPaths(c(file.path(root, "lib"), .Library), include.site = FALSE)
library(marginplyr)
connect <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(), host = file.path(root, "socket"),
    port = 55439L, dbname = "postgres", user = "hunt_user",
    bigint = bigint
  )
}
admin <- DBI::dbConnect(
  RPostgres::Postgres(), host = file.path(root, "socket"),
  port = 55439L, dbname = "postgres", user = "hunt_admin"
)
DBI::dbExecute(admin, "UPDATE marker SET value = 0")
DBI::dbExecute(admin, "DROP TABLE IF EXISTS mini_source")
DBI::dbExecute(admin, paste(
  "CREATE TABLE mini_source (g text NOT NULL, v double precision NOT NULL)"
))
DBI::dbExecute(admin, "INSERT INTO mini_source VALUES ('a',2),('b',6)")
DBI::dbExecute(admin, "GRANT SELECT ON mini_source TO hunt_user")
con <- connect()
pid <- DBI::dbGetInfo(con)$pid
DBI::dbExecute(con, paste0("SET application_name = 'boundary-", scenario, "'"))
options(marginplyr.audit_sql = TRUE)
remote <- dplyr::tbl(con, "mini_source", vars = c("g", "v"))
capture <- function(expr) {
  tryCatch(
    list(ok = TRUE, value = force(expr)),
    error = function(cnd) {
      list(
        ok = FALSE, class = class(cnd), message = conditionMessage(cnd),
        sqlstate = cnd$sqlstate,
        parent_class = if (is.null(cnd$parent)) NULL else class(cnd$parent)
      )
    }
  )
}
observe <- function(stage) {
  # Observe the server before sending any diagnostic SQL on the target.
  server <- DBI::dbGetQuery(admin, paste0(
    "SELECT pid, state, xact_start::text, query FROM pg_stat_activity ",
    "WHERE pid = ", pid
  ))
  list(
    stage = stage, server = server, valid = DBI::dbIsValid(con),
    external_marker = DBI::dbGetQuery(admin, "SELECT * FROM marker"),
    safe = capture(DBI::dbGetQuery(con, "SELECT 1 AS safe")),
    own_marker = capture(DBI::dbGetQuery(con, "SELECT * FROM marker")),
    sentinel = capture(DBI::dbGetQuery(con, "SELECT * FROM sentinel"))
  )
}
margin <- function(x) {
  summarize_with_margins(
    x, amount = sum(!!rlang::sym("v"), na.rm = TRUE),
    parent = share_of_parent(!!rlang::sym("amount")),
    total = share_of_total(!!rlang::sym("amount")),
    .grouping = rollup("g")
  )
}
warm <- NULL
if (scenario %in% c("warm-same", "warm-other", "character")) {
  first_con <- if (scenario == "warm-other") connect() else con
  first <- margin(dplyr::tbl(first_con, "mini_source", vars = c("g", "v")))
  warm <- list(audit = last_sent_queries(), values = dplyr::collect(first))
  if (scenario == "warm-other") DBI::dbDisconnect(first_con)
}
records <- new.env(parent = emptyenv())
execute <- function() {
  construction <- capture(if (scenario == "character") {
    character_input <- dplyr::mutate(
      remote, v = as.character(!!rlang::sym("v"))
    )
    summarize_with_margins(
      character_input, amount = max(!!rlang::sym("v"), na.rm = TRUE),
      total = share_of_total(!!rlang::sym("amount")),
      .grouping = rollup("g")
    )
  } else {
    margin(remote)
  })
  records$construction <- construction
  records$construction$value <- NULL
  records$audit <- capture(last_sent_queries())
  records$after_construction <- observe("after_construction")
  if (construction$ok) {
    records$collect <- capture(dplyr::collect(construction$value))
    records$after_collect <- observe("after_collect")
  }
  if (scenario == "helper" && !construction$ok) {
    stop(construction$message, call. = FALSE)
  }
  invisible(NULL)
}
if (scenario == "helper") {
  records$helper <- capture(DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "UPDATE marker SET value = 1")
    records$before <- observe("before")
    execute()
  }))
} else {
  if (scenario != "cold-auto") {
    DBI::dbBegin(con)
    DBI::dbExecute(con, "UPDATE marker SET value = 1")
  }
  if (scenario == "prebroken") {
    records$user_error <- capture(DBI::dbGetQuery(con, "SELECT 1/0"))
  }
  if (scenario == "invalid") DBI::dbDisconnect(con)
  if (scenario == "raw-probe-savepoint") {
    DBI::dbBegin(con, name = "user_probe")
  }
  records$before <- observe("before")
  if (scenario %in% c("raw-probe", "raw-probe-savepoint")) {
    records$probe <- capture(DBI::dbGetQuery(
      con, "SELECT SUM('x') AS p FROM (SELECT 1 AS z) AS q"
    ))
    records$after_probe <- observe("after_probe")
    if (scenario == "raw-probe-savepoint") {
      records$recover <- capture(DBI::dbRollback(con, name = "user_probe"))
      records$after_recover <- observe("after_recover")
    }
    records$control <- capture(DBI::dbGetQuery(
      con, "SELECT SUM(z) AS p FROM (SELECT 1 AS z) AS q"
    ))
  } else {
    execute()
  }
  if (scenario != "cold-auto") {
    records$finish <- capture(
      if (scenario %in% c("cold-rollback", "prebroken", "character")) {
        DBI::dbRollback(con)
      } else {
        DBI::dbCommit(con)
      }
    )
  }
}
records$after_finish <- observe("after_finish")
records$warm <- warm
records$scenario <- scenario
records$bigint <- bigint
records$pid <- pid
if (!is.null(records$collect) && records$collect$ok) {
  x <- records$collect$value
  x <- x[match(c("Total", "a", "b"), x$g), ]
  stopifnot(
    identical(x$g, c("Total", "a", "b")),
    identical(x$amount, c(8, 2, 6)),
    identical(x$parent, c(1, 1 / 4, 3 / 4)),
    identical(x$total, c(1, 1 / 4, 3 / 4))
  )
}
jsonlite::write_json(
  as.list(records),
  file.path(root, "results", paste0("boundary--", label, ".json")),
  auto_unbox = TRUE, pretty = TRUE, dataframe = "rows", null = "null"
)
if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
DBI::dbDisconnect(admin)
cat(scenario, "completed\n")
```

### backend.R

```r
args <- commandArgs(TRUE)
root <- args[[1]]
backend <- args[[2]]
mode <- args[[3]]
ending <- args[[4]]
history <- args[[5]]
.libPaths(c(file.path(root, "lib"), .Library), include.site = FALSE)
library(marginplyr)
label <- paste(backend, mode, ending, history, sep = "--")
dbfile <- file.path(root, paste0(label, ".db"))
stopifnot(!file.exists(dbfile))
driver <- if (backend == "duckdb") {
  duckdb::duckdb(shared_home = FALSE)
} else {
  RSQLite::SQLite()
}
connect <- function() DBI::dbConnect(driver, dbdir = dbfile, dbname = dbfile)
con <- connect()
fixture <- data.frame(g = c("a", "b"), v = c(2, 6))
DBI::dbWriteTable(con, "source", fixture)
DBI::dbWriteTable(con, "marker", data.frame(value = 0L))
DBI::dbWriteTable(con, "sentinel", data.frame(value = 99L))
observer <- connect()
options(marginplyr.audit_sql = TRUE)
remote <- dplyr::tbl(con, "source", vars = c("g", "v"))
capture <- function(expr) {
  tryCatch(list(ok = TRUE, value = force(expr)), error = function(cnd) {
    list(ok = FALSE, class = class(cnd), message = conditionMessage(cnd))
  })
}
margin <- function(x) {
  summarize_with_margins(
    x, amount = sum(!!rlang::sym("v"), na.rm = TRUE),
    parent = share_of_parent(!!rlang::sym("amount")),
    total = share_of_total(!!rlang::sym("amount")),
    .grouping = rollup("g")
  )
}
observe <- function() {
  list(
    external_marker = capture(DBI::dbGetQuery(
      observer, "SELECT * FROM marker"
    )),
    safe = capture(DBI::dbGetQuery(con, "SELECT 1 AS safe")),
    marker = capture(DBI::dbGetQuery(con, "SELECT * FROM marker")),
    source = capture(DBI::dbGetQuery(con, "SELECT * FROM source")),
    sentinel = capture(DBI::dbGetQuery(con, "SELECT * FROM sentinel"))
  )
}
warm <- NULL
if (history == "warm") {
  warm <- capture(margin(remote))
  warm$audit <- last_sent_queries()
  if (warm$ok) warm$values <- dplyr::collect(warm$value)
  warm$value <- NULL
}
settings <- if (backend == "duckdb") {
  DBI::dbGetQuery(con, paste(
    "SELECT name,value FROM duckdb_settings() WHERE name LIKE '%transaction%'"
  ))
} else {
  DBI::dbGetQuery(con, "SELECT sqlite_version() AS version")
}
if (mode == "write") {
  DBI::dbBegin(con)
  DBI::dbExecute(con, "UPDATE marker SET value = 1")
}
before <- observe()
construction <- capture(margin(remote))
audit <- last_sent_queries()
after_construction <- observe()
collected <- if (construction$ok) capture(dplyr::collect(construction$value))
after_collect <- if (construction$ok) observe()
if (!is.null(collected) && collected$ok) {
  x <- collected$value
  x <- x[match(c("Total", "a", "b"), x$g), ]
  stopifnot(
    identical(x$amount, c(8, 2, 6)),
    identical(x$parent, c(1, 1 / 4, 3 / 4)),
    identical(x$total, c(1, 1 / 4, 3 / 4))
  )
}
finish <- if (mode == "write") capture(
  if (ending == "commit") DBI::dbCommit(con) else DBI::dbRollback(con)
)
after_finish <- observe()
stopifnot(
  after_construction$safe$ok,
  after_construction$external_marker$value$value == 0,
  after_finish$marker$value$value ==
    as.integer(mode == "write" && ending == "commit"),
  identical(after_finish$source$value, fixture),
  after_finish$sentinel$value$value == 99L
)
construction$value <- NULL
jsonlite::write_json(
  list(
    case = label, settings = settings, warm = warm, before = before,
    construction = construction, audit = audit,
    after_construction = after_construction, collected = collected,
    after_collect = after_collect, finish = finish, after_finish = after_finish
  ),
  file.path(root, "results", paste0("backend--", label, ".json")),
  auto_unbox = TRUE, pretty = TRUE, null = "null", dataframe = "rows"
)
DBI::dbDisconnect(observer)
DBI::dbDisconnect(con)
if (backend == "duckdb") duckdb::duckdb_shutdown(driver)
unlink(c(dbfile, paste0(dbfile, "-wal"), paste0(dbfile, "-shm")))
cat(label, "construction=", construction$ok, "safe=TRUE\n")
```


## 検証と公開境界

このノートはRepository-onlyであり、生成入力・package挙動・check toolingを変えない。
investigationは.Rbuildignoreで除外されている。
Review-ready package boundaryの追加実行対象ではなかった。
公開前の検証はすべて成功した。

- jarl 0.6.0: repository全体と抽出した3つのR blockで指摘なし。
- lintr 3.4.0: pkgload::load_all()後のpackage-aware lintと、
  抽出した3つのR blockで指摘なし。
- 抽出コードのSHA-256は実行済みworker／boundary／backendと一致。
  その抽出コードからCold／Warm、helper、caller savepoint、integer64、
  DuckDB／SQLiteを再実行し、主50ケースの状態／値／ログassertionも再度成功した。
- context-budget: 20008 bytes、baseline 22005 bytes内。
- 文書参照verifier: 236参照を確認、指摘なし。
- verifier invocation: 14 verifierの呼び出しを確認、指摘なし。

最終確認後、専用クラスタの残存client接続は0だった。
pg_ctlの終了結果で停止成功を確認し、SQL証拠をリポジトリ外へ保存してから
専用DBデータを削除した。停止・削除をproduct cleanupの証拠には使用しなかった。
今回の変更は本ノートの追加だけであり、package-affecting変更はなかった。
コードレビューroundは実施しなかった。製品修正とPRマージは別工程とした。
