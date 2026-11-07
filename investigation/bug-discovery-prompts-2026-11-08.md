# 公開デバッグ・レビュー用プロンプトの調査

Investigated: 2026-11-08

## 調査範囲

2026-11-08 に、OpenAI、Anthropic、Sentry、GitHub、obra/superpowers、
Matt Pocock が公開した実際のプロンプト本文と Differential Prompting の
論文を読んだ。候補プロンプト間の採用率、人気順位、発見率を比較するデータは調べて
いないため、以下は「公開プロジェクトのプロンプト例」であり、「最も有名」
「最も有効」という順位づけではない。公開本文の調査であり、この場で各 skill
を実行した記録ではない。

コード上の原本は取得時に確認したコミットに固定した。最初に見つかった
`anthropics/claude-code/plugins/` の URL は直接取得で 404 となり、
`anthropics/claude-plugins-official/plugins/` の本文を使用した。

## 1. obra/superpowers: systematic-debugging

[原本の SKILL.md](https://github.com/obra/superpowers/blob/22f0b463e7a5e579904dd6211a520321035526f7/skills/systematic-debugging/SKILL.md)
は、原因調査、動作する類例との比較、単一仮説の最小実験、修正の四段階を
指定していた。原因調査には再現、最近の変更、各構成要素の境界での入出力
確認、異常値の発生源への逆向き追跡が含まれていた。

未知の不具合探索へ取り込めるのは、候補を見つけた後に、正常例との差分を
一つずつ変えて原因を絞る手順である。探索開始時には既知の症状がないため、
この手順だけでは探索対象や期待値を選べない。修正段階まで含む skill なので、
調査のみの依頼では再現と原因の説明までに区切る必要がある。

## 2. Anthropic: code-review command

[実行用 command 本文](https://github.com/anthropics/claude-plugins-official/blob/0412be76f50ab4f4f8abff3753efa9269fbbca5b/plugins/code-review/commands/code-review.md)
は、適用される指示の確認と差分要約を行い、複数の担当による候補生成後、
別の担当で候補を検証し、未検証のものを除く構成だった。重複コメントや、
一般論だけの品質・セキュリティ指摘も除外していた。

未知の不具合探索へ取り込めるのは、「候補を出す」と「本当に不具合かを
反証しながら確認する」を別の工程にする点である。ただし、本文は変更箇所
のみを対象とし、特定の入力や状態に依存する候補も除外していた。これを
既存パッケージ全体の探索へそのまま移すと、境界値の不具合を見落とす。
探索では具体的な入力と仕様違反を再現できる候補を残すのが妥当と判断した。

[同じプラグインの README](https://github.com/anthropics/claude-plugins-official/blob/0412be76f50ab4f4f8abff3753efa9269fbbca5b/plugins/code-review/README.md)
は 0–100 の採点と 80 点の閾値、履歴調査担当を説明していた。一方、読んだ
command 本文にはその数値閾値がなく、候補の別途検証を指定していた。
このため、本調査では実行用本文の手順を採用し、README の説明と同一視
しなかった。

## 補足 A. Anthropic PR review toolkit: code-reviewer

[agent 本文](https://github.com/anthropics/claude-plugins-official/blob/0412be76f50ab4f4f8abff3753efa9269fbbca5b/plugins/pr-review-toolkit/agents/code-reviewer.md)
は、対象範囲を宣言し、指示への適合と実際の機能不良を点検し、根拠・場所・
修正案を付けて報告する構成だった。80 以上だけを報告する採点表もあった。

対象範囲と報告形式の明示は再利用できる。ただし、採点表は「確信度」と
重要度を同じ尺度で記述していた。未知の不具合探索では、再現の確かさと
利用者への影響を別々に記録し、数値だけで裏づけを代用しない方がよいと
判断した。

## 補足 B. Anthropic PR review toolkit: silent-failure-hunter

[agent 本文](https://github.com/anthropics/claude-plugins-official/blob/0412be76f50ab4f4f8abff3753efa9269fbbca5b/plugins/pr-review-toolkit/agents/silent-failure-hunter.md)
は、例外処理、失敗時の既定値、fallback、エラー伝播、後始末を列挙し、
何を捕捉し、何を隠し、利用者に何が見えるかを調べる手順だった。

境界ごとに失敗経路を追う視点は有用だった。一方、本文には Sentry、Statsig、
`constants/errorIds.ts` などの前提や、全エラーのログ要求が含まれていた。
これらを R パッケージへ機械的に移す根拠はなかった。
marginplyr では、`CONTEXT.md` の Package condition、External condition、
Condition context、Repeated condition の契約に従って、クラス・原因・文脈・
通知回数の観測へ置き換えるのが適切と判断した。

## 3. OpenAI Codex: review rubric

[rubric 本文](https://github.com/openai/codex/blob/aa9b9cc94e215eb205901fcbfee6c68037130a4a/codex-rs/prompts/templates/review/rubric.md)
は、実害があり個別に対処可能な不具合を対象とし、暗黙の意図の推測を避け、
影響先を具体的に示すよう求めていた。発生に必要な入力・環境・条件を明記し、
該当する発見がなければ空の結果を許し、確信度と優先度を別フィールドに
していた。

未知の不具合探索にも、発生条件、影響する利用箇所、意図された仕様との差を
報告の必須要件にする点を利用できる。ただし、本文は新しい差分に入った
不具合だけを対象とし、場所も差分内に限っていた。パッケージ全体の探索では
既存不具合を除外する条件と patch 正当性の最終判定を外す必要がある。

## 4. mattpocock/skills: diagnosing-bugs

[SKILL.md 本文](https://github.com/mattpocock/skills/blob/4ae2317ddb22b046e26455e4f9c9b3aaaf62212d/skills/engineering/diagnosing-bugs/SKILL.md)
は、症状を正確に捉える短く安定した合否判定を先に構築し、再現を縮小し、
予測を持つ 3–5 個の仮説を順位づけして一変数ずつ検証する構成だった。
回帰テストが実際の呼び出し経路を再現するかも問うていた。

未知の不具合の候補が得られた後の最小化・原因検証に適していた。
既知の症状を前提とする「赤になる実行を作るまで仮説を立てない」という
停止条件を探索開始前へ移すと、まだ症状が見つかっていない探索が進まない。
この区別を調査プロンプトに書くのが適切と判断した。

## 5. getsentry/skills: find-bugs

[SKILL.md 本文](https://github.com/getsentry/skills/blob/e94f22214528d69899d09c627b3315ddc3544816/skills/find-bugs/SKILL.md)
は、差分を省略せず取得し、入力・問い合わせ・状態操作・外部呼び出し等を
列挙し、チェックリストで調べ、候補が他の箇所で既に処理されていないかと
既存テストを確認し、最後に未検証範囲を申告する構成だった。重要な発見が
なければ作らず、そのまま報告することも指定していた。

再利用できるのは入力から出力までの経路の棚卸し、周辺処理による反証、
未検証範囲の申告だった。本文の主眼はセキュリティであり、認証、XSS 等の
分類や優先順位を通常の機能正当性の調査へ移す根拠はなかった。

## 6. github/awesome-copilot: quality-playbook review protocols

[review_protocols.md](https://github.com/github/awesome-copilot/blob/797337f33f73e35d08d82f840e21c4b8c4932d88/skills/quality-playbook/references/review_protocols.md)
の先頭 190 行にあるコードレビュー手順を読んだ。構造の確認、要件と実装の
照合、要件間の整合性の三つの pass を分け、関数名だけで動作を推定せず
本体を読み、未確定候補を QUESTION とし、不在を主張する前に検索する
構成だった。再現テストと発見箇所の経路が一致しなければ未確認とする手順も
含まれていた。

未知の不具合探索へは、一般的な実装上の疑問と契約から導いた疑問を別々に
生成して照合し、再現が別の経路を調べていないかを確認する点が役立つと
判断した。本文の大きな成果物構成、修正・回帰テスト導入の強制、処理が
見当たらないことだけで不具合とするような断定は、調査だけの依頼に
そのまま移植しない。

## 7. Differential Prompting

[Nuances are the Key: Unlocking ChatGPT to Find Failure-Inducing Tests with Differential Prompting, v6](https://arxiv.org/html/2304.11686v6)
の Introduction、Methodology を読んだ。論文は意図を推定し、別の参照実装を
生成し、多様な入力を実行し、参照実装間の一致と調査対象との差を照合する
手法を説明していた。期待値の誤りでもテストは失敗し得るため、失敗だけを
不具合の証明にしないという区別も説明していた。

これは未知の不具合探索に直接関係する方法だった。ただし、参照実装が共通の
誤りを持つ場合は誤った期待値になる。論文は 100 行未満の単純なプログラムで
最もうまく働くと述べており、報告された性能を marginplyr や別モデルへ
転用する根拠はなかった。marginplyr では公開契約と小さな独立計算を期待値の
根拠にし、差異を見つけてから期待値自体を再検証するのが適切と判断した。

## 安全性に関する案内の確認

[OpenAI の追加安全性チェックの案内](https://help.openai.com/ja-jp/articles/20001326-additional-safety-checks-for-biological-and-cybersecurity-requests-in-chatgpt-codex-and-the-api)
は、許可される依頼でコンテンツが表示されない場合、対象を絞り必要な背景
だけを添えて再試行するよう説明していた。同時に、言い換えで依頼が許可
されるかどうかは変わらず、回答も保証されないと明記していた。

この案内から、特定の単語が本件の偽陽性を起こしたとは立証できない。
通常の機能正当性を調べるという作業の事実、対象、ローカル実験の範囲を
明確にすることと、安全性チェックを回避できると約束することを区別した。

## marginplyr の調査プロンプトへ導ける設計案

以下は原本の主張ではなく、上の読みから得た調整案である。パッケージの
実際の契約の権威は `CONTEXT.md`、適用される ADR、公開ヘルプに置く。

1. 通常の機能正当性を目的として、期待される値・型・順序・通知・副作用を
   明示する。セキュリティ監査用の攻撃者、攻撃経路、脆弱性の語彙を調査の
   前提にしない。語彙を変えるだけで正当性は保証されず、観測対象と根拠を
   具体化することが必要である。
2. 実装構造、公開契約、契約間の整合性の独立した観点から候補を生成する。
   公開された入力経路を使い、正常例から一変数ずつ変える。入力生成と期待値
   生成を分け、期待値は仕様か独立した計算から導く。実行した R が差を検出
   するようにし、調査対象の実装をそのまま正解に使わない。
3. 探索時には候補を広く生成し、確定時に、仕様違反、依存先の仕様、
   意図された拒否、期待値の誤りのどれかを検討する。再現しなかった仮説も
   短く残し、発見件数を要求しない。一つの発見で終了せず、隣接経路や別の
   原因へ探索を戻す。
4. 確認済みの発見には、契約の出典、最小の実行可能な R 再現、期待値と
   実測値、必要な版と backend、原因箇所、利用者への影響を付ける。
   実行できなかったものは未確認とし、確信度と重要度を分ける。
5. 条件・状態・backend 境界を調べる際も、原本のクラウドサービス用規約を
   移植せず、marginplyr の契約と許可された小さなローカル実験に合わせる。

この調査では marginplyr の不具合探索、修正、レビュー、パッケージ検証は
実行していない。公開プロンプトの発見性能や、安全性判定への影響も測定して
いない。
