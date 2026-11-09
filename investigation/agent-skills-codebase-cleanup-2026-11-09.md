# コード整理・責務分離のための公開 Agent Skills 調査

Investigated: 2026-11-09

## 調査結果

2026-11-09 の調査では、marginplyr の設計改善には Matt Pocock の
`improve-codebase-architecture` と `codebase-design` の組み合わせが最も
適合すると判断した。前者は改善箇所を探す手順、後者は小さなインターフェースに
複雑さを閉じ込めるための設計語彙だった。単に関数やファイルを短くするより、
呼出側が知る必要のある事実や変更箇所を減らす方向を明示していた。
[探索スキル][matt-improve]、[設計スキル][matt-design]

「clean code」「modular」「architecture」「separation of concerns」という
要望に名前と内容が直接対応した候補は、sickn33 のカタログの `clean-code` と
`software-architecture` だった。ただし、後者には npm、arrow functions、
React 系サービスの例、固定行数による分割が含まれていた。R パッケージに
そのまま常時適用するより、既存の設計に沿った判断材料として限定利用する方が
適切だと判断した。[clean-code][clean-code]、[software-architecture][software-architecture]

本調査ではインストール、スキルの更新、本体コードの変更を行わなかった。

## 候補の役割と適合性

| 用途 | 正確な名前・提供元 | 読んだ内容と marginplyr への判断 |
| --- | --- | --- |
| 日常のコード整理 | `clean-code` — sickn33/agentic-awesome-skills（本文の出典表記は ClawForge） | 命名、一つの責務、実装の隠蔽、重複、不要なコメントを扱っていた。リポジトリの規約を優先し、振る舞いを維持する注意書きがあった。一方で「20 行未満」「引数 0 が理想」「null を返さない」等は R の公開契約に機械的に適用できないと判断した。[本文][clean-code] |
| 日常の変更後の単純化 | `code-simplifier` — Anthropic | 変更した箇所を対象に、挙動維持、冗長な抽象化やネストの削減、読みやすさを重視していた。ただし `SKILL.md` ではなく `agents/code-simplifier.md` という Claude Code の agent 定義で、`model: opus` と ES modules・React 固有の指示があった。Codex にそのまま移せる汎用スキルとは扱わなかった。[定義][anthropic-simplifier] |
| 設計改善箇所の探索 | `improve-codebase-architecture` — Matt Pocock | 変更履歴から重点箇所を絞り、浅い module、呼出側への漏出、理解に必要なファイル往復を調べ、HTML の候補一覧を作って選択後に設計を掘り下げる手順だった。実装変更を行う手順ではなかった。既存 ADR を読む指示があり、R 固有のクラス階層を強制しなかった。[本文][matt-improve] |
| 選んだ module の設計 | `codebase-design` — Matt Pocock | function、class、package のいずれも module として扱い、引数だけでなく不変条件、順序、エラー、設定、性能まで interface に含めていた。二つの実装が正当化される箇所に seam を置く考え方は、複数 backend を持つ marginplyr に適合すると判断した。単独で探索を実行する手順ではなく参照用だった。[本文][matt-design] |
| 安全な変更の検証 | `test-driven-development` — obra/superpowers | failing test → 最小実装 → green の後に整理する手順だった。振る舞いのテストを保った小刻みな変更に有用だったが、refactor も TDD の適用対象とし、失敗を先に確認すること、例外には人への確認、最後には全 suite を要求する強い方針だった。marginplyr の検証範囲はローカル方針で判断する必要があった。[本文][obra-tdd] |
| 差分の品質ゲート | `code-review` — Matt Pocock | Standards と Spec を独立した agent で評価し、merge-base からの差分を固定する手順だった。臭いは判断の補助であり、規約違反と区別し、既存規約を優先する設計だった。新たな整理スキルより、不要な一般化や責務の混在を変更ごとに防ぐ用途に適合すると判断した。[本文][matt-review] |

### 設計候補の注意点

`improve-codebase-architecture` の依存先は `codebase-design`、`grilling`、
`domain-modeling` だった。調査時の upstream は `GLOSSARY.md` と `docs/adr/`
を参照していた。marginplyr の `CONTEXT.md` と `design/adr/` に読み替える必要が
あると判断した。候補選択後には語彙ファイルの変更も指示されており、探索だけの
用途と実際の運用変更は区別する必要があった。HTML は Tailwind と Mermaid の CDN
を使う指示だった。[本文][matt-improve]

`codebase-design` の補助文書は、古い浅い module のテストを新しい interface の
テストで置き換える指示と、代替 interface を三つ以上の agent で比較する手順を
持っていた。既存テストを一律に捨てる根拠とはせず、維持すべき backend の契約や
回帰防止を先に確認すべきだと判断した。
[DEEPENING.md][matt-deepening]、[DESIGN-IT-TWICE.md][matt-twice]

調査時の upstream `tdd`（Matt Pocock）は、古い紹介文の
red-green-refactor と異なり、refactor をループから除外して review 段階に置いていた。
また、テストを書く前に seam を利用者と確認する指示があった。Superpowers の
`test-driven-development` と同じ手順とは扱わなかった。[Matt の tdd][matt-tdd]

## 比較したが優先度を下げた候補

| 候補 | 調査時の確認結果 |
| --- | --- |
| `software-architecture` — sickn33/agentic-awesome-skills | 責務分離、domain 固有の命名、DDD、Clean Architecture を扱っていた。一方で npm 優先、arrow functions、80 行で function 分割・200 行で file 分割等の指示が混在した。単一 context の R パッケージに層や bounded context を追加する理由にはならないと判断した。[本文][software-architecture] |
| `architecture-patterns` — wshobson/agents | Clean / Hexagonal Architecture、DDD、内向き依存、port と adapter を扱っていた。主な適用先は backend service、monolith、ORM、controller だった。局所的な依存方向の相談には使えても、パッケージ全体をその構造に変える推奨にはしなかった。[本文][wsh-architecture] |
| `refactor-clean` — wshobson/agents | `SKILL.md` ではなく `commands/refactor-clean.md` だった。SOLID、重複削減、命名を扱った一方、「全 method 20 行未満」「引数 3 以下」「coverage 80% 超」等の固定基準や dashboard まで含み、R パッケージには広すぎると判断した。[command][wsh-refactor] |
| `systematic-debugging` — obra/superpowers | 原因調査、動作する例との比較、一仮説ずつの検証、最小修正を順に行う手順だった。壊れた箇所を直す用途であり、日常の整理や設計探索の第一候補とはしなかった。[本文][obra-debug] |
| `requesting-code-review` / `receiving-code-review` — obra/superpowers | 前者は base/head と要件を渡す reviewer agent の起動、後者は指摘を実際のコードと照合してから変更する手順だった。独立した `code-review` というスキル名は、このリポジトリの調査した tree にはなかった。レビュー補助の候補だが、marginplyr が指定した Matt 系の review を置き換える必要は見出さなかった。[依頼][obra-request]、[受領][obra-receive]、[tree][obra-tree] |
| `vercel-composition-patterns` — Vercel | 正確な frontmatter 名はこれで、配置 directory 名は `composition-patterns` だった。boolean props、compound components、context provider、React 19 が対象だった。R パッケージには適用対象が合わないと判断した。[本文][vercel-composition] |

`request-refactor-plan` は調査した Matt Pocock の `main` には存在しなかった。
削除 commit の changeset は、これが `to-spec` と `improve-codebase-architecture`
に吸収されたと明記していた。検索結果や skills.sh の累積表示に旧名が残っても、
新規導入の推奨候補にはしなかった。[削除 commit][matt-removal]

## 人気の証拠と限界

以下は 2026-11-09 20:49 JST ごろに GitHub REST API の
`GET /repos/{owner}/{repo}` から採取した `stargazers_count` だった。
リポジトリ全体への star であり、個々のスキルの採用数や品質ではなかった。

| 配布リポジトリ | stars | 一次情報 |
| --- | ---: | --- |
| mattpocock/skills | 319,122 | [GitHub API](https://api.github.com/repos/mattpocock/skills) |
| obra/superpowers | 314,818 | [GitHub API](https://api.github.com/repos/obra/superpowers) |
| sickn33/agentic-awesome-skills | 49,393 | [GitHub API](https://api.github.com/repos/sickn33/agentic-awesome-skills) |
| anthropics/claude-plugins-official | 46,532 | [GitHub API](https://api.github.com/repos/anthropics/claude-plugins-official) |
| wshobson/agents | 42,344 | [GitHub API](https://api.github.com/repos/wshobson/agents) |
| vercel-labs/agent-skills | 33,750 | [GitHub API](https://api.github.com/repos/vercel-labs/agent-skills) |

旧名 `sickn33/antigravity-awesome-skills` への repository API は、調査時には
`sickn33/agentic-awesome-skills` を返した。上表は移転先の正式名で記録した。
[旧名の API](https://api.github.com/repos/sickn33/antigravity-awesome-skills)

スキル単位の補助証拠として、同日の skills.sh 表示では
`improve-codebase-architecture` が `1.2M`、`codebase-design` が `930.2K`、
`code-review` が `869.9K` installs だった。これはサイトが丸めて表示した値で、
利用者数・継続利用数・R への適合性を示す値とは扱わなかった。別ページの値には
更新時刻の差も見られたため、正確な個数へ換算しなかった。
[探索](https://www.skills.sh/mattpocock/skills/improve-codebase-architecture)、
[設計](https://www.skills.sh/mattpocock/skills/codebase-design)、
[review](https://www.skills.sh/mattpocock/skills/code-review)

Anthropic の marketplace は Code Simplifier を `346,763` installs と表示していた。
これは当該 plugin の表示値であり、GitHub 全体の star や汎用 `SKILL.md` の
導入数とは区別した。[公式 marketplace](https://claude.com/marketplace/plugins/code-simplifier)

## marginplyr での使い分けに関する判断

2026-11-09 に読んだ `CONTEXT.md` と既存設計に対しては、次の順番が適切だと
判断した。これは調査結果に基づく提案であり、導入や設計変更の決定ではなかった。

1. 日常の変更では `clean-code` の命名・凝集度・冗長性の観点を参考にし、既存の
   コメント契約と R の公開引数を優先する。
2. 構造的な困りごとがあれば `improve-codebase-architecture` で候補を探し、
   `codebase-design` で一つの interface を検討する。
3. 挙動を保つ小さな差分として実装し、必要な回帰テストを保つ。一般スキルの
   blanket なテスト指示ではなく、[ローカル検証方針](../design/agents/local-checks.md)
   が定める検証範囲を使う。
4. 仕上げには既に指定された `code-review` を使い、規約と要件を別々に評価する。

セッションのスキル一覧には `codebase-design`、`tdd`、`code-review` があった。
`improve-codebase-architecture/SKILL.md` はローカルのスキル配置場所には存在したが、
当該一覧には含まれなかった。ファイルの存在と、そのセッションで公開された
スキル名は区別した。

一般スキルの「内部をテストしない」「コメントを減らす」という指示は、
marginplyr の正当化された構造検査、内部 function の契約 header、式固有の
`nolint` 理由を削除する根拠にはならないと判断した。
[Structural gates](../design/architecture.md)、[Code comments / Linting](../AGENTS.md)

既存の Capture/admit → prepare → execute → finalize、opaque な Margin operation、
backend adapter の責務は、[既存 architecture](../design/architecture.md) と ADR に
根拠があった。調査したスキルの一般論だけを理由に、新たな service 層・repository
層や細かな wrapper を足す必要は見出さなかった。

## 読み取った一次資料

リンクは調査時に取得した default-branch commit に固定した。本文の出典表記と
配布者は区別した。sickn33 の集約カタログの人気を、ClawForge や community
執筆者個人の人気とは扱わなかった。

[matt-improve]: https://github.com/mattpocock/skills/blob/4ae2317ddb22b046e26455e4f9c9b3aaaf62212d/skills/engineering/improve-codebase-architecture/SKILL.md
[matt-design]: https://github.com/mattpocock/skills/blob/4ae2317ddb22b046e26455e4f9c9b3aaaf62212d/skills/engineering/codebase-design/SKILL.md
[matt-deepening]: https://github.com/mattpocock/skills/blob/4ae2317ddb22b046e26455e4f9c9b3aaaf62212d/skills/engineering/codebase-design/DEEPENING.md
[matt-twice]: https://github.com/mattpocock/skills/blob/4ae2317ddb22b046e26455e4f9c9b3aaaf62212d/skills/engineering/codebase-design/DESIGN-IT-TWICE.md
[matt-tdd]: https://github.com/mattpocock/skills/blob/4ae2317ddb22b046e26455e4f9c9b3aaaf62212d/skills/engineering/tdd/SKILL.md
[matt-review]: https://github.com/mattpocock/skills/blob/4ae2317ddb22b046e26455e4f9c9b3aaaf62212d/skills/engineering/code-review/SKILL.md
[matt-removal]: https://github.com/mattpocock/skills/commit/c66bdeeee002d81e3f8b21403c07f9a0d7bea6da
[clean-code]: https://github.com/sickn33/agentic-awesome-skills/blob/828257c3037dc7c3c3ec987decc5a603025a0ce5/skills/clean-code/SKILL.md
[software-architecture]: https://github.com/sickn33/agentic-awesome-skills/blob/828257c3037dc7c3c3ec987decc5a603025a0ce5/skills/software-architecture/SKILL.md
[anthropic-simplifier]: https://github.com/anthropics/claude-plugins-official/blob/0412be76f50ab4f4f8abff3753efa9269fbbca5b/plugins/code-simplifier/agents/code-simplifier.md
[obra-tdd]: https://github.com/obra/superpowers/blob/22f0b463e7a5e579904dd6211a520321035526f7/skills/test-driven-development/SKILL.md
[obra-debug]: https://github.com/obra/superpowers/blob/22f0b463e7a5e579904dd6211a520321035526f7/skills/systematic-debugging/SKILL.md
[obra-request]: https://github.com/obra/superpowers/blob/22f0b463e7a5e579904dd6211a520321035526f7/skills/requesting-code-review/SKILL.md
[obra-receive]: https://github.com/obra/superpowers/blob/22f0b463e7a5e579904dd6211a520321035526f7/skills/receiving-code-review/SKILL.md
[obra-tree]: https://api.github.com/repos/obra/superpowers/git/trees/22f0b463e7a5e579904dd6211a520321035526f7?recursive=1
[wsh-architecture]: https://github.com/wshobson/agents/blob/9c61ab243def98f72da12abd41cfcf5bcd91af9d/plugins/backend-development/skills/architecture-patterns/SKILL.md
[wsh-refactor]: https://github.com/wshobson/agents/blob/9c61ab243def98f72da12abd41cfcf5bcd91af9d/plugins/code-refactoring/commands/refactor-clean.md
[vercel-composition]: https://github.com/vercel-labs/agent-skills/blob/1a2c79d4e6c4a2283dc2c614b6538cbe1ed5ca62/skills/composition-patterns/SKILL.md
