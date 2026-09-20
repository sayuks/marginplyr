# R-hub Quarto Setup Portability in Issue #588

Investigated: 2026-09-21

## Question

Why did issue #588 replace `quarto-dev/quarto-actions/setup@v2` in the R-hub
Linux-container job? Why does marginplyr need Quarto there, how did `setup@v2`
select the latest Quarto release, and why did it call `jq` without installing
it?

## Conclusion

The replacement was not a different way to run `jq`. It removed `jq` from this
call path entirely.

marginplyr needs the Quarto CLI because its `.qmd` vignettes declare
`quarto::html` as their vignette engine and `DESCRIPTION` declares both
`VignetteBuilder: quarto` and the CLI as a system requirement. The R-hub
workflow deliberately installs the local package during dependency setup, so
Quarto must already be on `PATH` before `r-hub/actions/setup-deps@v1` builds
that package.

At the commit used by the failed runs, `setup@v2` was a composite action that
defaulted `version` to `release`. On Linux it selected a `.deb`, fetched
Quarto's live `_download.json`, extracted `.version` with `jq`, downloaded the
matching GitHub release asset with `wget`, and installed it with `sudo apt`.
Both selected R-hub job containers lacked `jq`, so the action stopped before
dependency setup. Installing `jq` alone would not have made the action portable:
the Atlas container was Fedora, while the next action stage was hard-coded to
a Debian package and `apt`.

The upstream record does not state a positive design reason for omitting a
`jq` installation. It shows instead that upstream PR #132 deliberately removed
the heavier `gh` dependency and explicitly acknowledged that `jq` remained.
The best-supported explanation is therefore an environment assumption, not an
installer guarantee: the upstream test matrix used GitHub-hosted runners, whose
Ubuntu image included `jq`, while marginplyr's R-hub steps ran inside job
containers that did not inherit host tools. This paragraph is an inference
from the cited upstream PR, test matrix, runner image, GitHub execution model,
and the two observed failures; upstream did not document it as a policy.

## How Quarto became required in this workflow

Commit
[`be9ee53`](https://github.com/sayuks/marginplyr/commit/be9ee53d647929534802f43a91673eefe7adc468)
made the shared guide an R package vignette on 2026-07-25. The candidate later
checked by R-hub recorded `VignetteBuilder: quarto` and stated that the Quarto
CLI was needed to build `.qmd` vignettes
([`DESCRIPTION`](https://github.com/sayuks/marginplyr/blob/f5c0cc2b63c4f05e9a508a362438ed7e2b4a538c/DESCRIPTION#L61-L65));
the vignette itself named `quarto::html`
([`get_started.qmd`](https://github.com/sayuks/marginplyr/blob/f5c0cc2b63c4f05e9a508a362438ed7e2b4a538c/vignettes/get_started.qmd#L1-L6)).

Commit
[`9ac8768`](https://github.com/sayuks/marginplyr/commit/9ac8768e399ca84296c2d6633871aa01d8b9685b)
then added `setup@v2` to both R-hub job families. Its workflow comment states
the repository's reason: R-hub dependency setup installs the R package but not
the CLI required by the vignette engine. Commit
[`960fe00`](https://github.com/sayuks/marginplyr/commit/960fe002f7d5a6108e228e1f046540e2f4eb91f9)
set `RHUB_ACTIONS_INSTALL_LOCAL_PACKAGE=true` because Quarto renders the
vignettes in a child R process that must be able to load marginplyr.

The referenced R-hub action confirms both halves. With that environment value
it changes the requested package from `deps::.` to `local::.`, and it passes
`install-quarto: false` to `setup-r-dependencies`
([`setup-deps@v1`](https://github.com/r-hub/actions/blob/cf6c8eff3145ef225104294e6b0c023de4bba9c6/setup-deps/action.yaml#L68-L105)).
Quarto therefore had to be installed in an earlier workflow step; it was not a
redundant installation already performed by R-hub.

## What `setup@v2` actually did

The two failed Actions logs resolved the moving `@v2` tag to commit
`8a96df13519ee81fd526f2dfca5962811136661b`. `@v2` selected the action
implementation; it did not mean Quarto version 2. The action's separate
`version` input defaulted to `release`, described as the latest stable Quarto
([input definition](https://github.com/quarto-dev/quarto-actions/blob/8a96df13519ee81fd526f2dfca5962811136661b/setup/action.yml#L4-L12)).

On non-Windows runners, the `release` branch ran:

```sh
version=$(curl https://quarto.org/docs/download/_download.json | jq -r '.version')
wget https://github.com/quarto-dev/quarto-cli/releases/download/v$version/quarto-$version-${BUNDLE_EXT}
```

The pinned implementation shows this resolution beside the pre-release and
exact-version branches
([download implementation](https://github.com/quarto-dev/quarto-actions/blob/8a96df13519ee81fd526f2dfca5962811136661b/setup/action.yml#L46-L75)).
For every Linux runner it first chose an amd64 or arm64 `.deb`, then installed
that package using `sudo apt`
([bundle selection and installation](https://github.com/quarto-dev/quarto-actions/blob/8a96df13519ee81fd526f2dfca5962811136661b/setup/action.yml#L19-L45),
[`sudo apt`](https://github.com/quarto-dev/quarto-actions/blob/8a96df13519ee81fd526f2dfca5962811136661b/setup/action.yml#L76-L104)).

This also explains why supplying an exact Quarto `version` would bypass `jq`
but would not solve Atlas: it would skip the JSON lookup while preserving the
Linux `.deb` and `sudo apt` installation.

## Why `jq` was assumed rather than installed

The upstream record most directly addressing the dependency is merged PR
[#132](https://github.com/quarto-dev/quarto-actions/pull/132) from 2025-10-30.
That one-line change replaced `gh release download` with `wget`; the PR says
that `gh` was installed on default GitHub runners but was not guaranteed in a
default Docker container. It then states explicitly that the existing `jq`
dependency remained because it was harder to remove and used in more cases.
The maintainer approved and merged the PR, so #132 was not a `jq` fix and is
not an unmerged proposal; it knowingly improved only the `gh` part
([merged commit](https://github.com/quarto-dev/quarto-actions/commit/3e5a86f3ef0b520f38984b4e5c84f068f61f2a64)).

No dependency declaration or installation for `jq` appears in the action.
Its tests at the exact `@v2` commit exercised `macos-latest`,
`macos-15-intel`, `windows-latest`, `ubuntu-latest`, and
`ubuntu-22.04-arm`, not a job-container matrix
([test workflow](https://github.com/quarto-dev/quarto-actions/blob/8a96df13519ee81fd526f2dfca5962811136661b/.github/workflows/test.yaml#L24-L60)).
At the time of the marginplyr failures, `ubuntu-latest` mapped to Ubuntu 24.04
([runner-images mapping](https://github.com/actions/runner-images/blob/a99056ad72b0f921b96cd0ac7ac4106f86095671/README.md#L20-L27)),
and the published Ubuntu 24.04 image listed `jq` 1.7
([installed software](https://github.com/actions/runner-images/blob/cf6f08e1bc2b902d44a33500d2c4c63b65882bce/images/ubuntu/Ubuntu2404-Readme.md#L78-L86)).
The image build explicitly installed it with `apt-get install jq`
([image build script](https://github.com/actions/runner-images/blob/dff7cf5f1d89bdac4336cd261875e553582fe769/images/ubuntu/scripts/build/configure-apt.sh#L76-L82)).

That host inventory did not apply to the R-hub steps. The workflow used
`runs-on: ubuntu-latest` only as the Docker host and set
`jobs.linux-containers.container.image` to the R-hub image. GitHub specifies
that `jobs.<job_id>.container` runs steps that do not themselves specify a
container inside the job container
([workflow syntax](https://docs.github.com/en/actions/reference/workflows-and-actions/workflow-syntax#jobsjob_idcontainer)).
Because `setup@v2` is a composite action made of shell steps, its `curl | jq`
pipeline executed inside the R-hub image. The official R-hub inventory
identified `atlas` as Fedora 42 and `ubuntu-clang` as Ubuntu 22.04
([R-hub containers](https://r-hub.github.io/containers/)).

The result was observed, not hypothetical. Both candidate-SHA runs ended in
the `Download Quarto` substep with `jq: command not found` and exit 127:

- [Atlas run 35287877365](https://github.com/sayuks/marginplyr/actions/runs/35287877365)
- [ubuntu-clang run 35288014219](https://github.com/sayuks/marginplyr/actions/runs/35288014219)

Neither reached `r-hub/actions/setup-deps@v1` or the package check. Issue
[#588](https://github.com/sayuks/marginplyr/issues/588) therefore correctly
classified them as workflow-integration failures rather than package-check
results.

## Why #588 used a tarball instead

Issue #588 required one local replacement only in `linux-containers`. The
implementation merged through PR
[#589](https://github.com/sayuks/marginplyr/pull/589) as initial commit
[`46eb4fc`](https://github.com/sayuks/marginplyr/commit/46eb4fc67f245ec64c898134df956d5305292f0e),
followed by two review-response commits that clarified the comment and
strengthened the regression checks. The final step:

1. maps `x86_64`/`amd64` and `aarch64`/`arm64` explicitly;
2. downloads `https://quarto.org/download/latest/quarto-linux-<arch>.tar.gz`;
3. extracts it into a temporary directory;
4. adds the archive's `bin` directory to `GITHUB_PATH`; and
5. records both `quarto --version` and `quarto pandoc --version`.

The final PR version is visible at
[`fa780fd`](https://github.com/sayuks/marginplyr/blob/fa780fde5d9c114a3a731595f65be198a6d931eb/.github/workflows/rhub.yaml#L62-L106).
It contains no `jq`, `sudo`, or distribution package-manager invocation. It
uses the official latest-release redirect as the selector, so there is no JSON
metadata to parse. On 2026-09-21, an HTTP HEAD request to the amd64 URL
redirected to the official Quarto 1.10.18 GitHub release asset; the Quarto
download page also listed both amd64 and arm64 tarballs as stable downloads
([official download page](https://quarto.org/docs/download/index.html)).

This preserved the repository's floating-current-stable policy. It also
preserved the setup action's HTTPS-only trust model rather than dynamically
resolving and parsing checksum metadata; #588 records that tradeoff explicitly.
The ordinary PR lint contract was extended so regeneration by
`rhub::rhub_setup()` fails if it restores any Quarto setup action to the Linux
container job, removes either architecture mapping, moves installation after
dependency setup, or adds `jq` or a package manager back
([final verifier](https://github.com/sayuks/marginplyr/blob/fa780fde5d9c114a3a731595f65be198a6d931eb/.github/scripts/verify-cran-release.R#L503-L614)).
The non-container `other-platforms` job intentionally retained `setup@v2`.

## Validation boundary

PR #589 recorded passing local contract checks, the full review-ready check,
and two parallel code reviews. Issue #588 had no comments, and neither it nor
the PR recorded the two requested post-change R-hub runs. The repository's
Actions history contained no `rhub.yaml` dispatch at the PR head or merge SHA
as of this investigation. Release issue #554 likewise recorded that no
external release check was dispatched when the repair superseded the old
candidate
([release record](https://github.com/sayuks/marginplyr/issues/554#issuecomment-5731561874)).

Thus the failure cause and the portability improvement are established by
source, logs, and local verification, but the issue's acceptance criterion for
successful Atlas and ubuntu-clang reruns was not evidenced before #588 was
closed.
