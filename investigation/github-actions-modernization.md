# GitHub Actions modernization

Research date: 2026-07-18

## Findings

The R-specific actions are already on the supported major version. The
[`r-lib/actions` project](https://github.com/r-lib/actions) continues to
recommend `@v2`, and its current release notes state that its JavaScript
actions use Node.js 24. The explicit `use-public-rspm: true` inputs can be
removed because this is now the default on supported Linux and Windows
runners.

The general-purpose actions were behind their current releases:

| Area | Previous | Selected |
|---|---|---|
| Checkout | `actions/checkout@v4` | `actions/checkout@v7` |
| Codecov upload | `codecov/codecov-action@v5` | v7, pinned to the SHA used by the current r-lib template |
| Failure artifact | `actions/upload-artifact@v4` | `actions/upload-artifact@v7` |
| Quarto setup | `quarto-dev/quarto-actions/setup@v2` | unchanged |
| R setup/check | `r-lib/actions/*@v2` | unchanged |

`checkout` v7 is the current release and includes safer handling of fork
checkouts for privileged event types. The workflows here use ordinary
`pull_request`, so the change is compatible. See the
[`checkout` v7 release](https://github.com/actions/checkout/releases/tag/v7.0.0).

The current r-lib coverage example uses Codecov v7 at the immutable commit
`fb8b3582c8e4def4969c97caa2f19720cb33a72f`; its existing `files`,
`plugins: noop`, `disable_search`, token, and failure behavior remain valid.
See the [current r-lib coverage
template](https://raw.githubusercontent.com/r-lib/actions/v2/examples/test-coverage.yaml)
and [Codecov v7 release](https://github.com/codecov/codecov-action/releases/tag/v7.0.0).
The failure artifact update follows the current
[`upload-artifact` release](https://github.com/actions/upload-artifact/releases/tag/v7.0.1).

The lintr command itself is current: `lintr::lint_package()` with
`LINTR_ERROR_ON_LINT=true` matches the
[lintr CI guide](https://lintr.r-lib.org/articles/continuous-integration.html).

## altdoc and GitHub Pages

The previous altdoc workflow downloaded and executed a remote `run.sh`, then
used a third-party action to push generated files to `gh-pages`. The current
[GitHub Pages custom-workflow
guidance](https://docs.github.com/en/pages/getting-started-with-github-pages/using-custom-workflows-with-github-pages)
uses a Pages artifact and a dedicated deployment job instead.

The replacement therefore:

- installs R and website dependencies with `r-lib/actions`;
- builds and verifies the altdoc site on pull requests without deploying it;
- uses `actions/configure-pages@v6` and
  `actions/upload-pages-artifact@v5` on `main`;
- preserves `docs/.nojekyll` with `include-hidden-files: true`;
- deploys with `actions/deploy-pages@v5`, `pages: write`, `id-token: write`,
  and the `github-pages` environment;
- serializes production deployments with `group: pages` and does not cancel
  one already in progress.

These majors are the current Node.js 24 releases: [configure-pages
v6](https://github.com/actions/configure-pages/releases/tag/v6.0.0),
[upload-pages-artifact
v5](https://github.com/actions/upload-pages-artifact/releases/tag/v5.0.0),
and [deploy-pages
v5](https://github.com/actions/deploy-pages/releases/tag/v5.0.0).

The repository initially used the legacy `gh-pages` publishing source. Before
the first deployment of the new workflow, its Pages `build_type` must be
changed to `workflow`, as described by the
[GitHub Pages REST API](https://docs.github.com/en/rest/pages/pages).

## Maintenance and permissions

Routine CI jobs now request only `contents: read`; the Pages build additionally
requests `pages: read`, and only the deployment job receives write/OIDC
permissions. This follows GitHub's least-privilege guidance in
[Secure use reference](https://docs.github.com/en/actions/reference/security/secure-use).

Weekly Dependabot checks for the `github-actions` ecosystem were added so new
major releases are proposed automatically. See GitHub's guide to
[automatically updating
actions](https://docs.github.com/en/code-security/how-tos/secure-your-supply-chain/secure-your-dependencies/auto-update-actions).
