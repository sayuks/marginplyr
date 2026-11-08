#!/usr/bin/env bash
# This bootstrap runs before R is installed. Fetch the stable release through
# the asset API because browser download URLs can fail independently with 504.
set -euo pipefail

case "$RUNNER_OS" in
  Linux)
    case "$RUNNER_ARCH" in
      ARM64|ARM) suffix=linux-arm64.deb ;;
      X64) suffix=linux-amd64.deb ;;
      *) echo "Unsupported Quarto runner architecture: $RUNNER_ARCH" >&2; exit 1 ;;
    esac
    ;;
  macOS) suffix=macos.pkg ;;
  *) echo "Unsupported Quarto runner OS: $RUNNER_OS" >&2; exit 1 ;;
esac

version=$(gh api repos/quarto-dev/quarto-cli/releases/latest --jq .tag_name)
if [[ ! $version =~ ^v[0-9]+[.][0-9]+[.][0-9]+$ ]]; then
  echo "Invalid stable Quarto version: $version" >&2
  exit 1
fi
asset="quarto-${version#v}-$suffix"
asset_url=$(gh api "repos/quarto-dev/quarto-cli/releases/tags/$version" \
  --jq ".assets[] | select(.name == \"$asset\") | .url")
if [[ ! $asset_url =~ ^https://api[.]github[.]com/repos/quarto-dev/quarto-cli/releases/assets/[0-9]+$ ]]; then
  echo "Missing or invalid Quarto asset: $asset" >&2
  exit 1
fi

work=$(mktemp -d "$RUNNER_TEMP/quarto.XXXXXX")
trap 'rm -rf "$work"' EXIT
gh api -H 'Accept: application/octet-stream' "$asset_url" > "$work/$asset"
test -s "$work/$asset"
case "$RUNNER_OS" in
  Linux) sudo dpkg -i "$work/$asset" ;;
  macOS) sudo installer -pkg "$work/$asset" -target / ;;
esac
installed_version=$(quarto --version)
echo "Quarto $installed_version"
test "$installed_version" = "${version#v}"
