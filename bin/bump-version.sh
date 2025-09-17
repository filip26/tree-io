#!/usr/bin/env bash
set -euo pipefail

# Usage: ./bump-version.sh 1.2.3
NEW_VERSION="$1"

if [[ -z "$NEW_VERSION" ]]; then
  echo "Usage: $0 <new-version>"
  exit 1
fi

echo "Bumping version to $NEW_VERSION..."

# 1. Update main pom.xml
echo "Updating main pom.xml..."
mvn versions:set -DnewVersion="$NEW_VERSION" -DgenerateBackupPoms=false

# 2. Update submodules
SUBMODULES=("api" "jakarta-adapter" "jackson2-adapter" "cbor-adapter")

for MODULE in "${SUBMODULES[@]}"; do
  echo "Updating $MODULE/pom.xml..."
  mvn versions:set -DnewVersion="$NEW_VERSION" -DgenerateBackupPoms=false -f "$MODULE/pom.xml"
done

# 3. Update tree-io-api dependency in adapters
for MODULE in "jakarta-adapter" "cbor-adapter" "jackson2-adapter"; do
  echo "Updating tree-io-api dependency in $MODULE/pom.xml..."
  mvn versions:use-dep-version \
    -Dincludes=com.apicatalog:tree-io-api \
    -DdepVersion="$NEW_VERSION" \
    -DforceVersion=true \
    -DgenerateBackupPoms=false \
    -f "$MODULE/pom.xml"
done

echo "Version bump complete!"

