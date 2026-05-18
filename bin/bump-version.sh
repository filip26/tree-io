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

echo "Version bump complete!"
