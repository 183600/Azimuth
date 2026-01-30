#!/bin/bash

echo "Testing only our premium advanced tests..."

cd /home/runner/work/Azimuth/Azimuth/test

# Create a temporary package.json with only our test file
cp moon.pkg.json moon.pkg.json.backup

# Create a minimal test configuration
cat > moon.pkg.json.minimal << 'EOF'
{
  "name": "azimuth_test",
  "test-import": [
    "azimuth",
    "moonbitlang/core/builtin"
  ],
  "test": [
    "azimuth_premium_advanced_tests.mbt"
  ]
}
EOF

# Replace the package.json temporarily
cp moon.pkg.json.minimal moon.pkg.json

echo "Running our test file..."
timeout 60s ./moon test 2>&1

# Restore the original package.json
cp moon.pkg.json.backup moon.pkg.json

echo "Test completed."