#!/bin/bash

# Test script for new core enhanced tests
echo "Testing new Azimuth core enhanced tests..."

# Try to compile just the new test file with the main azimuth module
echo "Compiling azimuth_core_enhanced_tests.mbt..."

# Create a temporary test directory
mkdir -p temp_test
cp azimuth/azimuth.mbt temp_test/
cp azimuth_core_enhanced_tests.mbt temp_test/

# Create a minimal moon.pkg.json for testing
cat > temp_test/moon.pkg.json << 'EOF'
{
  "name": "azimuth-test",
  "version": "0.1.0",
  "description": "Temporary test package",
  "target": "wasm-gc",
  "source": ["."],
  "deps": {
    "moonbitlang/core/assertion": "*"
  },
  "test": {
    "test": ["azimuth_core_enhanced_tests.mbt"]
  }
}
EOF

cd temp_test

# Try to run the tests
echo "Running tests..."
moon test 2>&1 | tee test_output.log

# Check if tests ran successfully
if grep -q "All tests passed" test_output.log; then
    echo "✅ Tests completed successfully!"
else
    echo "❌ Tests failed or had issues"
    echo "Checking for syntax errors..."
    moon check 2>&1 | tee check_output.log
fi

# Return to original directory
cd ..

# Show results
echo "=== Test Results ==="
cat temp_test/test_output.log

if [ -f temp_test/check_output.log ]; then
    echo "=== Check Results ==="
    cat temp_test/check_output.log
fi

# Clean up
rm -rf temp_test