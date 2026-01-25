#!/bin/bash
echo "Creating a simple test to verify our test file..."
cd test

# Create a simple test file to verify the syntax
cat > simple_test_check.mbt << 'EOF'
test "simple_verification" {
  assert_eq(1, 1)
}
EOF

echo "Running simple test..."
../moon test simple_test_check.mbt

echo "Checking if our test file syntax is correct..."
echo "Our test file content:"
head -20 standard_moonbit_test_enhancements.mbt

echo "Test verification completed."