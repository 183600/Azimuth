#!/bin/bash
echo "=== Running tests and capturing output ==="
./moon test > test_output.log 2>&1

echo "=== Test output ==="
cat test_output.log

echo ""
echo "=== Checking if our test cases are in the file ==="
echo "Number of test cases in simple_test.mbt:"
grep -c "test \"" test/simple_test.mbt

echo ""
echo "=== Listing test cases in simple_test.mbt ==="
grep "test \"" test/simple_test.mbt