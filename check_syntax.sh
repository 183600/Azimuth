#!/bin/bash

# Simple syntax check for the new test file
echo "Checking syntax of azimuth_specialized_comprehensive_tests.mbt"

# Check if the file exists
if [ ! -f "azimuth_specialized_comprehensive_tests.mbt" ]; then
    echo "Error: File not found"
    exit 1
fi

# Basic syntax checks
echo "Checking for basic syntax patterns..."

# Check for test blocks
test_count=$(grep -c "^test " azimuth_specialized_comprehensive_tests.mbt)
echo "Found $test_count test blocks"

# Check for assert statements
assert_count=$(grep -c "assert_" azimuth_specialized_comprehensive_tests.mbt)
echo "Found $assert_count assert statements"

# Check for balanced braces
open_braces=$(grep -o "{" azimuth_specialized_comprehensive_tests.mbt | wc -l)
close_braces=$(grep -o "}" azimuth_specialized_comprehensive_tests.mbt | wc -l)
echo "Found $open_braces open braces and $close_braces close braces"

if [ $open_braces -eq $close_braces ]; then
    echo "Braces are balanced"
else
    echo "Warning: Braces are not balanced"
fi

# Check for proper test structure
echo "Checking test structure..."
for i in $(seq 1 $test_count); do
    test_name=$(grep "^test " azimuth_specialized_comprehensive_tests.mbt | head -n $i | tail -n 1 | sed 's/test "\(.*\)"/\1/')
    echo "Test $i: $test_name"
done

echo "Syntax check completed"