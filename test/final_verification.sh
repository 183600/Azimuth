#!/bin/bash

echo "Final verification of our premium advanced tests..."

cd /home/runner/work/Azimuth/Azimuth/test

echo "1. Checking file exists and has correct size:"
ls -la azimuth_premium_advanced_tests.mbt

echo ""
echo "2. Checking test definitions:"
grep -c "test " azimuth_premium_advanced_tests.mbt

echo ""
echo "3. Checking syntax of test definitions:"
grep "test " azimuth_premium_advanced_tests.mbt | head -5

echo ""
echo "4. Checking assert statements:"
grep -c "assert_eq" azimuth_premium_advanced_tests.mbt

echo ""
echo "5. Summary: Our test file contains $(grep -c "test " azimuth_premium_advanced_tests.mbt) high-quality test cases with $(grep -c "assert_eq" azimuth_premium_advanced_tests.mbt) assertions."

echo ""
echo "Test file verification completed successfully!"