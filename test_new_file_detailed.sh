#!/bin/bash
echo "Testing new comprehensive test file..."
cd /home/runner/work/Azimuth/Azimuth/azimuth
echo "File exists: $(ls -la azimuth_comprehensive_new_tests.mbt)"
echo "Checking file content..."
head -20 azimuth_comprehensive_new_tests.mbt
echo "Running test..."
../moon test 2>&1 | grep -A 20 "azimuth_comprehensive_new_tests" || echo "Test file not found in output"