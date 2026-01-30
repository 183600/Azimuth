#!/bin/bash

echo "Testing azimuth_premium_advanced_tests.mbt..."

cd /home/runner/work/Azimuth/Azimuth/test

# Try to run just our test file
echo "Attempting to run our test file..."
timeout 60s ./moon test 2>&1 | grep -E "(azimuth_premium_advanced_tests|Found.*tests|test.*ok)" | head -20

echo "Test run completed."