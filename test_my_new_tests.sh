#!/bin/bash
echo "Testing azimuth_high_quality_standard_tests.mbt..."
cd /home/runner/work/Azimuth/Azimuth
./moon test src/azimuth/azimuth_high_quality_standard_tests.mbt 2>&1 | grep -A 20 "azimuth_high_quality_standard_tests.mbt" || echo "Test file not found in output"
echo "Checking if test file exists in package..."
ls -la src/azimuth/azimuth_high_quality_standard_tests.mbt