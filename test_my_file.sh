#!/bin/bash

echo "Testing my new_standard_moonbit_test_cases.mbt file..."
cd /home/runner/work/Azimuth/Azimuth/src/azimuth
/home/runner/work/Azimuth/Azimuth/moon test new_standard_moonbit_test_cases.mbt 2>&1 | grep -A 15 "new_standard_moonbit_test_cases" || echo "Test file not found in output, checking if it exists..."

ls -la new_standard_moonbit_test_cases.mbt