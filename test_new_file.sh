#!/bin/bash
echo "Testing new comprehensive test file..."
cd /home/runner/work/Azimuth/Azimuth/azimuth
../moon test 2>&1 | grep -A 15 "azimuth_comprehensive_new_tests"