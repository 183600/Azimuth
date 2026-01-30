#!/bin/bash
echo "Testing azimuth_enhanced_test_suite_new.mbt..."
cd /home/runner/work/Azimuth/Azimuth/azimuth
../moon test 2>&1 | grep -A 20 "azimuth_enhanced_test_suite_new"