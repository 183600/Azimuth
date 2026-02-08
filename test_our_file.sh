#!/bin/bash
cd /home/runner/work/Azimuth/Azimuth
echo "Testing azimuth_standard_comprehensive_tests.mbt..."
./moon test src/azimuth/azimuth_standard_comprehensive_tests.mbt 2>&1 | grep -A 20 "Checking azimuth_standard_comprehensive_tests.mbt"