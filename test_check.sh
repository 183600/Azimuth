#!/bin/bash
echo "Checking test file syntax..."
./moon test 2>&1 | grep -A 10 "standard_azimuth_additional_tests"