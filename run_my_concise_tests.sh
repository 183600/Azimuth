#!/bin/bash
echo "Running azimuth_concise_tests.mbt specifically..."
./moon test 2>&1 | grep -A 15 "Checking azimuth_concise_tests.mbt"