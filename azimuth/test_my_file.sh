#!/bin/bash
echo "Testing azimuth_comprehensive_unit_tests.mbt..."

# 尝试使用moonc.js检查语法
if node ./moonc.js -single-file azimuth_comprehensive_unit_tests.mbt 2>/dev/null; then
    echo "Syntax check passed"
else
    echo "Syntax check failed"
fi

# 尝试使用简单的文本检查
echo "Checking for basic syntax elements..."
if grep -q "test " azimuth_comprehensive_unit_tests.mbt; then
    echo "Found test blocks"
else
    echo "No test blocks found"
fi

if grep -q "assert_eq" azimuth_comprehensive_unit_tests.mbt; then
    echo "Found assert_eq calls"
else
    echo "No assert_eq calls found"
fi

echo "Test file validation complete."