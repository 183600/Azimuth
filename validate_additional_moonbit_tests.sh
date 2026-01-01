#!/bin/bash
# 验证新添加的MoonBit测试文件

echo "验证新创建的测试文件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/src/additional_moonbit_tests.mbt" ]; then
    echo "✓ 测试文件存在: additional_moonbit_tests.mbt"
else
    echo "✗ 测试文件不存在: additional_moonbit_tests.mbt"
    exit 1
fi

# 检查文件中是否包含测试用例
test_count=$(grep -c 'test "' /home/runner/work/Azimuth/Azimuth/src/additional_moonbit_tests.mbt)
echo "✓ 找到 $test_count 个测试用例"

# 检查是否已添加到moon.pkg.json
if grep -q "additional_moonbit_tests.mbt" /home/runner/work/Azimuth/Azimuth/src/moon.pkg.json; then
    echo "✓ 测试文件已添加到moon.pkg.json"
else
    echo "✗ 测试文件未添加到moon.pkg.json"
    exit 1
fi

# 检查测试文件语法（简单检查）
if grep -q 'pub test' /home/runner/work/Azimuth/Azimuth/src/additional_moonbit_tests.mbt; then
    echo "✓ 测试文件包含正确的测试语法"
else
    echo "✗ 测试文件缺少正确的测试语法"
    exit 1
fi

# 检查测试文件是否包含azimuth相关的测试
if grep -q 'azimuth::' /home/runner/work/Azimuth/Azimuth/src/additional_moonbit_tests.mbt; then
    echo "✓ 测试文件包含azimuth相关的测试"
else
    echo "✗ 测试文件缺少azimuth相关的测试"
    exit 1
fi

echo "所有验证通过！新添加的测试文件符合要求。"