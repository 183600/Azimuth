#!/bin/bash

echo "验证新创建的测试文件..."

# 检查测试文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/src/azimuth/test/new_standard_moonbit_tests.mbt" ]; then
    echo "✓ 测试文件存在: new_standard_moonbit_tests.mbt"
else
    echo "✗ 测试文件不存在"
    exit 1
fi

# 检查测试文件是否已添加到配置中
if grep -q "new_standard_moonbit_tests.mbt" "/home/runner/work/Azimuth/Azimuth/src/azimuth/test/moon.pkg.json"; then
    echo "✓ 测试文件已添加到 moon.pkg.json 配置中"
else
    echo "✗ 测试文件未添加到配置中"
    exit 1
fi

# 统计测试用例数量
test_count=$(grep -c "^test " "/home/runner/work/Azimuth/Azimuth/src/azimuth/test/new_standard_moonbit_tests.mbt")
echo "✓ 测试用例数量: $test_count"

if [ $test_count -le 10 ]; then
    echo "✓ 测试用例数量符合要求（不超过10个）"
else
    echo "✗ 测试用例数量超过限制"
    exit 1
fi

# 显示测试用例列表
echo ""
echo "测试用例列表："
grep "^test " "/home/runner/work/Azimuth/Azimuth/src/azimuth/test/new_standard_moonbit_tests.mbt" | sed 's/^test "/- /' | sed 's/" {$//'

echo ""
echo "验证完成！所有测试文件都已正确创建和配置。"