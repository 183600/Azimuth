#!/bin/bash
# 验证新创建的增强MoonBit测试文件

echo "验证新创建的增强测试文件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/src/new_enhanced_moonbit_tests.mbt" ]; then
    echo "✓ 测试文件存在: new_enhanced_moonbit_tests.mbt"
else
    echo "✗ 测试文件不存在: new_enhanced_moonbit_tests.mbt"
    exit 1
fi

# 检查文件中是否包含测试用例
test_count=$(grep -c 'test "' /home/runner/work/Azimuth/Azimuth/src/new_enhanced_moonbit_tests.mbt)
echo "✓ 找到 $test_count 个测试用例"

# 检查是否已添加到moon.pkg.json
if grep -q "new_enhanced_moonbit_tests.mbt" /home/runner/work/Azimuth/Azimuth/src/moon.pkg.json; then
    echo "✓ 测试文件已添加到moon.pkg.json"
else
    echo "✗ 测试文件未添加到moon.pkg.json"
    exit 1
fi

# 检查测试文件语法（简单检查）
if grep -q 'pub test' /home/runner/work/Azimuth/Azimuth/src/new_enhanced_moonbit_tests.mbt; then
    echo "✓ 测试文件包含正确的测试语法"
else
    echo "✗ 测试文件缺少正确的测试语法"
    exit 1
fi

# 检查测试文件是否包含azimuth相关的测试
if grep -q 'azimuth::' /home/runner/work/Azimuth/Azimuth/src/new_enhanced_moonbit_tests.mbt; then
    echo "✓ 测试文件包含azimuth相关的测试"
else
    echo "✗ 测试文件缺少azimuth相关的测试"
    exit 1
fi

# 检查测试用例的主题多样性
if grep -q "实时数据流处理" /home/runner/work/Azimuth/Azimuth/src/new_enhanced_moonbit_tests.mbt; then
    echo "✓ 包含实时数据流处理测试"
else
    echo "✗ 缺少实时数据流处理测试"
fi

if grep -q "度量聚合" /home/runner/work/Azimuth/Azimuth/src/new_enhanced_moonbit_tests.mbt; then
    echo "✓ 包含度量聚合测试"
else
    echo "✗ 缺少度量聚合测试"
fi

if grep -q "分布式追踪" /home/runner/work/Azimuth/Azimuth/src/new_enhanced_moonbit_tests.mbt; then
    echo "✓ 包含分布式追踪测试"
else
    echo "✗ 缺少分布式追踪测试"
fi

if grep -q "多语言国际化" /home/runner/work/Azimuth/Azimuth/src/new_enhanced_moonbit_tests.mbt; then
    echo "✓ 包含多语言国际化测试"
else
    echo "✗ 缺少多语言国际化测试"
fi

if grep -q "内存管理" /home/runner/work/Azimuth/Azimuth/src/new_enhanced_moonbit_tests.mbt; then
    echo "✓ 包含内存管理测试"
else
    echo "✗ 缺少内存管理测试"
fi

if grep -q "错误恢复" /home/runner/work/Azimuth/Azimuth/src/new_enhanced_moonbit_tests.mbt; then
    echo "✓ 包含错误恢复测试"
else
    echo "✗ 缺少错误恢复测试"
fi

if grep -q "配置管理" /home/runner/work/Azimuth/Azimuth/src/new_enhanced_moonbit_tests.mbt; then
    echo "✓ 包含配置管理测试"
else
    echo "✗ 缺少配置管理测试"
fi

if grep -q "数据压缩" /home/runner/work/Azimuth/Azimuth/src/new_enhanced_moonbit_tests.mbt; then
    echo "✓ 包含数据压缩测试"
else
    echo "✗ 缺少数据压缩测试"
fi

# 检查测试用例数量是否在要求范围内（5-10个）
if [ $test_count -ge 5 ] && [ $test_count -le 10 ]; then
    echo "✓ 测试用例数量符合要求（5-10个）"
else
    echo "✗ 测试用例数量不符合要求（当前: $test_count，要求: 5-10个）"
    exit 1
fi

echo "所有验证通过！新创建的增强测试文件符合要求。"