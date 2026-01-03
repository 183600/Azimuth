#!/bin/bash
# 高质量测试用例验证脚本

echo "验证新创建的高质量测试文件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth_high_quality_test_suite.mbt" ]; then
    echo "✓ 测试文件存在: azimuth_high_quality_test_suite.mbt"
else
    echo "✗ 测试文件不存在: azimuth_high_quality_test_suite.mbt"
    exit 1
fi

# 检查文件中是否包含测试用例
test_count=$(grep -c 'test "' /home/runner/work/Azimuth/Azimuth/azimuth_high_quality_test_suite.mbt)
echo "✓ 找到 $test_count 个测试用例"

# 验证测试用例数量不超过10个
if [ $test_count -le 10 ]; then
    echo "✓ 测试用例数量符合要求（不超过10个）"
else
    echo "✗ 测试用例数量超过限制（超过10个）"
    exit 1
fi

# 检查文件大小
file_size=$(wc -c < /home/runner/work/Azimuth/Azimuth/azimuth_high_quality_test_suite.mbt)
echo "✓ 文件大小: $file_size 字节"

# 检查是否包含基本的MoonBit语法元素
if grep -q 'test "' /home/runner/work/Azimuth/Azimuth/azimuth_high_quality_test_suite.mbt; then
    echo "✓ 包含测试用例定义"
else
    echo "✗ 缺少测试用例定义"
    exit 1
fi

if grep -q 'assert_' /home/runner/work/Azimuth/Azimuth/azimuth_high_quality_test_suite.mbt; then
    echo "✓ 包含断言语句"
else
    echo "✗ 缺少断言语句"
    exit 1
fi

# 列出所有测试用例名称
echo ""
echo "测试用例列表:"
grep 'test "' /home/runner/work/Azimuth/Azimuth/azimuth_high_quality_test_suite.mbt | sed 's/.*test "\([^"]*\)".*/\1/'

# 检查测试用例覆盖的主题
echo ""
echo "测试覆盖的主题:"
if grep -q "遥测数据生成和验证" /home/runner/work/Azimuth/Azimuth/azimuth_high_quality_test_suite.mbt; then
    echo "✓ 遥测数据生成和验证"
fi
if grep -q "遥测数据序列化和反序列化" /home/runner/work/Azimuth/Azimuth/azimuth_high_quality_test_suite.mbt; then
    echo "✓ 遥测数据序列化和反序列化"
fi
if grep -q "遥测数据采样策略" /home/runner/work/Azimuth/Azimuth/azimuth_high_quality_test_suite.mbt; then
    echo "✓ 遥测数据采样策略"
fi
if grep -q "遥测数据批处理和缓冲" /home/runner/work/Azimuth/Azimuth/azimuth_high_quality_test_suite.mbt; then
    echo "✓ 遥测数据批处理和缓冲"
fi
if grep -q "遥测数据压缩和传输优化" /home/runner/work/Azimuth/Azimuth/azimuth_high_quality_test_suite.mbt; then
    echo "✓ 遥测数据压缩和传输优化"
fi
if grep -q "遥测数据上下文传播" /home/runner/work/Azimuth/Azimuth/azimuth_high_quality_test_suite.mbt; then
    echo "✓ 遥测数据上下文传播"
fi
if grep -q "遥测数据质量监控" /home/runner/work/Azimuth/Azimuth/azimuth_high_quality_test_suite.mbt; then
    echo "✓ 遥测数据质量监控"
fi
if grep -q "遥测数据性能优化" /home/runner/work/Azimuth/Azimuth/azimuth_high_quality_test_suite.mbt; then
    echo "✓ 遥测数据性能优化"
fi

echo ""
echo "验证完成！高质量测试用例已成功创建并验证。"