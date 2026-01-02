#!/bin/bash

echo "验证 Azimuth 综合测试套件..."

# 检查测试文件是否存在
if [ ! -f "/home/runner/work/Azimuth/Azimuth/azimuth_test/azimuth_comprehensive_test_suite.mbt" ]; then
    echo "错误：测试文件不存在"
    exit 1
fi

echo "✓ 测试文件存在"

# 检查测试文件是否包含10个测试
test_count=$(grep -c '^test "' /home/runner/work/Azimuth/Azimuth/azimuth_test/azimuth_comprehensive_test_suite.mbt)
echo "✓ 发现 $test_count 个测试用例"

if [ $test_count -ne 10 ]; then
    echo "警告：测试用例数量不是10个"
fi

# 检查测试文件是否包含所有必要的测试类型
test_types=("基础属性操作测试" "时间序列数据操作测试" "度量仪表盘测试" "日志记录完整测试" "跨服务一致性测试" "资源限制测试" "并发安全测试" "边界条件测试" "配置管理测试" "数据完整性测试")

for test_type in "${test_types[@]}"; do
    if grep -q "$test_type" /home/runner/work/Azimuth/Azimuth/azimuth_test/azimuth_comprehensive_test_suite.mbt; then
        echo "✓ 包含测试: $test_type"
    else
        echo "✗ 缺少测试: $test_type"
    fi
done

echo "验证完成！"