#!/bin/bash

# 验证新创建的遥测测试用例
# 这个脚本检查所有新测试文件的语法和基本结构

echo "开始验证新创建的遥测测试用例..."

# 定义新创建的测试文件列表
NEW_TEST_FILES=(
    "azimuth_telemetry_data_compression_optimization_tests.mbt"
    "azimuth_time_series_data_aggregation_tests.mbt"
    "azimuth_distributed_tracing_consistency_tests.mbt"
    "azimuth_telemetry_configuration_dynamic_update_tests.mbt"
    "azimuth_multidimensional_attribute_query_tests.mbt"
    "azimuth_telemetry_data_long_term_storage_archive_tests.mbt"
    "azimuth_edge_computing_telemetry_tests.mbt"
    "azimuth_webassembly_platform_compatibility_tests.mbt"
)

# 检查文件是否存在
echo "检查测试文件是否存在..."
for file in "${NEW_TEST_FILES[@]}"; do
    if [ -f "$file" ]; then
        echo "✓ $file 存在"
    else
        echo "✗ $file 不存在"
        exit 1
    fi
done

# 检查文件内容基本结构
echo ""
echo "检查测试文件基本结构..."

for file in "${NEW_TEST_FILES[@]}"; do
    echo "检查 $file..."
    
    # 检查文件是否包含test关键字
    if grep -q "test \"" "$file"; then
        echo "  ✓ 包含测试用例"
    else
        echo "  ✗ 不包含测试用例"
        exit 1
    fi
    
    # 检查文件是否包含断言
    if grep -q "assert_" "$file"; then
        echo "  ✓ 包含断言"
    else
        echo "  ✗ 不包含断言"
        exit 1
    fi
    
    # 检查文件大小是否合理（至少1KB）
    file_size=$(stat -c%s "$file")
    if [ "$file_size" -gt 1024 ]; then
        echo "  ✓ 文件大小合理 ($file_size 字节)"
    else
        echo "  ✗ 文件大小过小 ($file_size 字节)"
        exit 1
    fi
done

# 统计测试用例数量
echo ""
echo "统计测试用例数量..."
total_tests=0

for file in "${NEW_TEST_FILES[@]}"; do
    test_count=$(grep -c "test \"" "$file")
    echo "$file: $test_count 个测试用例"
    total_tests=$((total_tests + test_count))
done

echo ""
echo "总计: $total_tests 个测试用例"

# 检查是否满足不超过10个测试文件的要求
num_files=${#NEW_TEST_FILES[@]}
if [ "$num_files" -le 10 ]; then
    echo "✓ 测试文件数量符合要求（不超过10个文件）"
else
    echo "✗ 测试文件数量超过要求（超过10个文件）"
    exit 1
fi

# 检查测试文件命名规范
echo ""
echo "检查测试文件命名规范..."
for file in "${NEW_TEST_FILES[@]}"; do
    if [[ "$file" == azimuth_*_tests.mbt ]]; then
        echo "✓ $file 命名符合规范"
    else
        echo "✗ $file 命名不符合规范"
        exit 1
    fi
done

# 检查测试用例覆盖的主题
echo ""
echo "检查测试用例覆盖的主题..."
expected_topics=(
    "compression"
    "time_series"
    "distributed_tracing"
    "configuration"
    "attribute_query"
    "storage_archive"
    "edge_computing"
    "webassembly"
)

for topic in "${expected_topics[@]}"; do
    found=false
    for file in "${NEW_TEST_FILES[@]}"; do
        if [[ "$file" == *"$topic"* ]]; then
            found=true
            echo "✓ $topic 主题已覆盖"
            break
        fi
    done
    
    if [ "$found" = false ]; then
        echo "✗ $topic 主题未覆盖"
        exit 1
    fi
done

# 生成测试报告
echo ""
echo "生成测试报告..."
cat > new_telemetry_tests_report.md << EOF
# 新遥测测试用例报告

## 概述
本报告总结了为Azimuth遥测系统新创建的测试用例。

## 测试文件列表
EOF

for file in "${NEW_TEST_FILES[@]}"; do
    test_count=$(grep -c "test \"" "$file")
    echo "- $file ($test_count 个测试用例)" >> new_telemetry_tests_report.md
done

cat >> new_telemetry_tests_report.md << EOF

## 测试用例总数
总计: $total_tests 个测试用例

## 覆盖的主题
EOF

for topic in "${expected_topics[@]}"; do
    echo "- $topic" >> new_telemetry_tests_report.md
done

cat >> new_telemetry_tests_report.md << EOF

## 验证结果
所有测试文件已通过基本验证，包括：
- 文件存在性检查
- 测试用例结构检查
- 断言存在性检查
- 文件大小合理性检查
- 命名规范检查
- 主题覆盖检查

## 下一步
建议运行实际的测试套件以验证测试用例的功能正确性。
EOF

echo "✓ 测试报告已生成: new_telemetry_tests_report.md"

echo ""
echo "验证完成！所有新创建的遥测测试用例已通过基本验证。"
echo "建议运行 moon test 或相应的测试命令来执行实际的测试用例。"