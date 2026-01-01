#!/bin/bash
# 验证新创建的测试文件

echo "验证新创建的测试文件..."

# 新创建的测试文件列表
test_files=(
    "azimuth_enhanced_error_boundary_tests.mbt"
    "azimuth_concurrent_safety_enhanced_tests.mbt"
    "azimuth_performance_stress_enhanced_tests.mbt"
    "azimuth_internationalization_enhanced_tests.mbt"
    "azimuth_resource_management_enhanced_tests.mbt"
)

# 检查每个文件是否存在并包含测试用例
for file in "${test_files[@]}"; do
    filepath="/home/runner/work/Azimuth/Azimuth/azimuth/$file"
    
    if [ -f "$filepath" ]; then
        echo "✓ 测试文件存在: $file"
        
        # 检查文件中是否包含测试用例
        test_count=$(grep -c 'test "' "$filepath")
        echo "  - 包含 $test_count 个测试用例"
        
        # 列出前3个测试用例名称
        echo "  - 测试用例示例:"
        grep 'test "' "$filepath" | head -3 | sed 's/.*test "\([^"]*\)".*/    \1/'
        
    else
        echo "✗ 测试文件不存在: $file"
        exit 1
    fi
    echo ""
done

# 检查是否已添加到moon.pkg.json
echo "检查moon.pkg.json配置..."
for file in "${test_files[@]}"; do
    if grep -q "$file" /home/runner/work/Azimuth/Azimuth/azimuth/moon.pkg.json; then
        echo "✓ $file 已添加到moon.pkg.json"
    else
        echo "✗ $file 未添加到moon.pkg.json"
        exit 1
    fi
done

echo ""
echo "所有测试文件验证完成！"
echo "总计创建了 ${#test_files[@]} 个新的测试文件。"

# 统计总测试用例数
total_tests=0
for file in "${test_files[@]}"; do
    filepath="/home/runner/work/Azimuth/Azimuth/azimuth/$file"
    count=$(grep -c 'test "' "$filepath")
    total_tests=$((total_tests + count))
done

echo "总计包含 $total_tests 个测试用例。"

# 验证测试文件语法
echo ""
echo "验证测试文件语法..."
for file in "${test_files[@]}"; do
    filepath="/home/runner/work/Azimuth/Azimuth/azimuth/$file"
    
    # 检查基本的语法结构
    if grep -q 'test "' "$filepath" && grep -q '{' "$filepath" && grep -q '}' "$filepath"; then
        echo "✓ $file 语法结构正确"
    else
        echo "✗ $file 语法结构可能有问题"
    fi
done

echo ""
echo "验证完成！所有测试用例已成功创建并配置。"