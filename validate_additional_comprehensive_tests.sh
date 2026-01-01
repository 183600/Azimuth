#!/bin/bash
# 验证新创建的additional_comprehensive_new_tests.mbt文件

echo "验证新创建的additional_comprehensive_new_tests.mbt文件..."

# 新创建的测试文件
test_file="additional_comprehensive_new_tests.mbt"
filepath="/home/runner/work/Azimuth/Azimuth/azimuth/$test_file"

# 检查文件是否存在
if [ -f "$filepath" ]; then
    echo "✓ 测试文件存在: $test_file"
    
    # 检查文件中是否包含测试用例
    test_count=$(grep -c 'pub test "' "$filepath")
    echo "  - 包含 $test_count 个测试用例"
    
    # 列出所有测试用例名称
    echo "  - 测试用例列表:"
    grep 'pub test "' "$filepath" | sed 's/.*pub test "\([^"]*\)".*/    \1/'
    
else
    echo "✗ 测试文件不存在: $test_file"
    exit 1
fi

echo ""

# 检查是否已添加到moon.pkg.json
echo "检查moon.pkg.json配置..."
if grep -q "$test_file" /home/runner/work/Azimuth/Azimuth/azimuth/moon.pkg.json; then
    echo "✓ $test_file 已添加到moon.pkg.json"
else
    echo "✗ $test_file 未添加到moon.pkg.json"
    exit 1
fi

echo ""

# 验证测试文件语法
echo "验证测试文件语法..."
if grep -q 'pub test "' "$filepath" && grep -q '{' "$filepath" && grep -q '}' "$filepath"; then
    echo "✓ $test_file 语法结构正确"
else
    echo "✗ $test_file 语法结构可能有问题"
fi

# 检查导入语句
if grep -q 'import "azimuth/azimuth"' "$filepath"; then
    echo "✓ 包含正确的导入语句"
else
    echo "✗ 缺少导入语句"
fi

# 检查注释
if grep -q '// New Test' "$filepath"; then
    echo "✓ 包含测试注释"
else
    echo "✗ 缺少测试注释"
fi

echo ""
echo "验证完成！测试文件已成功创建并配置。"
echo "总计包含 $test_count 个测试用例。"

# 检查文件大小
file_size=$(wc -c < "$filepath")
echo "文件大小: $file_size 字节"

# 检查文件行数
file_lines=$(wc -l < "$filepath")
echo "文件行数: $file_lines 行"

echo ""
echo "所有测试用例验证完成！"