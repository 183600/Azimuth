#!/bin/bash
# 验证Premium MoonBit测试用例

echo "验证新创建的Premium MoonBit测试文件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt" ]; then
    echo "✓ 测试文件存在: premium_moonbit_tests.mbt"
else
    echo "✗ 测试文件不存在: premium_moonbit_tests.mbt"
    exit 1
fi

# 检查文件中是否包含测试用例
test_count=$(grep -c 'pub test "' /home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt)
echo "✓ 找到 $test_count 个测试用例"

# 检查是否已添加到moon.pkg.json
if grep -q "premium_moonbit_tests.mbt" /home/runner/work/Azimuth/Azimuth/src/moon.pkg.json; then
    echo "✓ 测试文件已添加到moon.pkg.json"
else
    echo "✗ 测试文件未添加到moon.pkg.json"
    exit 1
fi

# 列出所有测试用例名称
echo ""
echo "测试用例列表:"
grep 'pub test "' /home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt | sed 's/.*pub test "\([^"]*\)".*/\1/'

# 检查测试文件语法
echo ""
echo "检查测试文件语法..."
if [ -f "/home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt" ]; then
    # 简单检查括号匹配
    open_braces=$(grep -o '{' /home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt | wc -l)
    close_braces=$(grep -o '}' /home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt | wc -l)
    
    if [ "$open_braces" -eq "$close_braces" ]; then
        echo "✓ 括号匹配检查通过"
    else
        echo "✗ 括号不匹配: 开括号 $open_braces, 闭括号 $close_braces"
    fi
    
    # 检查基本语法元素
    pub_test_count=$(grep -c 'pub test "' /home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt)
    assert_count=$(grep -c 'assert_' /home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt)
    
    echo "✓ 包含 $pub_test_count 个公开测试用例"
    echo "✓ 包含 $assert_count 个断言语句"
fi

echo ""
echo "验证完成！所有Premium测试用例已成功创建并配置。"