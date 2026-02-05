#!/bin/bash

echo "=== 验证增强 MoonBit 测试用例 ==="

cd /home/runner/work/Azimuth/Azimuth

# 检查文件是否存在
if [ -f "src/azimuth/azimuth_enhanced_moonbit_tests.mbt" ]; then
    echo "✓ 文件存在: src/azimuth/azimuth_enhanced_moonbit_tests.mbt"
    
    # 显示文件的前20行
    echo "文件前20行内容:"
    head -20 src/azimuth/azimuth_enhanced_moonbit_tests.mbt
    
    echo ""
    echo "统计测试用例数量..."
    test_count=$(grep -c "^test " src/azimuth/azimuth_enhanced_moonbit_tests.mbt)
    echo "测试用例数量: $test_count"
    
    if [ $test_count -le 10 ]; then
        echo "✓ 测试用例数量符合要求 (不超过10个)"
    else
        echo "✗ 测试用例数量超出要求 (超过10个)"
    fi
    
    echo ""
    echo "显示测试用例名称:"
    grep "^test " src/azimuth/azimuth_enhanced_moonbit_tests.mbt
    
    echo ""
    echo "检查断言语法..."
    assert_count=$(grep -c "assert_eq" src/azimuth/azimuth_enhanced_moonbit_tests.mbt)
    assert_string_count=$(grep -c "assert_eq_string" src/azimuth/azimuth_enhanced_moonbit_tests.mbt)
    echo "assert_eq 断言数量: $assert_count"
    echo "assert_eq_string 断言数量: $assert_string_count"
    
    echo ""
    echo "=== 验证完成 ==="
else
    echo "✗ 文件不存在: src/azimuth/azimuth_enhanced_moonbit_tests.mbt"
fi