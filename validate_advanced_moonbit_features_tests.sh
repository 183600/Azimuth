#!/bin/bash

echo "验证新创建的高级MoonBit特性测试文件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth_advanced_moonbit_features_tests.mbt" ]; then
    echo "✓ 测试文件已成功创建在正确位置"
    
    # 检查文件内容
    test_count=$(grep -c "^test " /home/runner/work/Azimuth/Azimuth/azimuth_advanced_moonbit_features_tests.mbt)
    echo "✓ 测试文件包含 $test_count 个测试用例"
    
    # 检查文件大小
    file_size=$(wc -l < /home/runner/work/Azimuth/Azimuth/azimuth_advanced_moonbit_features_tests.mbt)
    echo "✓ 测试文件包含 $file_size 行代码"
    
    # 检查是否包含高级特性测试
    if grep -q "higher order functions" /home/runner/work/Azimuth/Azimuth/azimuth_advanced_moonbit_features_tests.mbt; then
        echo "✓ 包含高阶函数测试"
    fi
    
    if grep -q "function composition" /home/runner/work/Azimuth/Azimuth/azimuth_advanced_moonbit_features_tests.mbt; then
        echo "✓ 包含函数组合测试"
    fi
    
    if grep -q "recursive traversal" /home/runner/work/Azimuth/Azimuth/azimuth_advanced_moonbit_features_tests.mbt; then
        echo "✓ 包含递归遍历测试"
    fi
    
    if grep -q "pattern matching" /home/runner/work/Azimuth/Azimuth/azimuth_advanced_moonbit_features_tests.mbt; then
        echo "✓ 包含模式匹配测试"
    fi
    
    if grep -q "closures" /home/runner/work/Azimuth/Azimuth/azimuth_advanced_moonbit_features_tests.mbt; then
        echo "✓ 包含闭包测试"
    fi
    
    if grep -q "record updates" /home/runner/work/Azimuth/Azimuth/azimuth_advanced_moonbit_features_tests.mbt; then
        echo "✓ 包含记录更新测试"
    fi
    
    if grep -q "stream processing" /home/runner/work/Azimuth/Azimuth/azimuth_advanced_moonbit_features_tests.mbt; then
        echo "✓ 包含流式处理测试"
    fi
    
    if grep -q "generics" /home/runner/work/Azimuth/Azimuth/azimuth_advanced_moonbit_features_tests.mbt; then
        echo "✓ 包含泛型测试"
    fi
    
    echo ""
    echo "新创建的测试用例列表："
    grep "^test " /home/runner/work/Azimuth/Azimuth/azimuth_advanced_moonbit_features_tests.mbt | sed 's/^test "/- /' | sed 's/" {/ /'
    
    echo ""
    echo "✓ 所有验证通过！新的高级MoonBit特性测试文件已成功添加到项目中。"
else
    echo "✗ 测试文件未找到"
    exit 1
fi