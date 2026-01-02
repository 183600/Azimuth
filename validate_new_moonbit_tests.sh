#!/bin/bash

echo "验证新创建的MoonBit测试文件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_new_moonbit_tests.mbt" ]; then
    echo "✓ 测试文件已成功创建在正确位置"
    
    # 检查文件内容
    test_count=$(grep -c "^test " /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_new_moonbit_tests.mbt)
    echo "✓ 测试文件包含 $test_count 个测试用例"
    
    # 检查配置文件是否包含新测试
    if grep -q "azimuth_new_moonbit_tests.mbt" /home/runner/work/Azimuth/Azimuth/azimuth/moon.pkg.json; then
        echo "✓ 测试配置已正确更新"
    else
        echo "✗ 测试配置未正确更新"
        exit 1
    fi
    
    echo ""
    echo "新创建的测试用例列表："
    grep "^test " /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_new_moonbit_tests.mbt | sed 's/^test "/- /' | sed 's/" {/ /'
    
    echo ""
    echo "✓ 所有验证通过！新的MoonBit测试文件已成功添加到项目中。"
else
    echo "✗ 测试文件未找到"
    exit 1
fi