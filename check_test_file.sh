#!/bin/bash

echo "检查新创建的测试文件..."
echo "--------------------------------"

# 检查文件是否存在
if [ -f "src/azimuth/azimuth_core_functionality_tests.mbt" ]; then
    echo "✓ 测试文件存在"
    
    # 显示文件内容的前20行
    echo "文件内容预览："
    head -n 20 src/azimuth/azimuth_core_functionality_tests.mbt
    echo ""
    
    # 检查是否在moon.pkg.json中
    if grep -q "azimuth_core_functionality_tests.mbt" src/azimuth/moon.pkg.json; then
        echo "✓ 测试文件已在moon.pkg.json中"
    else
        echo "✗ 测试文件未在moon.pkg.json中"
    fi
else
    echo "✗ 测试文件不存在"
fi

echo ""
echo "尝试直接运行测试文件..."
./moon test src/azimuth/azimuth_core_functionality_tests.mbt 2>&1 | head -n 20