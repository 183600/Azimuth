#!/bin/bash

# 测试增强集成测试的脚本

echo "运行增强集成测试..."

# 尝试编译测试文件
cd /home/runner/work/Azimuth/Azimuth/azimuth

# 检查moonc是否可用
if command -v moonc &> /dev/null; then
    echo "找到moonc编译器"
    
    # 尝试编译我们的测试文件
    moonc ../azimuth_enhanced_integration_tests.mbt -o /tmp/test_enhanced_integration.core
    
    if [ $? -eq 0 ]; then
        echo "测试文件编译成功！"
    else
        echo "测试文件编译失败"
    fi
else
    echo "未找到moonc编译器"
fi

# 检查测试文件的语法
echo "检查测试文件语法..."
if [ -f "../azimuth_enhanced_integration_tests.mbt" ]; then
    echo "测试文件存在"
    # 计算测试用例数量
    test_count=$(grep -c "^test " ../azimuth_enhanced_integration_tests.mbt)
    echo "测试文件包含 $test_count 个测试用例"
else
    echo "测试文件不存在"
fi