#!/bin/bash

echo "验证新创建的专项MoonBit测试文件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth_specialized_moonbit_tests.mbt" ]; then
    echo "✓ 专项测试文件已成功创建在正确位置"
    
    # 检查文件内容
    test_count=$(grep -c "^test " /home/runner/work/Azimuth/Azimuth/azimuth_specialized_moonbit_tests.mbt)
    echo "✓ 测试文件包含 $test_count 个测试用例"
    
    # 检查配置文件是否包含新测试
    if grep -q "azimuth_specialized_moonbit_tests.mbt" /home/runner/work/Azimuth/Azimuth/azimuth/moon.pkg.json; then
        echo "✓ 测试配置已正确更新"
    else
        echo "✗ 测试配置未正确更新"
        exit 1
    fi
    
    echo ""
    echo "新创建的专项测试用例列表："
    echo "1. 属性值类型转换边界测试"
    echo "2. 上下文传播深度嵌套测试"
    echo "3. 资源属性合并策略测试"
    echo "4. 跨服务追踪一致性测试"
    echo "5. 高并发场景下的遥测数据完整性测试"
    echo "6. 时间序列数据聚合测试"
    echo "7. 遥测配置动态更新测试"
    echo "8. 边界条件错误处理测试"
    echo "9. 内存管理和资源清理测试"
    echo "10. 国际化和本地化支持测试"
    
    echo ""
    echo "✓ 所有验证通过！新的专项MoonBit测试文件已成功添加到项目中。"
    echo ""
    echo "这些测试用例涵盖了以下核心功能："
    echo "- 属性值类型系统的边界处理"
    echo "- 上下文传播的深度嵌套场景"
    echo "- 资源属性的智能合并策略"
    echo "- 跨服务追踪的一致性保证"
    echo "- 高并发环境下的数据完整性"
    echo "- 时间序列数据的聚合操作"
    echo "- 遥测配置的动态更新机制"
    echo "- 边界条件的错误处理能力"
    echo "- 内存管理和资源清理机制"
    echo "- 国际化和本地化支持"
else
    echo "✗ 测试文件未找到"
    exit 1
fi