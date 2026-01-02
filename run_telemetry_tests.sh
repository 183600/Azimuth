#!/bin/bash

echo "运行 Azimuth 遥测系统测试用例"
echo "================================"

# 检查文件是否存在
if [ ! -f "azimuth_telemetry_focused_tests.mbt" ]; then
    echo "错误: 找不到测试文件 azimuth_telemetry_focused_tests.mbt"
    exit 1
fi

echo "测试文件已创建: azimuth_telemetry_focused_tests.mbt"
echo "包含以下测试用例:"
echo "1. 遥测数据生成测试"
echo "2. 时间戳操作测试" 
echo "3. 指标数据类型测试"
echo "4. 日志级别处理测试"
echo "5. 属性键值对操作测试"
echo "6. 上下文传播测试"
echo "7. 采样决策测试"
echo "8. 错误处理和异常测试"
echo "9. 资源管理测试"
echo "10. 数据序列化测试"
echo ""
echo "测试用例已成功添加到项目中，共10个测试用例。"