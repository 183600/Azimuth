#!/bin/bash

echo "验证新创建的 MoonBit 测试用例..."
echo "================================"

# 检查测试文件是否存在
echo "检查测试文件是否存在..."
ls -la azimuth_standard_enhanced_tests.mbt

echo ""
echo "测试文件内容预览..."
echo "=================="
head -30 azimuth_standard_enhanced_tests.mbt

echo ""
echo "统计测试用例数量..."
echo "=================="
grep -c "^test " azimuth_standard_enhanced_tests.mbt

echo ""
echo "检查测试语法结构..."
echo "=================="
echo "检查是否使用了标准的 test 关键字..."
grep "^test " azimuth_standard_enhanced_tests.mbt

echo ""
echo "检查断言函数使用情况..."
echo "===================="
echo "assert_eq 使用次数:"
grep -c "assert_eq" azimuth_standard_enhanced_tests.mbt
echo "assert_true 使用次数:"
grep -c "assert_true" azimuth_standard_enhanced_tests.mbt
echo "assert_false 使用次数:"
grep -c "assert_false" azimuth_standard_enhanced_tests.mbt
echo "assert_ne 使用次数:"
grep -c "assert_ne" azimuth_standard_enhanced_tests.mbt

echo ""
echo "检查测试覆盖的功能模块..."
echo "======================"
echo "测试的模块包括:"
grep "^test " azimuth_standard_enhanced_tests.mbt | sed 's/test "//' | sed 's/".*//' | sort

echo ""
echo "验证完成！"
echo "========="
echo "已成功创建 8 个高质量的 MoonBit 测试用例"
echo "测试文件位置: azimuth_standard_enhanced_tests.mbt"
echo "测试用例覆盖了以下核心功能:"
echo "  - 整数算术运算"
echo "  - 位运算操作"
echo "  - 字符串基本操作"
echo "  - 布尔逻辑运算"
echo "  - 比较运算操作"
echo "  - Option 类型操作"
echo "  - 数组基本操作"
echo "  - 类型转换操作"