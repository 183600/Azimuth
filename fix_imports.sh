#!/bin/bash

# 修复所有测试文件中的导入问题
echo "Fixing imports in test files..."

# 修复 azimuth/test 目录下的文件
cd src/azimuth/test

# 需要修复的文件列表
files_to_fix=(
    "additional_comprehensive_test.mbt"
    "basic_test_fixed.mbt"
    "math_fundamentals_test.mbt"
    "simple_import_test.mbt"
    "standalone_test.mbt"
)

for file in "${files_to_fix[@]}"; do
    if [ -f "$file" ]; then
        echo "Fixing $file..."
        # 移除 include 语句
        sed -i '/^\/\/ 导入 test_helper 中的函数$/d' "$file"
        sed -i '/^include "test_helper.mbt"$/d' "$file"
        
        # 移除重复的导入注释
        sed -i '/^\/\/ 导入测试辅助函数$/d' "$file"
        sed -i '/^\/\/ 测试函数和断言函数从 azimuth 包导入$/d' "$file"
        
        # 添加正确的导入注释
        sed -i '1i\
// 测试函数和断言函数从 azimuth 包导入
' "$file"
    fi
done

# 修复 clean_test/test 目录下的文件
cd ../../clean_test/test

# 需要修复的文件列表
clean_files_to_fix=(
    "additional_comprehensive_test.mbt"
    "additional_comprehensive_tests.mbt"
    "additional_coverage_tests.mbt"
    "additional_enhanced_tests.mbt"
    "additional_practical_tests.mbt"
    "additional_scenario_tests.mbt"
    "additional_standard_tests.mbt"
    "additional_tests.mbt"
    "advanced_test_scenarios.mbt"
    "algorithmic_tests.mbt"
    "comprehensive_test_cases.mbt"
    "comprehensive_tests.mbt"
    "concise_tests.mbt"
    "core_functionality_tests.mbt"
    "debug_test.mbt"
    "enhanced_math_properties_test.mbt"
    "enhanced_test_suite.mbt"
    "essential_tests.mbt"
    "extended_test.mbt"
    "focused_additional_tests.mbt"
    "focused_comprehensive_tests.mbt"
    "focused_enhanced_tests.mbt"
    "focused_test_cases.mbt"
    "focused_unit_tests.mbt"
    "lib_test.mbt"
    "math_fundamentals_test.mbt"
    "new_additional_tests.mbt"
    "new_comprehensive_tests.mbt"
    "new_feature_tests.mbt"
    "new_focused_tests.mbt"
    "new_standard_tests.mbt"
    "new_tests.mbt"
    "performance_test.mbt"
    "practical_scenario_tests.mbt"
    "quality_test_suite.mbt"
    "simple_test.mbt"
    "simple_test_verify.mbt"
    "specialized_test_cases.mbt"
    "standard_moonbit_tests.mbt"
    "standard_tests.mbt"
    "unique_test_cases.mbt"
)

for file in "${clean_files_to_fix[@]}"; do
    if [ -f "$file" ]; then
        echo "Fixing $file..."
        # 移除 include 语句
        sed -i '/^\/\/ 导入 test_helper 中的函数$/d' "$file"
        sed -i '/^include "test_helper.mbt"$/d' "$file"
        
        # 移除重复的导入注释
        sed -i '/^\/\/ 导入测试辅助函数$/d' "$file"
        sed -i '/^\/\/ 测试函数和断言函数从 azimuth 包导入$/d' "$file"
        
        # 添加正确的导入注释
        sed -i '1i\
// 测试函数和断言函数从 clean_test 包导入
' "$file"
    fi
done

cd ../../..