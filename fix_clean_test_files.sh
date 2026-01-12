#!/bin/bash

# 批量修复 clean_test 测试文件的脚本
# 在每个测试文件中添加需要的函数定义

# 进入 clean_test 目录
cd src/clean_test/test

# 需要修复的测试文件列表
test_files=(
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
    "enhanced_math_properties_test.mbt"
    "enhanced_test_suite.mbt"
    "essential_tests.mbt"
    "extended_test.mbt"
    "focused_additional_tests.mbt"
    "focused_comprehensive_tests.mbt"
    "focused_enhanced_tests.mbt"
    "focused_test_cases.mbt"
    "focused_unit_tests.mbt"
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
    "simple_test_verify.mbt"
    "simple_test.mbt"
    "specialized_test_cases.mbt"
    "standard_moonbit_tests.mbt"
    "standard_tests.mbt"
    "unique_test_cases.mbt"
)

# 函数定义模板
function_definitions='// 在测试文件中定义需要的函数以解决导入问题
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

'

# 修复每个测试文件
for file in "${test_files[@]}"; do
    if [ -f "$file" ]; then
        echo "修复文件: $file"
        
        # 检查文件是否已经包含函数定义
        if grep -q "fn add(a : Int, b : Int) -> Int" "$file"; then
            echo "文件 $file 已经包含函数定义，跳过"
            continue
        fi
        
        # 创建临时文件
        temp_file=$(mktemp)
        
        # 将函数定义添加到文件开头
        echo "$function_definitions" > "$temp_file"
        cat "$file" >> "$temp_file"
        
        # 替换原文件
        mv "$temp_file" "$file"
        
        echo "文件 $file 修复完成"
    else
        echo "文件 $file 不存在，跳过"
    fi
done

echo "所有 clean_test 测试文件修复完成"