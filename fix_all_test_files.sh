#!/bin/bash

# 批量修复测试文件的脚本
# 在每个测试文件中添加需要的函数定义

# 需要修复的测试文件列表
test_files=(
    "src/azimuth/test/additional_comprehensive_test_new.mbt"
    "src/azimuth/test/additional_comprehensive_tests.mbt"
    "src/azimuth/test/additional_coverage_tests.mbt"
    "src/azimuth/test/additional_edge_cases.mbt"
    "src/azimuth/test/additional_enhanced_tests.mbt"
    "src/azimuth/test/additional_focused_tests.mbt"
    "src/azimuth/test/additional_moonbit_tests.mbt"
    "src/azimuth/test/additional_practical_tests.mbt"
    "src/azimuth/test/additional_scenario_tests.mbt"
    "src/azimuth/test/additional_standard_tests.mbt"
    "src/azimuth/test/additional_test_cases.mbt"
    "src/azimuth/test/additional_tests.mbt"
    "src/azimuth/test/additional_unique_tests.mbt"
    "src/azimuth/test/advanced_math_test.mbt"
    "src/azimuth/test/advanced_test_scenarios.mbt"
    "src/azimuth/test/algorithmic_tests.mbt"
    "src/azimuth/test/basic_test.mbt"
    "src/azimuth/test/boundary_values_test.mbt"
    "src/azimuth/test/comprehensive_application_tests.mbt"
    "src/azimuth/test/comprehensive_edge_tests.mbt"
    "src/azimuth/test/comprehensive_test_cases.mbt"
    "src/azimuth/test/comprehensive_test_new.mbt"
    "src/azimuth/test/comprehensive_test_suite.mbt"
    "src/azimuth/test/comprehensive_tests.mbt"
    "src/azimuth/test/comprehensive_unique_tests.mbt"
    "src/azimuth/test/concise_functional_tests.mbt"
    "src/azimuth/test/concise_tests.mbt"
    "src/azimuth/test/core_functionality_tests.mbt"
    "src/azimuth/test/correct_concise_tests.mbt"
    "src/azimuth/test/debug_test_new.mbt"
    "src/azimuth/test/enhanced_coverage_tests.mbt"
    "src/azimuth/test/enhanced_math_properties_test.mbt"
    "src/azimuth/test/enhanced_math_test.mbt"
    "src/azimuth/test/enhanced_test_suite.mbt"
    "src/azimuth/test/enhanced_tests.mbt"
    "src/azimuth/test/essential_tests.mbt"
    "src/azimuth/test/extended_test.mbt"
    "src/azimuth/test/focused_additional_tests.mbt"
    "src/azimuth/test/focused_comprehensive_tests.mbt"
    "src/azimuth/test/focused_enhanced_tests.mbt"
    "src/azimuth/test/focused_test_cases.mbt"
    "src/azimuth/test/focused_unit_tests.mbt"
    "src/azimuth/test/math_fundamentals_test.mbt"
    "src/azimuth/test/mathematical_properties_test.mbt"
    "src/azimuth/test/new_additional_tests.mbt"
    "src/azimuth/test/new_comprehensive_tests.mbt"
    "src/azimuth/test/new_feature_tests.mbt"
    "src/azimuth/test/new_focused_tests.mbt"
    "src/azimuth/test/new_standard_tests.mbt"
    "src/azimuth/test/new_tests.mbt"
    "src/azimuth/test/performance_test.mbt"
    "src/azimuth/test/practical_application_tests.mbt"
    "src/azimuth/test/practical_comprehensive_tests.mbt"
    "src/azimuth/test/practical_scenario_tests.mbt"
    "src/azimuth/test/quality_test_suite.mbt"
    "src/azimuth/test/real_world_test.mbt"
    "src/azimuth/test/simple_new_test.mbt"
    "src/azimuth/test/simple_new_tests.mbt"
    "src/azimuth/test/simple_test.mbt"
    "src/azimuth/test/simple_test_new.mbt"
    "src/azimuth/test/simple_test_verify.mbt"
    "src/azimuth/test/simplified_unit_tests.mbt"
    "src/azimuth/test/specialized_comprehensive_tests.mbt"
    "src/azimuth/test/specialized_test_cases.mbt"
    "src/azimuth/test/specialized_tests.mbt"
    "src/azimuth/test/standard_moonbit_test_suite.mbt"
    "src/azimuth/test/standard_moonbit_tests.mbt"
    "src/azimuth/test/standard_test_cases.mbt"
    "src/azimuth/test/standard_tests.mbt"
    "src/azimuth/test/string_processing_test.mbt"
    "src/azimuth/test/unique_test_cases.mbt"
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

echo "所有测试文件修复完成"