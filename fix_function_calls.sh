#!/bin/bash

# 批量修复azimuth测试文件中的函数调用问题
# 添加azimuth::前缀到所有函数调用

echo "开始修复azimuth测试文件中的函数调用..."

# 定义要修复的文件列表
files=(
  "src/azimuth/test/additional_comprehensive_test.mbt"
  "src/azimuth/test/basic_test_fixed.mbt"
  "src/azimuth/test/math_fundamentals_test.mbt"
  "src/azimuth/test/simple_import_test.mbt"
  "src/azimuth/test/standalone_test.mbt"
)

# 对于每个文件，进行替换
for file in "${files[@]}"; do
  if [ -f "$file" ]; then
    echo "修复文件: $file"
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 使用sed进行替换
    sed -e 's/assert_eq(/azimuth::assert_eq(/g' \
        -e 's/assert_eq_string(/azimuth::assert_eq_string(/g' \
        -e 's/assert_true(/azimuth::assert_true(/g' \
        -e 's/assert_false(/azimuth::assert_false(/g' \
        -e 's/add(/azimuth::add(/g' \
        -e 's/multiply(/azimuth::multiply(/g' \
        -e 's/greet(/azimuth::greet(/g' \
        "$file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
  else
    echo "文件不存在: $file"
  fi
done

echo "azimuth测试文件修复完成！"

# 批量修复clean_test测试文件中的函数调用
echo "开始修复clean_test测试文件中的函数调用..."

# 定义要修复的clean_test文件列表
clean_files=(
  "src/clean_test/test/additional_comprehensive_test.mbt"
  "src/clean_test/test/additional_comprehensive_tests.mbt"
  "src/clean_test/test/additional_coverage_tests.mbt"
  "src/clean_test/test/additional_enhanced_tests.mbt"
  "src/clean_test/test/additional_practical_tests.mbt"
  "src/clean_test/test/additional_scenario_tests.mbt"
  "src/clean_test/test/additional_standard_tests.mbt"
  "src/clean_test/test/additional_tests.mbt"
  "src/clean_test/test/advanced_test_scenarios.mbt"
  "src/clean_test/test/algorithmic_tests.mbt"
  "src/clean_test/test/comprehensive_test_cases.mbt"
  "src/clean_test/test/comprehensive_tests.mbt"
  "src/clean_test/test/concise_tests.mbt"
  "src/clean_test/test/core_functionality_tests.mbt"
  "src/clean_test/test/debug_test.mbt"
  "src/clean_test/test/enhanced_math_properties_test.mbt"
  "src/clean_test/test/enhanced_test_suite.mbt"
  "src/clean_test/test/essential_tests.mbt"
  "src/clean_test/test/extended_test.mbt"
  "src/clean_test/test/focused_additional_tests.mbt"
  "src/clean_test/test/focused_comprehensive_tests.mbt"
  "src/clean_test/test/focused_enhanced_tests.mbt"
  "src/clean_test/test/focused_test_cases.mbt"
  "src/clean_test/test/focused_unit_tests.mbt"
  "src/clean_test/test/lib_test.mbt"
  "src/clean_test/test/math_fundamentals_test.mbt"
  "src/clean_test/test/new_additional_tests.mbt"
  "src/clean_test/test/new_comprehensive_tests.mbt"
  "src/clean_test/test/new_feature_tests.mbt"
  "src/clean_test/test/new_focused_tests.mbt"
  "src/clean_test/test/new_standard_tests.mbt"
  "src/clean_test/test/new_tests.mbt"
  "src/clean_test/test/performance_test.mbt"
  "src/clean_test/test/practical_scenario_tests.mbt"
  "src/clean_test/test/quality_test_suite.mbt"
  "src/clean_test/test/simple_test.mbt"
  "src/clean_test/test/simple_test_verify.mbt"
  "src/clean_test/test/specialized_test_cases.mbt"
  "src/clean_test/test/standard_moonbit_tests.mbt"
  "src/clean_test/test/standard_tests.mbt"
  "src/clean_test/test/unique_test_cases.mbt"
)

# 对于每个文件，进行替换
for file in "${clean_files[@]}"; do
  if [ -f "$file" ]; then
    echo "修复文件: $file"
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 使用sed进行替换
    sed -e 's/assert_eq(/test_helper::assert_eq(/g' \
        -e 's/assert_eq_string(/test_helper::assert_eq_string(/g' \
        -e 's/assert_true(/test_helper::assert_true(/g' \
        -e 's/assert_false(/test_helper::assert_false(/g' \
        -e 's/add(/test_helper::add(/g' \
        -e 's/multiply(/test_helper::multiply(/g' \
        -e 's/greet(/test_helper::greet(/g' \
        "$file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
  else
    echo "文件不存在: $file"
  fi
done

echo "clean_test测试文件修复完成！"