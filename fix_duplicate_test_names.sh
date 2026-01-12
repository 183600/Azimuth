#!/bin/bash

# 修复重复测试名称的脚本

echo "Fixing duplicate test names..."

# 修复 azimuth 目录中的测试文件
AZIMUTH_TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/azimuth/test"

# 修复 basic_test.mbt
if [ -f "$AZIMUTH_TEST_DIR/basic_test.mbt" ]; then
  sed -i 's/"test_1_basic_add"/"test_1_basic_add_original"/g' "$AZIMUTH_TEST_DIR/basic_test.mbt"
  sed -i 's/"test_1_basic_multiply"/"test_1_basic_multiply_original"/g' "$AZIMUTH_TEST_DIR/basic_test.mbt"
  sed -i 's/"test_1_basic_greet"/"test_1_basic_greet_original"/g' "$AZIMUTH_TEST_DIR/basic_test.mbt"
fi

# 修复 math_fundamentals_test.mbt
if [ -f "$AZIMUTH_TEST_DIR/math_fundamentals_test.mbt" ]; then
  sed -i 's/"test_3_math_properties_add"/"test_3_math_properties_add_fundamentals"/g' "$AZIMUTH_TEST_DIR/math_fundamentals_test.mbt"
  sed -i 's/"test_3_math_properties_multiply"/"test_3_math_properties_multiply_fundamentals"/g' "$AZIMUTH_TEST_DIR/math_fundamentals_test.mbt"
  sed -i 's/"test_3_boundary_values"/"test_3_boundary_values_fundamentals"/g' "$AZIMUTH_TEST_DIR/math_fundamentals_test.mbt"
fi

# 修复 additional_comprehensive_test.mbt
if [ -f "$AZIMUTH_TEST_DIR/additional_comprehensive_test.mbt" ]; then
  sed -i 's/"test_2_edge_cases_add"/"test_2_edge_cases_add_additional"/g' "$AZIMUTH_TEST_DIR/additional_comprehensive_test.mbt"
  sed -i 's/"test_2_edge_cases_multiply"/"test_2_edge_cases_multiply_additional"/g' "$AZIMUTH_TEST_DIR/additional_comprehensive_test.mbt"
  sed -i 's/"test_2_edge_cases_greet"/"test_2_edge_cases_greet_additional"/g' "$AZIMUTH_TEST_DIR/additional_comprehensive_test.mbt"
fi

# 修复 clean_test 目录中的测试文件
CLEAN_TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/clean_test/test"

# 为每个文件添加唯一后缀
files=(
  "additional_comprehensive_tests.mbt:comprehensive2"
  "additional_coverage_tests.mbt:coverage"
  "additional_enhanced_tests.mbt:enhanced"
  "additional_practical_tests.mbt:practical"
  "additional_scenario_tests.mbt:scenario"
  "additional_standard_tests.mbt:standard"
  "additional_tests.mbt:additional"
  "advanced_test_scenarios.mbt:advanced"
  "algorithmic_tests.mbt:algorithmic"
  "comprehensive_test_cases.mbt:cases"
  "comprehensive_tests.mbt:comprehensive"
  "concise_tests.mbt:concise"
  "core_functionality_tests.mbt:core"
  "enhanced_math_properties_test.mbt:math_props"
  "enhanced_test_suite.mbt:enhanced_suite"
  "essential_tests.mbt:essential"
  "extended_test.mbt:extended"
  "focused_additional_tests.mbt:focused_add"
  "focused_comprehensive_tests.mbt:focused_comp"
  "focused_enhanced_tests.mbt:focused_enh"
  "focused_test_cases.mbt:focused_cases"
  "focused_unit_tests.mbt:focused_unit"
  "math_fundamentals_test.mbt:fundamentals"
  "new_additional_tests.mbt:new_add"
  "new_comprehensive_tests.mbt:new_comp"
  "new_feature_tests.mbt:new_feature"
  "new_focused_tests.mbt:new_focused"
  "new_standard_tests.mbt:new_standard"
  "new_tests.mbt:new"
  "performance_test.mbt:performance"
  "practical_scenario_tests.mbt:practical_scenario"
  "quality_test_suite.mbt:quality"
  "simple_test.mbt:simple"
  "simple_test_verify.mbt:simple_verify"
  "specialized_test_cases.mbt:specialized_cases"
  "standard_moonbit_tests.mbt:standard_moon"
  "standard_tests.mbt:standard2"
  "unique_test_cases.mbt:unique"
)

for file_info in "${files[@]}"; do
  file=$(echo "$file_info" | cut -d: -f1)
  suffix=$(echo "$file_info" | cut -d: -f2)
  
  if [ -f "$CLEAN_TEST_DIR/$file" ]; then
    sed -i "s/\"basic_add\"/\"basic_add_$suffix\"/g" "$CLEAN_TEST_DIR/$file"
    sed -i "s/\"basic_multiply\"/\"basic_multiply_$suffix\"/g" "$CLEAN_TEST_DIR/$file"
    sed -i "s/\"basic_greet\"/\"basic_greet_$suffix\"/g" "$CLEAN_TEST_DIR/$file"
  fi
done

echo "Fixed duplicate test names"