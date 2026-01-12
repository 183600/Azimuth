#!/bin/bash

# 批量修复测试文件中的导入问题

# 修复 azimuth/test 目录下的测试文件
azimuth_test_dir="/home/runner/work/Azimuth/Azimuth/src/azimuth/test"

# 需要修复的文件列表
azimuth_files=(
    "additional_comprehensive_test.mbt"
    "additional_comprehensive_tests.mbt"
    "additional_coverage_tests.mbt"
    "additional_edge_cases.mbt"
    "additional_enhanced_tests.mbt"
    "additional_focused_tests.mbt"
    "additional_moonbit_tests.mbt"
    "additional_practical_tests.mbt"
    "additional_scenario_tests.mbt"
    "additional_standard_tests.mbt"
    "additional_test_cases.mbt"
    "additional_tests.mbt"
    "additional_unique_tests.mbt"
    "advanced_math_test.mbt"
    "advanced_test_scenarios.mbt"
    "algorithmic_tests.mbt"
    "basic_test_fixed.mbt"
    "basic_test.mbt"
    "boundary_values_test.mbt"
    "comprehensive_application_tests.mbt"
    "comprehensive_edge_tests.mbt"
    "comprehensive_test_cases.mbt"
    "comprehensive_test_new.mbt"
    "comprehensive_test_suite.mbt"
    "comprehensive_tests.mbt"
    "comprehensive_unique_tests.mbt"
    "concise_functional_tests.mbt"
    "concise_tests.mbt"
    "core_functionality_tests.mbt"
    "correct_concise_tests.mbt"
    "debug_test.mbt"
    "debug_test_new.mbt"
    "enhanced_coverage_tests.mbt"
    "enhanced_math_properties_test.mbt"
    "enhanced_math_test.mbt"
    "enhanced_test_suite.mbt"
    "enhanced_tests.mbt"
    "essential_tests.mbt"
    "extended_test.mbt"
    "focused_additional_tests.mbt"
    "focused_comprehensive_tests.mbt"
    "focused_enhanced_tests.mbt"
    "focused_test_cases.mbt"
    "focused_unit_tests.mbt"
    "lib_test.mbt"
    "lib_with_tests.mbt"
    "math_fundamentals_test.mbt"
    "mathematical_properties_test.mbt"
    "new_additional_tests.mbt"
    "new_comprehensive_tests.mbt"
    "new_feature_tests.mbt"
    "new_focused_tests.mbt"
    "new_standard_tests.mbt"
    "new_tests.mbt"
    "performance_test.mbt"
    "practical_application_tests.mbt"
    "practical_comprehensive_tests.mbt"
    "practical_scenario_tests.mbt"
    "quality_test_suite.mbt"
    "real_world_test.mbt"
    "simple_debug_test.mbt"
    "simple_import_test.mbt"
    "simple_new_test.mbt"
    "simple_new_tests.mbt"
    "simple_test_new.mbt"
    "simple_test_verify.mbt"
    "simplified_unit_tests.mbt"
    "specialized_comprehensive_tests.mbt"
    "specialized_test_cases.mbt"
    "specialized_tests.mbt"
    "standalone_test.mbt"
    "standard_moonbit_test_suite.mbt"
    "standard_moonbit_tests.mbt"
    "standard_test_cases.mbt"
    "standard_tests.mbt"
    "string_processing_test.mbt"
    "template.mbt"
    "unique_test_cases.mbt"
)

# 修复 clean_test/test 目录下的测试文件
clean_test_dir="/home/runner/work/Azimuth/Azimuth/src/clean_test/test"

# 需要修复的文件列表
clean_test_files=(
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
    "simple_test_verify.mbt"
    "simple_test.mbt"
    "specialized_test_cases.mbt"
    "standard_moonbit_tests.mbt"
    "standard_tests.mbt"
    "unique_test_cases.mbt"
)

# 修复 azimuth/test 目录下的文件
echo "修复 azimuth/test 目录下的文件..."
for file in "${azimuth_files[@]}"; do
    filepath="$azimuth_test_dir/$file"
    if [ -f "$filepath" ]; then
        # 检查文件是否已经有 import 语句
        if ! grep -q "import.*azimuth" "$filepath"; then
            # 在文件开头添加 import 语句
            sed -i '1i import "../azimuth"' "$filepath"
            echo "已修复: $filepath"
        else
            echo "已包含导入语句，跳过: $filepath"
        fi
    else
        echo "文件不存在: $filepath"
    fi
done

# 修复 clean_test/test 目录下的文件
echo "修复 clean_test/test 目录下的文件..."
for file in "${clean_test_files[@]}"; do
    filepath="$clean_test_dir/$file"
    if [ -f "$filepath" ]; then
        # 检查文件是否已经有 import 语句
        if ! grep -q "import.*clean_test" "$filepath"; then
            # 在文件开头添加 import 语句
            sed -i '1i import "../clean_test"' "$filepath"
            echo "已修复: $filepath"
        else
            echo "已包含导入语句，跳过: $filepath"
        fi
    else
        echo "文件不存在: $filepath"
    fi
done

echo "批量修复完成！"