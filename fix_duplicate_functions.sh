#!/bin/bash

# 批量修复测试文件中的重复函数定义问题
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_DIR="$PROJECT_ROOT/src/azimuth/test"

# 修复 azimuth 测试文件
echo "=== 修复 azimuth 测试文件中的重复函数定义 ==="
cd "$AZIMUTH_TEST_DIR"

# 需要保留的测试文件（不包含test_shared.mbt）
TEST_FILES=("simple_test.mbt" "lib_test.mbt" "basic_test.mbt" "enhanced_tests.mbt" 
           "additional_tests.mbt" "additional_comprehensive_test.mbt" 
           "math_fundamentals_test.mbt" "new_tests.mbt")

for file in "${TEST_FILES[@]}"; do
  if [ -f "$file" ]; then
    echo "修复文件: $file"
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 只保留测试函数，移除辅助函数定义
    awk '
      /^test / { in_test = 1 }
      in_test { print }
      /^}$/ && in_test { in_test = 0 }
    ' "$file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
  fi
done

echo "所有测试文件修复完成！"