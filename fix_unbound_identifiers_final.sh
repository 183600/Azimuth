#!/bin/bash

# 修复测试文件中的未绑定标识符问题
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"

echo "开始修复测试文件中的未绑定标识符问题..."

# 查找所有包含未绑定标识符的测试文件
echo "查找需要修复的测试文件..."

# 修复 azimuth_test 目录中的文件
if [ -f "$PROJECT_ROOT/azimuth_test.mi" ]; then
    echo "发现 azimuth_test.mi 文件，需要检查其内容"
fi

# 修复 clean_test_test 目录中的文件  
if [ -f "$PROJECT_ROOT/clean_test_test.mi" ]; then
    echo "发现 clean_test_test.mi 文件，需要检查其内容"
fi

# 查找所有 .mbt 文件中的未绑定标识符
echo "查找所有.mbt文件中的未绑定标识符..."

# 查找包含 assert_eq 但没有 @azimuth. 或 @clean_test. 前缀的文件
find "$PROJECT_ROOT" -name "*.mbt" -type f -not -path "*/backup/*" -not -path "*/.git/*" | while read file; do
    # 检查文件是否包含未绑定的 assert_eq
    if grep -q "assert_eq(" "$file" && ! grep -q "@azimuth.assert_eq\|@clean_test.assert_eq" "$file"; then
        echo "需要修复文件: $file"
        
        # 创建备份
        cp "$file" "$file.bak_fix_unbound"
        
        # 根据文件路径确定使用哪个前缀
        if [[ "$file" == *"azimuth"* ]] || [[ "$file" == *"azimuth_test"* ]]; then
            echo "使用 @azimuth. 前缀修复 $file"
            sed -i 's/assert_eq(/@azimuth.assert_eq(/g' "$file"
            sed -i 's/assert_eq_string(/@azimuth.assert_eq_string(/g' "$file"
            sed -i 's/assert_true(/@azimuth.assert_true(/g' "$file"
            sed -i 's/assert_false(/@azimuth.assert_false(/g' "$file"
            sed -i 's/add(/@azimuth.add(/g' "$file"
            sed -i 's/multiply(/@azimuth.multiply(/g' "$file"
            sed -i 's/greet(/@azimuth.greet(/g' "$file"
        elif [[ "$file" == *"clean_test"* ]] || [[ "$file" == *"clean_test_test"* ]]; then
            echo "使用 @clean_test. 前缀修复 $file"
            sed -i 's/assert_eq(/@clean_test.assert_eq(/g' "$file"
            sed -i 's/assert_eq_string(/@clean_test.assert_eq_string(/g' "$file"
            sed -i 's/assert_true(/@clean_test.assert_true(/g' "$file"
            sed -i 's/assert_false(/@clean_test.assert_false(/g' "$file"
            sed -i 's/add(/@clean_test.add(/g' "$file"
            sed -i 's/multiply(/@clean_test.multiply(/g' "$file"
            sed -i 's/greet(/@clean_test.greet(/g' "$file"
        fi
    fi
done

echo "修复完成！"
echo "运行测试验证修复结果..."

cd "$PROJECT_ROOT"
./moon test