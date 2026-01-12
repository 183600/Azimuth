#!/bin/bash

# 批量修复测试文件中的断言问题

# 修复azimuth包下的测试文件
echo "修复azimuth包下的测试文件..."

# 找到所有azimuth包下的测试文件
azimuth_test_files=$(find src/azimuth/test -name "*.mbt" -type f)

for file in $azimuth_test_files; do
    echo "处理文件: $file"
    
    # 检查文件是否包含@builtin.panic()但没有使用assert函数
    if grep -q "@builtin.panic()" "$file" && ! grep -q "@azimuth.assert_" "$file"; then
        echo "  修复断言..."
        
        # 创建临时文件
        temp_file=$(mktemp)
        
        # 替换断言模式
        sed -e 's/let result = @azimuth\.add(\([^,]*\), \([^)]*\))/let result = @azimuth.add(\1, \2)/g' \
            -e 's/if result != \([0-9]*\) {@builtin.panic()/@azimuth.assert_eq(\1, result)/g' \
            -e 's/if "\([^"]*\)" != result {@builtin.panic()/@azimuth.assert_eq_string("\1", result)/g' \
            -e 's/if result != "\([^"]*\)" {@builtin.panic()/@azimuth.assert_eq_string("\1", result)/g' \
            "$file" > "$temp_file"
        
        # 移动临时文件回原位置
        mv "$temp_file" "$file"
    fi
done

# 修复clean_test包下的测试文件
echo "修复clean_test包下的测试文件..."

# 找到所有clean_test包下的测试文件
clean_test_files=$(find src/clean_test/test -name "*.mbt" -type f)

for file in $clean_test_files; do
    echo "处理文件: $file"
    
    # 检查文件是否包含注释的println但没有使用assert函数
    if grep -q "// println" "$file" && ! grep -q "@clean_test.assert_" "$file"; then
        echo "  修复断言..."
        
        # 创建临时文件
        temp_file=$(mktemp)
        
        # 替换断言模式
        sed -e 's/let result  = @clean_test\.add(\([^,]*\), \([^)]*\))/let result = @clean_test.add(\1, \2)/g' \
            -e 's/if result != \([0-9]*\) {\/\/ println("Test failed")/@clean_test.assert_eq(\1, result)/g' \
            -e 's/if "\([^"]*\)" != result {\/\/ println("Test failed")/@clean_test.assert_eq_string("\1", result)/g' \
            -e 's/if result != "\([^"]*\)" {\/\/ println("Test failed")/@clean_test.assert_eq_string("\1", result)/g' \
            "$file" > "$temp_file"
        
        # 移动临时文件回原位置
        mv "$temp_file" "$file"
    fi
done

echo "批量修复完成！"