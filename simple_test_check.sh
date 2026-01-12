#!/bin/bash

# 简单的测试检查脚本
echo "Running simple test check..."

PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"

total_errors=0

# 函数：检查文件编译错误
check_file() {
    local file=$1
    local pkg_name=$2
    
    echo "Checking $file..."
    
    # 尝试编译文件
    output=$(node "$PROJECT_ROOT/moonc.js" check \
        -pkg "$pkg_name" \
        -std-path "$CORE_PATH" \
        "$file" 2>&1)
    
    if [ $? -ne 0 ]; then
        echo "Error in $file:"
        echo "$output"
        total_errors=$((total_errors + 1))
        return 1
    fi
    
    return 0
}

# 检查 azimuth 包的源文件
echo "Checking azimuth source files..."
cd "$PROJECT_ROOT/src/azimuth"
check_file "lib.mbt" "azimuth"

# 检查 azimuth 测试文件
echo ""
echo "Checking azimuth test files..."
cd test
for file in *.mbt; do
    if [ -f "$file" ]; then
        check_file "$file" "azimuth_test"
    fi
done

# 检查 clean_test 包的源文件
echo ""
echo "Checking clean_test source files..."
cd "$PROJECT_ROOT/src/clean_test"
check_file "lib.mbt" "clean_test"

# 检查 clean_test 测试文件
echo ""
echo "Checking clean_test test files..."
cd test
for file in *.mbt; do
    if [ -f "$file" ]; then
        check_file "$file" "clean_test_test"
    fi
done

# 输出结果
echo ""
echo "================================"
echo "Test Check Results:"
if [ $total_errors -eq 0 ]; then
    echo "✓ All files compiled successfully!"
    echo "✓ No compilation errors found!"
else
    echo "✗ Found $total_errors file(s) with compilation errors!"
    exit 1
fi