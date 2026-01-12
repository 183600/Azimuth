#!/bin/bash

# 修复的测试检查脚本
echo "Running fixed test check..."

PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"

total_errors=0

# 函数：检查文件编译错误
check_file() {
    local file=$1
    local pkg_name=$2
    local mi_file=$3
    
    echo "Checking $file..."
    
    # 构建编译命令
    local cmd="node \"$PROJECT_ROOT/moonc.js\" check -pkg \"$pkg_name\" -std-path \"$CORE_PATH\""
    
    # 如果有 mi 文件，添加 -i 参数
    if [ -n "$mi_file" ] && [ -f "$mi_file" ]; then
        cmd="$cmd -i \"$mi_file\""
    fi
    
    # 添加测试相关参数
    if [[ "$pkg_name" == *_test ]]; then
        cmd="$cmd -include-doctests -blackbox-test"
    fi
    
    # 添加文件
    cmd="$cmd \"$file\""
    
    # 执行编译
    output=$(eval "$cmd" 2>&1)
    local exit_code=$?
    
    if [ $exit_code -ne 0 ]; then
        echo "Error in $file:"
        echo "$output"
        total_errors=$((total_errors + 1))
        return 1
    fi
    
    return 0
}

# 先编译主包生成 .mi 文件
echo "Compiling main packages..."

# 编译 azimuth 包
cd "$PROJECT_ROOT/src/azimuth"
echo "Generating azimuth.mi..."
node "$PROJECT_ROOT/moonc.js" check -pkg "azimuth" -std-path "$CORE_PATH" -o "azimuth.mi" "lib.mbt" 2>/dev/null

# 编译 clean_test 包  
cd "$PROJECT_ROOT/src/clean_test"
echo "Generating clean_test.mi..."
node "$PROJECT_ROOT/moonc.js" check -pkg "clean_test" -std-path "$CORE_PATH" -o "clean_test.mi" "lib.mbt" 2>/dev/null

# 检查 azimuth 测试文件
echo ""
echo "Checking azimuth test files..."
cd "$PROJECT_ROOT/src/azimuth/test"
for file in *.mbt; do
    if [ -f "$file" ]; then
        check_file "$file" "azimuth_test" "../azimuth.mi"
    fi
done

# 检查 clean_test 测试文件
echo ""
echo "Checking clean_test test files..."
cd "$PROJECT_ROOT/src/clean_test/test"
for file in *.mbt; do
    if [ -f "$file" ]; then
        check_file "$file" "clean_test_test" "../clean_test.mi"
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