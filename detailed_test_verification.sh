#!/bin/bash

# 详细测试验证脚本 - 深度分析所有可能的问题
echo "开始详细测试验证..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test"

# 统计变量
TOTAL_ERRORS=0
TOTAL_WARNINGS=0

echo "========================================="
echo "1. 检查azimuth包编译状态"
echo "========================================="
cd "$AZIMUTH_PATH"

# 编译azimuth包并检查详细输出
echo "编译azimuth包..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt 2>azimuth_compile_errors.log
AZIMUTH_COMPILE_RESULT=$?

if [ $AZIMUTH_COMPILE_RESULT -ne 0 ]; then
    echo "❌ azimuth包编译失败！"
    echo "错误详情："
    cat azimuth_compile_errors.log
    TOTAL_ERRORS=$((TOTAL_ERRORS + 1))
else
    echo "✅ azimuth包编译成功"
fi

echo ""
echo "========================================="
echo "2. 检查clean_test包编译状态"
echo "========================================="
cd "$CLEAN_TEST_PATH"

# 编译clean_test包并检查详细输出
echo "编译clean_test包..."
node "$PROJECT_ROOT/moonc.js" check -pkg clean_test -std-path "$CORE_PATH" lib.mbt 2>clean_test_compile_errors.log
CLEAN_TEST_COMPILE_RESULT=$?

if [ $CLEAN_TEST_COMPILE_RESULT -ne 0 ]; then
    echo "❌ clean_test包编译失败！"
    echo "错误详情："
    cat clean_test_compile_errors.log
    TOTAL_ERRORS=$((TOTAL_ERRORS + 1))
else
    echo "✅ clean_test包编译成功"
fi

echo ""
echo "========================================="
echo "3. 检查azimuth测试编译状态"
echo "========================================="
cd "$AZIMUTH_PATH/test"

# 生成azimuth.mi文件（如果不存在）
if [ ! -f "../azimuth.mi" ]; then
    echo "生成azimuth.mi文件..."
    node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" -o azimuth.mi lib.mbt
fi

# 编译azimuth测试包
echo "编译azimuth测试包..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi simple_test.mbt 2>azimuth_test_errors.log
AZIMUTH_TEST_RESULT=$?

if [ $AZIMUTH_TEST_RESULT -ne 0 ]; then
    echo "❌ azimuth测试包编译失败！"
    echo "错误详情："
    cat azimuth_test_errors.log
    TOTAL_ERRORS=$((TOTAL_ERRORS + 1))
else
    echo "✅ azimuth测试包编译成功"
fi

echo ""
echo "========================================="
echo "4. 检查clean_test测试编译状态"
echo "========================================="
cd "$CLEAN_TEST_PATH/test"

# 生成clean_test.mi文件（如果不存在）
if [ ! -f "../clean_test.mi" ]; then
    echo "生成clean_test.mi文件..."
    node "$PROJECT_ROOT/moonc.js" check -pkg clean_test -std-path "$CORE_PATH" -o clean_test.mi lib.mbt
fi

# 编译clean_test测试包
echo "编译clean_test测试包..."
node "$PROJECT_ROOT/moonc.js" check -pkg clean_test_test -std-path "$CORE_PATH" -i ../clean_test.mi simple_test.mbt 2>clean_test_test_errors.log
CLEAN_TEST_TEST_RESULT=$?

if [ $CLEAN_TEST_TEST_RESULT -ne 0 ]; then
    echo "❌ clean_test测试包编译失败！"
    echo "错误详情："
    cat clean_test_test_errors.log
    TOTAL_ERRORS=$((TOTAL_ERRORS + 1))
else
    echo "✅ clean_test测试包编译成功"
fi

echo ""
echo "========================================="
echo "5. 检查测试用例语法和逻辑"
echo "========================================="

# 检查azimuth测试用例
echo "检查azimuth测试用例..."
cd "$AZIMUTH_PATH/test"

# 统计测试数量
AZIMUTH_TEST_COUNT=$(grep "^test " simple_test.mbt | wc -l)
AZIMUTH_TEST_COUNT=$(echo "$AZIMUTH_TEST_COUNT" | tr -d ' ')
echo "azimuth包测试数量: $AZIMUTH_TEST_COUNT"

# 检查测试用例中的常见问题
if grep -q "@azimuth\." simple_test.mbt; then
    echo "✅ azimuth测试使用了正确的包引用"
else
    echo "⚠️  azimuth测试可能没有使用正确的包引用"
    TOTAL_WARNINGS=$((TOTAL_WARNINGS + 1))
fi

# 检查clean_test测试用例
echo "检查clean_test测试用例..."
cd "$CLEAN_TEST_PATH/test"

# 统计测试数量
CLEAN_TEST_COUNT=$(grep "^test " simple_test.mbt | wc -l)
CLEAN_TEST_COUNT=$(echo "$CLEAN_TEST_COUNT" | tr -d ' ')
echo "clean_test包测试数量: $CLEAN_TEST_COUNT"

# 检查测试用例中的常见问题
if grep -q "@clean_test\." simple_test.mbt; then
    echo "✅ clean_test测试使用了正确的包引用"
else
    echo "⚠️  clean_test测试可能没有使用正确的包引用"
    TOTAL_WARNINGS=$((TOTAL_WARNINGS + 1))
fi

echo ""
echo "========================================="
echo "6. 最终验证结果"
echo "========================================="
echo "错误数量: $TOTAL_ERRORS"
echo "警告数量: $TOTAL_WARNINGS"

if [ $TOTAL_ERRORS -eq 0 ]; then
    echo "✅ 所有编译检查通过！"
    if [ $TOTAL_WARNINGS -eq 0 ]; then
        echo "✅ 没有发现任何问题！"
    else
        echo "⚠️  有 $TOTAL_WARNINGS 个警告需要注意"
    fi
    exit 0
else
    echo "❌ 发现 $TOTAL_ERRORS 个错误需要修复"
    exit 1
fi