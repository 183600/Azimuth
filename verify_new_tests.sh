#!/bin/bash

# 验证新创建的标准 MoonBit 测试套件
echo "验证新创建的标准 MoonBit 测试套件..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 检查新测试文件是否存在
TEST_FILE="$AZIMUTH_PATH/standard_moonbit_test_suite.mbt"
if [ ! -f "$TEST_FILE" ]; then
    echo "错误：新测试文件不存在: $TEST_FILE"
    exit 1
fi

echo "测试文件存在: $TEST_FILE"

# 显示测试文件内容
echo ""
echo "测试文件内容："
cat "$TEST_FILE"

# 统计测试数量
echo ""
echo "测试统计："
TEST_COUNT=$(grep -c "test \"" "$TEST_FILE")
echo "测试用例数量: $TEST_COUNT"

# 检查测试文件语法
echo ""
echo "检查测试文件语法..."
cd "$AZIMUTH_PATH"

# 首先编译主库文件
echo "编译主库文件..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
    echo "错误：主库文件编译失败"
    exit 1
fi

# 生成 .mi 文件
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt -o azimuth.mi

# 检查新测试文件
echo "检查新测试文件..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i azimuth.mi "$TEST_FILE"
if [ $? -eq 0 ]; then
    echo "成功：新测试文件语法检查通过"
else
    echo "错误：新测试文件语法检查失败"
    exit 1
fi

echo ""
echo "验证完成！新测试文件包含 $TEST_COUNT 个标准 MoonBit 测试用例，语法正确。"