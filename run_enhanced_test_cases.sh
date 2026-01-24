#!/bin/bash

# 运行新创建的增强测试用例
echo "运行新创建的增强测试用例..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/azimuth"

# 编译 azimuth 包
echo "编译 azimuth 包..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "错误: azimuth/lib.mbt 编译失败"
  exit 1
fi

# 生成 .mi 文件
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt -o azimuth.mi

# 测试新创建的增强测试用例
echo ""
echo "测试新创建的增强测试用例..."
cd "$PROJECT_ROOT"

echo "检查 azimuth_enhanced_test_cases.mbt..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i "$AZIMUTH_PATH/azimuth.mi" "$AZIMUTH_PATH/azimuth_enhanced_test_cases.mbt" 2>&1 | tee "test_enhanced_cases.log"
if [ ${PIPESTATUS[0]} -ne 0 ]; then
  echo "错误: azimuth_enhanced_test_cases.mbt 编译失败"
else
  echo "成功: azimuth_enhanced_test_cases.mbt 编译通过"
fi

echo ""
echo "增强测试用例检查完成。查看 test_enhanced_cases.log 文件获取详细信息。"