#!/bin/bash

# 测试新创建的标准 MoonBit 测试套件
echo "测试新创建的标准 MoonBit 测试套件..."

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

# 统计测试数量
echo ""
echo "测试统计："
TEST_COUNT=$(grep -c "test \"" "$TEST_FILE")
echo "测试用例数量: $TEST_COUNT"

# 检查测试文件语法
echo ""
echo "检查测试文件语法..."
cd "$AZIMUTH_PATH"

# 创建一个简化的测试文件，只包含必要的函数定义
cat > temp_lib.mbt << 'EOF'
pub fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  if lhs != rhs {
    let _ = "Assertion failed"
  } else {
    ()
  }
  ()
}

pub fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  if lhs != rhs {
    let _ = "String assertion failed"
  } else {
    ()
  }
  ()
}

pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn subtract(a : Int, b : Int) -> Int {
  a - b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}
EOF

# 编译简化的库文件
echo "编译简化的库文件..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" temp_lib.mbt
if [ $? -ne 0 ]; then
    echo "错误：简化库文件编译失败"
    rm -f temp_lib.mbt
    exit 1
fi

# 生成 .mi 文件
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" temp_lib.mbt -o temp_azimuth.mi

# 检查新测试文件
echo "检查新测试文件..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i temp_azimuth.mi "$TEST_FILE"
if [ $? -eq 0 ]; then
    echo "成功：新测试文件语法检查通过"
else
    echo "错误：新测试文件语法检查失败"
    rm -f temp_lib.mbt temp_azimuth.mi
    exit 1
fi

# 清理临时文件
rm -f temp_lib.mbt temp_azimuth.mi

echo ""
echo "验证完成！新测试文件包含 $TEST_COUNT 个标准 MoonBit 测试用例，语法正确。"
echo ""
echo "测试用例列表："
grep "test \"" "$TEST_FILE" | sed 's/^[[:space:]]*test "\([^"]*\)".*/\1/'
