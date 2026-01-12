#!/bin/bash

# 简单的测试运行器

echo "运行简单的测试..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 创建临时测试文件
temp_test="$PROJECT_ROOT/temp_test.mbt"

# 创建一个简单的测试文件，包含函数定义和测试
cat > "$temp_test" << 'EOF'
// 包含函数定义
pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

// 测试
test "simple_test" {
  let result = add(1, 1)
  if result != 2 {
    raise @moonbitlang/core/builtin.Failure("Expected 2 but got " + result.to_string())
  }
}

test "basic_add" {
  let result = add(2, 3)
  if result != 5 {
    raise @moonbitlang/core/builtin.Failure("Add test failed")
  }
}

test "basic_multiply" {
  let result = multiply(2, 3)
  if result != 6 {
    raise @moonbitlang/core/builtin.Failure("Multiply test failed")
  }
}

test "basic_greet" {
  let result = greet("World")
  if result != "Hello, World!" {
    raise @moonbitlang/core/builtin.Failure("Greet test failed")
  }
}
EOF

# 编译并运行测试
echo "编译并运行测试..."
cd "$PROJECT_ROOT"
node moonc.js check -pkg temp_test -std-path core temp_test.mbt
if [ $? -eq 0 ]; then
  echo "测试编译成功！"
else
  echo "测试编译失败！"
fi

# 清理临时文件
rm -f "$temp_test"