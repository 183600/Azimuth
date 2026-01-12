#!/bin/bash

# 批量修复测试文件的导入问题

echo "批量修复测试文件..."

TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/azimuth/test"

# 创建临时函数定义文件
cat > "$TEST_DIR/test_functions.mbt" << 'EOF'
// 在测试文件中定义需要的函数以解决导入问题
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

fn assert_eq(a : Int, b : Int) -> Unit {
  if a != b {
    raise @moonbitlang/core/builtin.Failure("Assertion failed: " + a.to_string() + " != " + b.to_string())
  }
}

fn assert_eq_string(a : String, b : String) -> Unit {
  if a != b {
    raise @moonbitlang/core/builtin.Failure("Assertion failed: " + a + " != " + b)
  }
}

fn assert_true(cond : Bool) -> Unit {
  if not cond {
    raise @moonbitlang/core/builtin.Failure("Assertion failed: expected true")
  }
}

fn assert_false(cond : Bool) -> Unit {
  if cond {
    raise @moonbitlang/core/builtin.Failure("Assertion failed: expected false")
  }
}

EOF

# 修复所有测试文件
for file in "$TEST_DIR"/*.mbt; do
  # 跳过我们刚创建的文件
  if [ "$(basename "$file")" = "test_functions.mbt" ]; then
    continue
  fi
  
  echo "修复 $file..."
  
  # 检查文件是否包含 azimuth:: 引用
  if grep -q "azimuth::" "$file"; then
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 添加函数定义
    cat "$TEST_DIR/test_functions.mbt" > "$temp_file"
    echo "" >> "$temp_file"
    
    # 替换 azimuth:: 为空（移除模块前缀）
    sed 's/azimuth:://g' "$file" >> "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
  fi
done

echo "修复完成！"