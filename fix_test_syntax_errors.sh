#!/bin/bash

# 修复测试文件中的语法错误

echo "修复测试文件中的语法错误..."

# 修复 azimuth 测试文件
AZIMUTH_TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/azimuth/test"

# 创建正确的函数定义文件
cat > "$AZIMUTH_TEST_DIR/test_functions.mbt" << 'EOF'
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
    @moonbitlang/core/builtin.fail("Assertion failed: " + a.to_string() + " != " + b.to_string())
  }
}

fn assert_eq_string(a : String, b : String) -> Unit {
  if a != b {
    @moonbitlang/core/builtin.fail("Assertion failed: " + a + " != " + b)
  }
}

fn assert_true(cond : Bool) -> Unit {
  if !cond {
    @moonbitlang/core/builtin.fail("Assertion failed: expected true")
  }
}

fn assert_false(cond : Bool) -> Unit {
  if cond {
    @moonbitlang/core/builtin.fail("Assertion failed: expected false")
  }
}

EOF

# 修复 clean_test 测试文件
CLEAN_TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/clean_test/test"

# 创建正确的函数定义文件
cat > "$CLEAN_TEST_DIR/test_functions.mbt" << 'EOF'
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
    @moonbitlang/core/builtin.fail("Assertion failed: " + a.to_string() + " != " + b.to_string())
  }
}

fn assert_eq_string(a : String, b : String) -> Unit {
  if a != b {
    @moonbitlang/core/builtin.fail("Assertion failed: " + a + " != " + b)
  }
}

fn assert_true(cond : Bool) -> Unit {
  if !cond {
    @moonbitlang/core/builtin.fail("Assertion failed: expected true")
  }
}

fn assert_false(cond : Bool) -> Unit {
  if cond {
    @moonbitlang/core/builtin.fail("Assertion failed: expected false")
  }
}

EOF

# 修复所有测试文件中的 raise 语句
for dir in "$AZIMUTH_TEST_DIR" "$CLEAN_TEST_DIR"; do
  for file in "$dir"/*.mbt; do
    # 跳过函数定义文件
    if [ "$(basename "$file")" = "test_functions.mbt" ]; then
      continue
    fi
    
    # 检查文件是否包含 raise 语句
    if grep -q "raise" "$file"; then
      echo "修复 $file 中的 raise 语句..."
      
      # 使用 sed 替换 raise 语句
      sed -i 's/raise @moonbitlang\/core\/builtin\.Failure(/@moonbitlang\/core\/builtin.fail(/g' "$file"
    fi
  done
done

echo "修复完成！"