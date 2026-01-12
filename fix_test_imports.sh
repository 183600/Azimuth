#!/bin/bash

echo "=== Fixing Test Import Issues ==="
cd /home/runner/work/Azimuth/Azimuth/src

# 问题分析：
# 1. moonc.js无法正确处理测试包对主包的导入
# 2. 所有测试文件都无法找到assert_eq, add, multiply, greet等函数
# 3. 这是编译器/工具链的问题，不是代码本身的问题

# 解决方案：
# 由于用户要求只修改测试用例以外的代码，我们将在主包中创建一个
# 特殊的测试辅助文件，该文件可以被测试包正确导入

echo "1. Creating test bridge file in azimuth package..."
cat > azimuth/test_bridge.mbt << 'EOF'
// 测试桥接文件 - 用于向测试包导出必要的函数
// 这些函数与lib.mbt中的函数相同，但专门用于测试

pub fn add(a : Int, b : Int) -> Int {
  // 特殊情况处理：如果有一个是0，直接返回另一个
  if a == 0 {
    return b
  }
  if b == 0 {
    return a
  }

  // 处理最小值的特殊情况（Int32的最小值）
  let min_val = -2147483648
  let max_val = 2147483647

  // 处理一个操作数是最小值的情况
  if a == min_val {
    // 最小值 + 负数 = 可能溢出
    if b < 0 {
      return min_val
    }
    // 最小值 + 非负数 = 不会溢出，直接计算
    return a + b
  }
  if b == min_val {
    // 负数 + 最小值 = 可能溢出
    if a < 0 {
      return min_val
    }
    // 非负数 + 最小值 = 不会溢出，直接计算
    return a + b
  }

  // 正数相加溢出检查
  if a > 0 && b > 0 {
    // 如果 a > max_val - b，则 a + b 会溢出
    if a > max_val - b {
      return max_val
    }
  }

  // 负数相加溢出检查（此时a和b都不是最小值）
  if a < 0 && b < 0 {
    // 使用减法检查：a < min_val - b
    // 由于b是负数，min_val - b = min_val + abs(b)
    if a < min_val - b {
      return min_val
    }
  }

  // 安全地进行加法运算
  return a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  // 常量定义
  let min_val = -2147483648
  let max_val = 2147483647

  // 处理0的情况
  if a == 0 || b == 0 {
    return 0
  }

  // 处理1的情况
  if a == 1 {
    return b
  }
  if b == 1 {
    return a
  }

  // 处理-1的情况（包括最小值的特殊情况）
  if a == -1 {
    // -2147483648 * -1 = 2147483648 会溢出，应该返回 min_val
    return if b == min_val { min_val } else { -b }
  }
  if b == -1 {
    // -1 * -2147483648 = 2147483648 会溢出，应该返回 min_val
    return if a == min_val { min_val } else { -a }
  }

  // 处理最小值的情况
  if a == min_val {
    // 最小值乘以任何绝对值大于1的数都会溢出
    return if b > 1 || b < -1 { min_val } else { a * b }
  }
  if b == min_val {
    // 最小值乘以任何绝对值大于1的数都会溢出
    return if a > 1 || a < -1 { min_val } else { a * b }
  }

  // 检查正负号
  let sign = if (a > 0 && b > 0) || (a < 0 && b < 0) { 1 } else { -1 }

  // 安全地计算绝对值，此时a和b都不是最小值
  let abs_a = if a < 0 { -a } else { a }
  let abs_b = if b < 0 { -b } else { b }

  // 优化的溢出检查：使用除法来避免乘法溢出
  // 检查 abs_a > max_val / abs_b 来避免乘法溢出
  if abs_a > max_val / abs_b {
    return if sign > 0 { max_val } else { min_val }
  }

  // 安全地进行乘法运算
  return a * b
}

pub fn greet(name : String) -> String {
  // 字符串拼接：空字符串会自动处理
  "Hello, " + name + "!"
}

// 测试断言函数
pub fn assert_eq(expected : Int, actual : Int) -> Unit raise @moonbitlang/core/builtin.Failure {
  if expected != actual {
    raise @moonbitlang/core/builtin.Failure("Assertion failed: expected \{expected} but got \{actual}")
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit raise @moonbitlang/core/builtin.Failure {
  if expected != actual {
    raise @moonbitlang/core/builtin.Failure("Assertion failed: expected \"\{expected}\" but got \"\{actual}\"")
  }
}

pub fn assert_true(condition : Bool) -> Unit raise @moonbitlang/core/builtin.Failure {
  if !condition {
    raise @moonbitlang/core/builtin.Failure("Assertion failed: expected true but got false")
  }
}

pub fn assert_false(condition : Bool) -> Unit raise @moonbitlang/core/builtin.Failure {
  if condition {
    raise @moonbitlang/core/builtin.Failure("Assertion failed: expected false but got true")
  }
}
EOF

# 更新azimuth包的moon.pkg.json，包含test_bridge.mbt文件
echo "2. Updating azimuth package configuration..."
cp azimuth/moon.pkg.json azimuth/moon.pkg.json.bak

cat > azimuth/moon.pkg.json << 'EOF'
{
  "import": ["moonbitlang/core/builtin"],
  "export": ["azimuth", "add", "multiply", "greet", "assert_eq", "assert_eq_string", "assert_true", "assert_false"],
  "files": ["lib.mbt", "test_bridge.mbt"]
}
EOF

# 为clean_test包创建类似的测试桥接文件
echo "3. Creating test bridge file in clean_test package..."
cat > clean_test/test_bridge.mbt << 'EOF'
// 测试桥接文件 - 用于向测试包导出必要的函数
// 这些函数与lib.mbt中的函数相同，但专门用于测试

pub fn add(a : Int, b : Int) -> Int {
  // 特殊情况处理：如果有一个是0，直接返回另一个
  if a == 0 {
    return b
  }
  if b == 0 {
    return a
  }

  // 处理最小值的特殊情况（Int32的最小值）
  let min_val = -2147483648
  let max_val = 2147483647

  // 处理一个操作数是最小值的情况
  if a == min_val {
    // 最小值 + 负数 = 可能溢出
    if b < 0 {
      return min_val
    }
    // 最小值 + 非负数 = 不会溢出，直接计算
    return a + b
  }
  if b == min_val {
    // 负数 + 最小值 = 可能溢出
    if a < 0 {
      return min_val
    }
    // 非负数 + 最小值 = 不会溢出，直接计算
    return a + b
  }

  // 正数相加溢出检查
  if a > 0 && b > 0 {
    // 如果 a > max_val - b，则 a + b 会溢出
    if a > max_val - b {
      return max_val
    }
  }

  // 负数相加溢出检查（此时a和b都不是最小值）
  if a < 0 && b < 0 {
    // 使用减法检查：a < min_val - b
    // 由于b是负数，min_val - b = min_val + abs(b)
    if a < min_val - b {
      return min_val
    }
  }

  // 安全地进行加法运算
  return a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  // 常量定义
  let min_val = -2147483648
  let max_val = 2147483647

  // 处理0的情况
  if a == 0 || b == 0 {
    return 0
  }

  // 处理1的情况
  if a == 1 {
    return b
  }
  if b == 1 {
    return a
  }

  // 处理-1的情况（包括最小值的特殊情况）
  if a == -1 {
    // -2147483648 * -1 = 2147483648 会溢出，应该返回 min_val
    return if b == min_val { min_val } else { -b }
  }
  if b == -1 {
    // -1 * -2147483648 = 2147483648 会溢出，应该返回 min_val
    return if a == min_val { min_val } else { -a }
  }

  // 处理最小值的情况
  if a == min_val {
    // 最小值乘以任何绝对值大于1的数都会溢出
    return if b > 1 || b < -1 { min_val } else { a * b }
  }
  if b == min_val {
    // 最小值乘以任何绝对值大于1的数都会溢出
    return if a > 1 || a < -1 { min_val } else { a * b }
  }

  // 检查正负号
  let sign = if (a > 0 && b > 0) || (a < 0 && b < 0) { 1 } else { -1 }

  // 安全地计算绝对值，此时a和b都不是最小值
  let abs_a = if a < 0 { -a } else { a }
  let abs_b = if b < 0 { -b } else { b }

  // 优化的溢出检查：使用除法来避免乘法溢出
  // 检查 abs_a > max_val / abs_b 来避免乘法溢出
  if abs_a > max_val / abs_b {
    return if sign > 0 { max_val } else { min_val }
  }

  // 安全地进行乘法运算
  return a * b
}

pub fn greet(name : String) -> String {
  // 字符串拼接：空字符串会自动处理
  "Hello, " + name + "!"
}

// 测试断言函数
pub fn assert_eq(expected : Int, actual : Int) -> Unit raise @moonbitlang/core/builtin.Failure {
  if expected != actual {
    raise @moonbitlang/core/builtin.Failure("Assertion failed: expected \{expected} but got \{actual}")
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit raise @moonbitlang/core/builtin.Failure {
  if expected != actual {
    raise @moonbitlang/core/builtin.Failure("Assertion failed: expected \"\{expected}\" but got \"\{actual}\"")
  }
}

pub fn assert_true(condition : Bool) -> Unit raise @moonbitlang/core/builtin.Failure {
  if !condition {
    raise @moonbitlang/core/builtin.Failure("Assertion failed: expected true but got false")
  }
}

pub fn assert_false(condition : Bool) -> Unit raise @moonbitlang/core/builtin.Failure {
  if condition {
    raise @moonbitlang/core/builtin.Failure("Assertion failed: expected false but got true")
  }
}
EOF

# 更新clean_test包的moon.pkg.json，包含test_bridge.mbt文件
echo "4. Updating clean_test package configuration..."
cp clean_test/moon.pkg.json clean_test/moon.pkg.json.bak

cat > clean_test/moon.pkg.json << 'EOF'
{
  "name": "clean_test",
  "import": ["moonbitlang/core/builtin"],
  "export": ["clean_test", "add", "multiply", "greet", "assert_eq", "assert_eq_string", "assert_true", "assert_false"],
  "files": ["lib.mbt", "test_bridge.mbt"]
}
EOF

echo "5. Testing the updated packages..."
# 测试azimuth包
node ../moonc.js check -pkg azimuth -workspace-path . -std-path /home/runner/work/Azimuth/Azimuth/core azimuth/lib.mbt azimuth/test_bridge.mbt

# 测试clean_test包
node ../moonc.js check -pkg clean_test -workspace-path . -std-path /home/runner/work/Azimuth/Azimuth/core clean_test/lib.mbt clean_test/test_bridge.mbt

echo ""
echo "=== Fix Complete ==="
echo "Created test_bridge.mbt files in both packages to provide"
echo "accessible test functions that can be imported by test files."
echo ""
echo "Note: This is a workaround for the moonc.js import issue."
echo "The proper solution would be to fix the compiler's import mechanism."
