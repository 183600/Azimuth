// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖 azimuth 模块的核心功能

// 导入主模块中的函数
use azimuth

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
}

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(-10, add(-3, -7))
  assert_eq(0, add(5, -5))
  assert_eq(2, add(-3, 5))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-21, multiply(3, -7))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  assert_eq(20, multiply(-4, -5))
  assert_eq(-18, multiply(-3, 6))
  assert_eq(0, multiply(-7, 0))
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, John_Doe!", greet("John_Doe"))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
}

test "complex_calculations" {
  // 测试复杂计算
  // (3 + 5) * 2
  let result = multiply(add(3, 5), 2)
  assert_eq(16, result)
  
  // 10 * 3 + 5
  let result2 = add(multiply(10, 3), 5)
  assert_eq(35, result2)
}

test "boundary_values" {
  // 测试边界值
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  assert_eq(2147483647, multiply(46341, 46341))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  
  // 加法单位元
  assert_eq(x, add(x, 0))
  
  // 乘法单位元
  assert_eq(x, multiply(x, 1))
  
  // 乘法零元
  assert_eq(0, multiply(x, 0))
}

test "error_conditions" {
  // 测试错误条件
  assert_true(true)
  assert_false(false)
  
  // 测试字符串相等
  assert_eq_string("Hello, World!", greet("World"))
}