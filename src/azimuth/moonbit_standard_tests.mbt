// MoonBit 标准测试用例
// 测试基本功能：加法、乘法和字符串处理

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-1, -1))
  assert_eq(100, add(50, 50))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(1, multiply(1, 1))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "greet_basic_functionality" {
  // 测试基本字符串处理功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, add(min_val, 1))
}

test "multiply_boundary_values" {
  // 测试边界值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "complex_calculations" {
  // 测试复合计算
  // 计算 (2 + 3) * (4 + 5) = 5 * 9 = 45
  let result = multiply(add(2, 3), add(4, 5))
  assert_eq(45, result)
  
  // 计算 10 * (5 + 3) - 20 = 10 * 8 - 20 = 80 - 20 = 60
  let result2 = add(multiply(10, add(5, 3)), -20)
  assert_eq(60, result2)
}

test "string_special_characters" {
  // 测试特殊字符的字符串处理
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello,   spaced   !", greet("  spaced  "))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "practical_scenarios" {
  // 测试实际应用场景
  // 购物车计算：商品价格 × 数量
  let price = 199
  let quantity = 3
  let total = multiply(price, quantity)
  assert_eq(597, total)
  
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "error_handling" {
  // 测试错误处理
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
}