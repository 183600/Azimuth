// 额外的标准 MoonBit 测试用例
// 测试 azimuth 项目的基本功能

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-3, -7))
}

test "add_function_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  
  // 测试与0相加
  assert_eq(42, add(42, 0))
  assert_eq(-42, add(-42, 0))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(5, 0))
}

test "multiply_function_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
  
  // 测试与1相乘
  assert_eq(42, multiply(42, 1))
  assert_eq(-42, multiply(-42, 1))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_function_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, user@123!", greet("user@123"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "mathematical_combinations" {
  // 测试数学运算的组合
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result = multiply(add(a, b), c)
  assert_eq(16, result)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(17, result2)
}

test "assertion_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算矩形面积：长 × 宽
  let length = 8
  let width = 6
  let area = multiply(length, width)
  assert_eq(48, area)
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(28, perimeter)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let total = add(multiply(item_price, quantity), shipping)
  assert_eq(612, total)
  
  // 温度转换（简化版）：摄氏度转华氏度 F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}