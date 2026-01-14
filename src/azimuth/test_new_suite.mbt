// 标准 MoonBit 测试套件
// 包含不超过10个标准测试用例，使用标准的 MoonBit 测试语法

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-3, 3))
  assert_eq(-10, add(-5, -5))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(0, multiply(5, 0))
}

test "greet_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "add_overflow_handling" {
  // 测试加法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
}

test "multiply_overflow_handling" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算矩形面积：长 × 宽
  let length = 10
  let width = 5
  let area = multiply(length, width)
  assert_eq(50, area)
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(30, perimeter)
}

test "string_processing" {
  // 测试字符串处理
  let name_with_spaces = "  John Doe  "
  let result = greet(name_with_spaces)
  assert_eq_string("Hello,   John Doe  !", result)
  
  let name_with_symbols = "user@123"
  let result2 = greet(name_with_symbols)
  assert_eq_string("Hello, user@123!", result2)
}

test "assert_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 简单购物车计算
  let price = 199
  let quantity = 3
  let total = multiply(price, quantity)
  assert_eq(597, total)
}