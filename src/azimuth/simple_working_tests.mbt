// 简单的 MoonBit 测试用例
// 确保能够正常工作

test "basic_addition_test" {
  // 基本加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
}

test "basic_multiplication_test" {
  // 基本乘法测试
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(0, multiply(5, 0))
}

test "greet_function_test" {
  // greet函数测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "mathematical_properties_test" {
  // 数学性质测试
  let x = 7
  let y = 11
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
}

test "boundary_values_test" {
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "complex_calculation_test" {
  // 复杂计算测试
  // 计算矩形周长：2 × (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = multiply(2, add(length, width))
  assert_eq(30, perimeter)
  
  // 计算平均数
  let a = 80
  let b = 90
  let c = 85
  let average = add(add(a, b), c) / 3
  assert_eq(85, average)
}

test "real_world_scenario_test" {
  // 实际应用场景测试
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let total = add(multiply(item_price, quantity), shipping)
  assert_eq(617, total)
  
  // 温度转换（简化版）：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "string_processing_test" {
  // 字符串处理测试
  // 包含特殊字符
  assert_eq_string("Hello, test@123!", greet("test@123"))
  assert_eq_string("Hello, 用户!", greet("用户"))
  
  // 长字符串
  let long_name = "VeryLongNameForTesting"
  assert_eq_string("Hello, VeryLongNameForTesting!", greet(long_name))
}

test "overflow_protection_test" {
  // 溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(1000000000, 1500000000))
  
  // 负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(-1500000000, -1000000000))
  
  // 乘法溢出
  assert_eq(max_val, multiply(46341, 46341))
}

test "assert_functions_test" {
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}