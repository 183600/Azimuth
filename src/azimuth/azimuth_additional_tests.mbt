// 标准 MoonBit 测试用例 - Azimuth 项目补充测试
// 测试核心功能：add, multiply, greet 和断言函数

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
}

test "add_function_negative_numbers" {
  // 测试 add 函数处理负数的能力
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-10, 10))
  assert_eq(-20, add(-30, 10))
  assert_eq(5, add(-15, 20))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(0, multiply(0, 5))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_function_large_numbers" {
  // 测试 multiply 函数处理大数的能力
  assert_eq(10000, multiply(100, 100))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(-1000, 1000))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数处理各种输入的能力
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "overflow_protection_add" {
  // 测试 add 函数的溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
}

test "overflow_protection_multiply" {
  // 测试 multiply 函数的溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(-46341, 46341))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(900, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(230, result2)
}

test "assertion_functions_validation" {
  // 测试断言函数的正确性
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("MoonBit", "MoonBit")
  
  // 测试复合断言
  let x = 5
  let y = 10
  assert_true(add(x, y) == 15)
  assert_false(multiply(x, y) == 40)
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let cart_total = add(total1, total2)
  
  assert_eq(697, cart_total)
  
  // 场景2：温度转换（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}