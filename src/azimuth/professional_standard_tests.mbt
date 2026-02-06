// 专业标准 MoonBit 测试用例
// 测试 lib.mbt 中的核心功能

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  assert_eq(5, 2 + 3)
  assert_eq(30, add(10, 20))
  assert_eq(0, add(-5, 5))
  assert_eq(-30, add(-10, -20))
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  assert_eq(20, 4 * 5)
  assert_eq(42, multiply(6, 7))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(24, multiply(-4, -6))
  assert_eq(0, multiply(0, 100))
}

test "basic_arithmetic_subtraction" {
  // 测试基本减法运算
  assert_eq(7, 10 - 3)
  assert_eq(12, subtract(20, 8))
  assert_eq(-5, subtract(5, 10))
  assert_eq(5, subtract(-5, -10))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "string_greeting_function" {
  // 测试字符串问候函数
  assert_eq("Hello, World!", greet("World"))
  assert_eq("Hello, MoonBit!", greet("MoonBit"))
  assert_eq("Hello, !", greet(""))
  assert_eq("Hello, 123!", greet("123"))
}

test "assert_eq_functionality" {
  // 测试断言函数功能
  assert_eq(1, 1)
  assert_eq(100, 100)
  assert_eq(-50, -50)
  assert_eq(0, 0)
}

test "assert_eq_string_functionality" {
  // 测试字符串断言函数功能
  assert_eq_string("hello", "hello")
  assert_eq_string("", "")
  assert_eq_string("MoonBit", "MoonBit")
  assert_eq_string("123", "123")
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let a = 15
  let b = 25
  let c = 10
  
  // 计算 (a + b) * c
  let sum = add(a, b)
  let result = multiply(sum, c)
  
  assert_eq(40, sum)
  assert_eq(400, result)
  
  // 计算 result - a * b
  let product = multiply(a, b)
  let final_result = subtract(result, product)
  
  assert_eq(375, product)
  assert_eq(25, final_result)
}

test "edge_cases_handling" {
  // 测试边界情况处理
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 负数运算
  assert_eq(100, subtract(-100, -200))
  assert_eq(-33, divide_with_ceil(-100, 3))
  
  // 零值运算
  assert_eq(0, multiply(0, -5000))
  assert_eq(0, add(0, 0))
}

test "real_world_application" {
  // 测试实际应用场景
  // 计算购物车总价
  let item1_price = 199
  let item2_price = 299
  let item3_price = 149
  let discount = 50
  
  let subtotal = add(add(item1_price, item2_price), item3_price)
  let total = subtract(subtotal, discount)
  
  assert_eq(647, subtotal)
  assert_eq(597, total)
  
  // 计算每人需要支付的金额（向上取整）
  let people_count = 4
  let per_person = divide_with_ceil(total, people_count)
  
  assert_eq(150, per_person)
}