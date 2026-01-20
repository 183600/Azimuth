// 标准MoonBit测试用例
// 测试azimuth包的核心功能

test "add_function_basic_cases" {
  // 测试基本加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(50, 50))
}

test "add_function_edge_cases" {
  // 测试边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值
  assert_eq(42, add(0, 42))
  assert_eq(42, add(42, 0))
  assert_eq(0, add(0, 0))
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
}

test "multiply_function_basic_cases" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(0, multiply(0, 100))
  assert_eq(100, multiply(1, 100))
}

test "multiply_function_edge_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试特殊值
  assert_eq(-100, multiply(100, -1))
  assert_eq(100, multiply(-100, -1))
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_various_inputs" {
  // 测试greet函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "greet_function_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
  assert_eq_string("Hello,   spaced   !", greet("  spaced  "))
}

test "combined_arithmetic_operations" {
  // 测试复合算术运算
  // 计算 (2 + 3) * 4 - 5
  let sum = add(2, 3)
  let product = multiply(sum, 4)
  let result = add(product, -5)
  assert_eq(15, result)
  
  // 计算 10 * (5 + 3) + 2
  let sum2 = add(5, 3)
  let product2 = multiply(10, sum2)
  let result2 = add(product2, 2)
  assert_eq(82, result2)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律 (简化测试)
  let c = 5
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "practical_calculation_scenarios" {
  // 实际计算场景
  // 购物车计算：商品价格 × 数量 + 运费
  let price = 199
  let quantity = 3
  let shipping = 20
  let total = add(multiply(price, quantity), shipping)
  assert_eq(617, total)
  
  // 温度转换：摄氏度 × 2 + 30（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "assertion_functions_verification" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试计算结果的断言
  let calc_result = add(10, 20)
  assert_eq(30, calc_result)
}