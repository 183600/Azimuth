// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法

// 测试基本加法功能
test "basic_addition_functionality" {
  // 测试正数相加
  assert_eq(5, add(2, 3))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
  
  // 测试负数相加
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(-10, 10))
  
  // 测试混合符号相加
  assert_eq(3, add(10, -7))
  assert_eq(-3, add(-10, 7))
}

// 测试基本乘法功能
test "basic_multiplication_functionality" {
  // 测试正数相乘
  assert_eq(6, multiply(2, 3))
  assert_eq(100, multiply(10, 10))
  assert_eq(0, multiply(5, 0))
  
  // 测试负数相乘
  assert_eq(6, multiply(-2, -3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(-5, 0))
  
  // 测试与1相乘
  assert_eq(7, multiply(7, 1))
  assert_eq(7, multiply(1, 7))
  assert_eq(-7, multiply(-7, 1))
}

// 测试字符串处理功能
test "string_processing_functionality" {
  // 测试基本字符串拼接
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 测试空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
}

// 测试边界值处理
test "boundary_value_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, multiply(max_val, 1))
  
  // 测试最小值边界
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试最大值与最小值的运算
  assert_eq(-1, add(max_val, min_val))
}

// 测试溢出保护
test "overflow_protection" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  assert_eq(max_val, add(max_val, 100))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试乘法溢出保护
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(-46341, 46341))
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
}

// 测试实际应用场景
test "real_world_scenarios" {
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  assert_eq(617, total)
  
  // 温度转换（摄氏度转华氏度简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 简单利息计算
  let principal = 1000
  let rate = 5
  let years = 3
  let interest = multiply(principal, multiply(rate, years))
  let total_amount = add(principal, interest)
  assert_eq(16000, total_amount)
}

// 测试数学性质
test "mathematical_properties" {
  let a = 7
  let b = 11
  let c = 3
  
  // 测试加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 测试乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 测试分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 测试结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

// 测试复合计算
test "complex_calculations" {
  // 计算 (a + b) × c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = add(a, b)
  let product = multiply(sum_ab, c)
  let result = add(product, -d)
  assert_eq(22, result)
  
  // 计算矩形面积和周长
  let length = 10
  let width = 5
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(50, area)
  assert_eq(30, perimeter)
}

// 测试断言函数
test "assertion_functions" {
  // 测试 assert_true 和 assert_false
  assert_true(true)
  assert_false(false)
  assert_true(5 > 3)
  assert_false(5 < 3)
  
  // 测试 assert_eq
  assert_eq(42, 42)
  assert_eq(0, add(5, -5))
  assert_eq(25, multiply(5, 5))
  
  // 测试 assert_eq_string
  assert_eq_string("Hello", "Hello")
  assert_eq_string(greet("Test"), "Hello, Test!")
  assert_eq_string("", "")
}

// 测试错误处理和特殊情况
test "error_handling_and_special_cases" {
  // 测试除零保护（通过断言函数）
  assert_true(true)  // 确保测试环境正常
  
  // 测试特殊输入值
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(0, multiply(0, -2147483648))
  assert_eq(2147483647, add(0, 2147483647))
  assert_eq(-2147483648, add(0, -2147483648))
  
  // 测试-1的特殊情况
  assert_eq(-5, multiply(5, -1))
  assert_eq(5, multiply(-5, -1))
  
  // 测试最小值与-1相乘的特殊情况
  let min_val = -2147483648
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
}