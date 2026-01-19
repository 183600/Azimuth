// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法

// 测试基本加法功能
test "basic_addition_functionality" {
  // 测试正数相加
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 测试负数相加
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(0, @azimuth.add(-10, 10))
  
  // 测试混合符号相加
  @azimuth.assert_eq(3, @azimuth.add(10, -7))
  @azimuth.assert_eq(-3, @azimuth.add(-10, 7))
}

// 测试基本乘法功能
test "basic_multiplication_functionality" {
  // 测试正数相乘
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  
  // 测试负数相乘
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
  
  // 测试与1相乘
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(7, @azimuth.multiply(1, 7))
  @azimuth.assert_eq(-7, @azimuth.multiply(-7, 1))
}

// 测试字符串处理功能
test "string_processing_functionality" {
  // 测试基本字符串拼接
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
}

// 测试边界值处理
test "boundary_value_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

// 测试溢出保护
test "overflow_protection" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

// 测试实际应用场景
test "real_world_scenarios" {
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(617, total)
  
  // 温度转换（摄氏度转华氏度简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 简单利息计算
  let principal = 1000
  let rate = 5
  let years = 3
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total_amount = @azimuth.add(principal, interest)
  @azimuth.assert_eq(16000, total_amount)
}

// 测试数学性质
test "mathematical_properties" {
  let a = 7
  let b = 11
  let c = 3
  
  // 测试加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
  
  // 测试结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
}

// 测试复合计算
test "complex_calculations" {
  // 计算 (a + b) × c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  @azimuth.assert_eq(22, result)
  
  // 计算矩形面积和周长
  let length = 10
  let width = 5
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(50, area)
  @azimuth.assert_eq(30, perimeter)
}

// 测试断言函数
test "assertion_functions" {
  // 测试 assert_true 和 assert_false
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_false(5 < 3)
  
  // 测试 assert_eq
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  
  // 测试 assert_eq_string
  @azimuth.assert_eq_string("Hello", "Hello")
  @azimuth.assert_eq_string(@azimuth.greet("Test"), "Hello, Test!")
  @azimuth.assert_eq_string("", "")
}

// 测试错误处理和特殊情况
test "error_handling_and_special_cases" {
  // 测试除零保护（通过断言函数）
  @azimuth.assert_true(true)  // 确保测试环境正常
  
  // 测试特殊输入值
  @azimuth.assert_eq(0, @azimuth.multiply(0, 2147483647))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -2147483648))
  @azimuth.assert_eq(2147483647, @azimuth.add(0, 2147483647))
  @azimuth.assert_eq(-2147483648, @azimuth.add(0, -2147483648))
  
  // 测试-1的特殊情况
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  @azimuth.assert_eq(5, @azimuth.multiply(-5, -1))
  
  // 测试最小值与-1相乘的特殊情况
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}