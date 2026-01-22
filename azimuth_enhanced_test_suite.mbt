// Azimuth 增强测试套件
// 使用标准 MoonBit 测试语法，覆盖核心功能的全面测试

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(5, 2))
  @azimuth.assert_eq(10, @azimuth.divide_with_ceil(100, 10))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 2))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(5, -2))
}

test "divide_with_ceil_zero_division" {
  // 测试零除法的处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
}

test "greet_function_empty_and_special_cases" {
  // 测试问候函数的空字符串和特殊情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !@#$!", @azimuth.greet("!@#$"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

test "greet_function_unicode_and_emoji" {
  // 测试问候函数的Unicode和emoji支持
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, naïve!", @azimuth.greet("naïve"))
  @azimuth.assert_eq_string("Hello, résumé!", @azimuth.greet("résumé"))
}

test "add_function_large_numbers" {
  // 测试加法函数的大数处理
  @azimuth.assert_eq(3000000, @azimuth.add(1000000, 2000000))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
  @azimuth.assert_eq(0, @azimuth.add(500000, -500000))
  @azimuth.assert_eq(2000, @azimuth.add(1000, 1000))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(99999, @azimuth.multiply(1, 99999))
  @azimuth.assert_eq(-100, @azimuth.multiply(-1, 100))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

test "comprehensive_calculation_scenarios" {
  // 测试综合计算场景
  let base_price = 100
  let quantity = 5
  let tax_rate = 10
  let discount = 20
  
  let subtotal = @azimuth.multiply(base_price, quantity)
  let total_before_discount = @azimuth.add(subtotal, tax_rate)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(500, subtotal)
  @azimuth.assert_eq(510, total_before_discount)
  @azimuth.assert_eq(490, final_total)
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 7
  let b = 11
  let c = 13
  
  // 交换律
  let add_comm1 = @azimuth.add(a, b)
  let add_comm2 = @azimuth.add(b, a)
  let mul_comm1 = @azimuth.multiply(a, b)
  let mul_comm2 = @azimuth.multiply(b, a)
  
  // 结合律
  let add_assoc1 = @azimuth.add(@azimuth.add(a, b), c)
  let add_assoc2 = @azimuth.add(a, @azimuth.add(b, c))
  
  @azimuth.assert_eq(add_comm1, add_comm2)
  @azimuth.assert_eq(mul_comm1, mul_comm2)
  @azimuth.assert_eq(add_assoc1, add_assoc2)
}

test "performance_boundary_tests" {
  // 测试性能边界情况
  // 小数累加
  let sum_small = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  
  // 大数累加
  let sum_large = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(100000, 200000), 300000), 400000), 500000)
  
  // 嵌套计算
  let nested = @azimuth.multiply(@azimuth.add(10, 20), @azimuth.add(5, 15))
  
  @azimuth.assert_eq(15, sum_small)
  @azimuth.assert_eq(1500000, sum_large)
  @azimuth.assert_eq(600, nested)
}