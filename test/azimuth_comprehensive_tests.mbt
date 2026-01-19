// Azimuth 综合测试用例
// 测试核心功能的各种场景和边界情况

test "add_comprehensive_range_test" {
  // 测试加法的全面范围
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(1000, -1000))
  @azimuth.assert_eq(-200, @azimuth.add(-100, -100))
  @azimuth.assert_eq(1500, @azimuth.add(1000, 500))
  
  // 测试大数相加
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(-3000000, @azimuth.add(-1000000, -2000000))
}

test "multiply_comprehensive_range_test" {
  // 测试乘法的全面范围
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(-30, @azimuth.multiply(6, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  
  // 测试大数相乘
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-2000000, @azimuth.multiply(1000, -2000))
}

test "greet_function_special_characters" {
  // 测试 greet 函数的特殊字符处理
  @azimuth.assert_eq_string("Hello, John_Doe!", @azimuth.greet("John_Doe"))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
  @azimuth.assert_eq_string("Hello, C++!", @azimuth.greet("C++"))
  @azimuth.assert_eq_string("Hello, Python/Java!", @azimuth.greet("Python/Java"))
  @azimuth.assert_eq_string("Hello, $100!", @azimuth.greet("$100"))
}

test "boundary_value_extreme_test" {
  // 测试边界值的极端情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 最小值运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 极值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "mathematical_sequence_test" {
  // 测试数学序列计算
  // 计算等差数列：2, 4, 6, 8, 10 的和
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(2, 4), 6), 8), 10)
  @azimuth.assert_eq(30, sum1)
  
  // 计算等比数列：2, 4, 8, 16 的积
  let product1 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 4), 8), 16)
  @azimuth.assert_eq(1024, product1)
}

test "real_world_application_test" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  
  // 计算总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(total1, total2)
  let final_total = @azimuth.add(subtotal, shipping_fee)
  
  @azimuth.assert_eq(717, final_total)  // 199*2 + 299*1 + 20 = 398 + 299 + 20 = 717
}

test "complex_expression_test" {
  // 测试复杂表达式计算
  // 计算：(a + b) * (c - d) + e * f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  let sum_ab = @azimuth.add(a, b)  // 15
  let diff_cd = @azimuth.add(c, -d)  // 12
  let product1 = @azimuth.multiply(sum_ab, diff_cd)  // 180
  let product2 = @azimuth.multiply(e, f)  // 21
  let result = @azimuth.add(product1, product2)  // 201
  
  @azimuth.assert_eq(201, result)
}

test "string_processing_comprehensive_test" {
  // 测试字符串处理的综合情况
  // 测试包含各种字符的字符串
  @azimuth.assert_eq_string("Hello, Test123!", @azimuth.greet("Test123"))
  @azimuth.assert_eq_string("Hello, 测试123!", @azimuth.greet("测试123"))
  @azimuth.assert_eq_string("Hello, aBcDeF!", @azimuth.greet("aBcDeF"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced  "))
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet("!@#$%^&*()"))
}

test "overflow_protection_comprehensive_test" {
  // 测试溢出保护的全面情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(1000000, 3000))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1000000, 3000))
}

test "conditional_calculation_test" {
  // 测试条件计算场景
  let base_value = 100
  let multiplier = 2
  let addend = 50
  
  // 根据条件选择不同的计算方式
  let use_multiplication = true
  let result1 = if use_multiplication {
    @azimuth.multiply(base_value, multiplier)
  } else {
    @azimuth.add(base_value, addend)
  }
  
  @azimuth.assert_eq(200, result1)
  
  let use_multiplication2 = false
  let result2 = if use_multiplication2 {
    @azimuth.multiply(base_value, multiplier)
  } else {
    @azimuth.add(base_value, addend)
  }
  
  @azimuth.assert_eq(150, result2)
}