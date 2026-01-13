// 额外的 MoonBit 测试用例
// 测试 azimuth 包中的更多功能和边界情况

// 测试 add 函数的极端边界值
test "add_extreme_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与较小正数的相加
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 100, 200))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1000, 2000))
  
  // 测试最小值与较小负数的相加
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 100, -200))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1000, -2000))
}

// 测试 multiply 函数的极端边界值
test "multiply_extreme_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近最大值的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(46340, 46341))
  @azimuth.assert_eq(max_val, @azimuth.multiply(65536, 32768))
  
  // 测试最小值的特殊乘法情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(2, min_val))
}

// 测试 greet 函数的复杂字符串处理
test "greet_complex_string_processing" {
  // 测试包含各种特殊字符的字符串
  @azimuth.assert_eq_string("Hello, \n\t\r!", @azimuth.greet("\n\t\r"))
  @azimuth.assert_eq_string("Hello, \"\'!", @azimuth.greet("\"\'"))
  @azimuth.assert_eq_string("Hello, \0!", @azimuth.greet("\0"))
  
  // 测试非常长的字符串
  let long_name = "A" + "B" + "C" + "D" + "E" + "F" + "G" + "H" + "I" + "J" + "K" + "L" + "M" + "N" + "O" + "P" + "Q" + "R" + "S" + "T"
  @azimuth.assert_eq_string("Hello, ABCDEFGHIJKLMNOPQRST!", @azimuth.greet(long_name))
}

// 测试复合运算场景
test "compound_arithmetic_operations" {
  // 测试 (a + b) * c 的复合运算
  let a = 10
  let b = 20
  let c = 3
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(90, result)
  
  // 测试 a * b + c 的复合运算
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(203, result2)
}

// 测试数学恒等式
test "mathematical_identities" {
  // 测试加法恒等式
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  
  // 测试乘法恒等式
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  
  // 测试负数恒等式
  @azimuth.assert_eq(0, @azimuth.add(42, @azimuth.multiply(-1, 42)))
  @azimuth.assert_eq(0, @azimuth.multiply(-1, @azimuth.multiply(-1, 0)))
}

// 测试错误输入处理
test "error_input_handling" {
  // 测试连续的加法运算
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result1)
  
  // 测试连续的乘法运算
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result2)
  
  // 测试混合运算
  let result3 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result3)
}

// 测试实际应用场景
test "real_world_application_scenarios" {
  // 场景1：计算购物车总价
  let item1_price = 199
  let item2_price = 299
  let item3_price = 399
  let quantity1 = 2
  let quantity2 = 1
  let quantity3 = 3
  
  let total1 = @azimuth.multiply(item1_price, quantity1)
  let total2 = @azimuth.multiply(item2_price, quantity2)
  let total3 = @azimuth.multiply(item3_price, quantity3)
  let cart_total = @azimuth.add(@azimuth.add(total1, total2), total3)
  
  @azimuth.assert_eq(1994, cart_total)
  
  // 场景2：计算平均分
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let score4 = 80
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4)
  let average = @azimuth.multiply(sum, 25)  // 除以4等同于乘以0.25，这里用整数近似
  @azimuth.assert_eq(8750, average)  // 350 * 25 = 8750，表示87.5分
}

// 测试边界值组合
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试最大值与自身的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 测试最小值与自身的运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

// 测试字符串处理的边界情况
test "string_processing_edge_cases" {
  // 测试单个字符的字符串
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 1!", @azimuth.greet("1"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet("!"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, A B C!", @azimuth.greet("A B C"))
  
  // 测试数字和字母混合的字符串
  @azimuth.assert_eq_string("Hello, Test123!", @azimuth.greet("Test123"))
  @azimuth.assert_eq_string("Hello, 123Test!", @azimuth.greet("123Test"))
}