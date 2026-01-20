// 增强功能测试用例
// 测试一些特殊场景和边界条件

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))  // 10/3 = 3.33... 向上取整为 4
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))  // 11/3 = 3.66... 向上取整为 4
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))   // 6/3 = 2，无需向上取整
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 3))   // 1/3 = 0.33... 向上取整为 1
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -10/3 = -3.33... 向上取整为 -3
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-5, 3))   // -5/3 = -1.66... 向上取整为 -1
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))  // -11/3 = -3.66... 向上取整为 -3
}

test "add_cascading_operations" {
  // 测试级联加法操作
  let result = @azimuth.add(@azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4)), @azimuth.add(5, 6))
  @azimuth.assert_eq(21, result)  // (1+2) + (3+4) + (5+6) = 3 + 7 + 11 = 21
  
  // 测试包含负数的级联操作
  let result2 = @azimuth.add(@azimuth.add(@azimuth.add(10, -5), @azimuth.add(-3, 8)), @azimuth.add(-2, 7))
  @azimuth.assert_eq(15, result2)  // (10-5) + (-3+8) + (-2+7) = 5 + 5 + 5 = 15
}

test "multiply_nested_operations" {
  // 测试嵌套乘法操作
  let result = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result)  // (2*3) * (4*5) = 6 * 20 = 120
  
  // 测试包含负数的嵌套操作
  let result2 = @azimuth.multiply(@azimuth.multiply(-2, 3), @azimuth.multiply(4, -5))
  @azimuth.assert_eq(120, result2)  // (-2*3) * (4*-5) = -6 * -20 = 120
}

test "complex_arithmetic_expression" {
  // 测试复杂算术表达式：(a + b) * c - d / e
  let a = 10
  let b = 5
  let c = 3
  let d = 20
  let e = 4
  
  let sum_ab = @azimuth.add(a, b)  // 15
  let product = @azimuth.multiply(sum_ab, c)  // 45
  let division = d / e  // 5
  let result = @azimuth.add(product, -division)  // 40
  
  @azimuth.assert_eq(40, result)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含数字和特殊字符的字符串
  let numeric_string = "12345"
  let result1 = @azimuth.greet(numeric_string)
  @azimuth.assert_eq_string("Hello, 12345!", result1)
  
  // 测试包含空格的字符串
  let spaced_string = " John Doe "
  let result2 = @azimuth.greet(spaced_string)
  @azimuth.assert_eq_string("Hello,  John Doe !", result2)
  
  // 测试包含引号的字符串
  let quoted_string = "\"Alice\""
  let result3 = @azimuth.greet(quoted_string)
  @azimuth.assert_eq_string("Hello, \"Alice\"!", result3)
}

test "overflow_protection_comprehensive" {
  // 全面测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  
  // 验证加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 验证乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证分配律
  let c = 5
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "real_world_practical_scenarios" {
  // 实际应用场景测试
  // 场景1：计算购物车总价
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(797, cart_total)  // 299*2 + 199*1 = 598 + 199 = 797
  
  // 场景2：计算平均分数
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let score4 = 80
  
  let sum_scores = @azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4)
  let average_score = sum_scores / 4
  
  @azimuth.assert_eq(87, average_score)  // (85+90+95+80)/4 = 350/4 = 87
}

test "boundary_combination_scenarios" {
  // 边界值组合场景测试
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  
  // 测试最大值与零的组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(max_val, zero))
  
  // 测试最小值与零的组合
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(min_val, zero))
  
  // 测试最大值与最小值的组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}