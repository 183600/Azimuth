// 增强的覆盖率测试用例
// 测试 azimuth 包中的各种边界情况和复杂场景

// 测试1: 验证加法交换律
test "add_commutative_property" {
  let a = 12345
  let b = -67890
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.add(100, 200), @azimuth.add(200, 100))
}

// 测试2: 验证乘法交换律
test "multiply_commutative_property" {
  let a = 15
  let b = -7
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  @azimuth.assert_eq(@azimuth.multiply(8, 12), @azimuth.multiply(12, 8))
}

// 测试3: 验证分配律 a * (b + c) = a * b + a * c
test "distributive_property" {
  let a = 5
  let b = 10
  let c = 15
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

// 测试4: 大数运算边界测试
test "large_number_boundary_operations" {
  let large_positive = 1000000000
  let large_negative = -1000000000
  
  // 测试大数相加
  let sum = @azimuth.add(large_positive, large_positive)
  @azimuth.assert_true(sum > 0)
  
  // 测试大数相乘
  let product = @azimuth.multiply(1000, 1000)
  @azimuth.assert_eq(1000000, product)
  
  // 测试大负数相加
  let neg_sum = @azimuth.add(large_negative, large_negative)
  @azimuth.assert_true(neg_sum < 0)
}

// 测试5: 最小值特殊处理测试
test "minimum_value_special_handling" {
  let min_val = -2147483648
  
  // 测试最小值加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(-1, min_val))
  
  // 测试最小值乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(2, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

// 测试6: 长字符串和特殊字符测试
test "long_string_and_special_characters" {
  let long_name = "VeryLongNameThatExceedsNormalLengthExpectations"
  let result1 = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, VeryLongNameThatExceedsNormalLengthExpectations!", result1)
  
  // 测试包含空格的字符串
  let name_with_spaces = "John Doe"
  let result2 = @azimuth.greet(name_with_spaces)
  @azimuth.assert_eq_string("Hello, John Doe!", result2)
  
  // 测试包含特殊符号的字符串
  let name_with_symbols = "test@example.com"
  let result3 = @azimuth.greet(name_with_symbols)
  @azimuth.assert_eq_string("Hello, test@example.com!", result3)
}

// 测试7: 混合正负数的复杂运算
test "mixed_positive_negative_operations" {
  let values = [-10, -5, 0, 5, 10]
  
  // 测试所有数对的加法
  let sum1 = @azimuth.add(@azimuth.add(values[0], values[2]), values[4])
  @azimuth.assert_eq(0, sum1)
  
  // 测试混合乘法
  let product1 = @azimuth.multiply(values[1], values[3])
  @azimuth.assert_eq(-25, product1)
  
  let product2 = @azimuth.multiply(values[0], values[4])
  @azimuth.assert_eq(-100, product2)
}

// 测试8: 连续运算场景测试
test "sequential_operations" {
  // 连续加法
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result1)
  
  // 连续乘法
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result2)
  
  // 混合连续运算
  let result3 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result3)
}

// 测试9: 实际应用场景 - 计算总价
test "real_world_price_calculation" {
  let item_price = 199
  let quantity = 5
  let tax_rate = 10
  let discount = 50
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算最终价格
  let total = @azimuth.add(subtotal, tax)
  let final_total = @azimuth.add(total, -discount)
  
  @azimuth.assert_true(final_total > 0)
  @azimuth.assert_true(final_total < subtotal)
}

// 测试10: 边界条件组合测试
test "boundary_condition_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与各种值的组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(1, max_val))
  
  // 测试最小值与各种值的组合
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  
  // 测试极值运算
  let extreme_result = @azimuth.add(@azimuth.multiply(1000000, 1000), -500000000)
  @azimuth.assert_eq(500000000, extreme_result)
}