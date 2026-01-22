// 新的综合测试用例
// 使用标准 MoonBit 测试语法

// 测试用例1: divide_with_ceil 函数的除零处理
test divide_with_ceil_zero_division {
  // 测试除数为零的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-2147483648, 0))
}

// 测试用例2: 复杂的函数组合运算
test complex_function_composition {
  // 计算 (a + b) * c - d / e 的模拟（使用现有函数）
  let a = 10
  let b = 15
  let c = 3
  let d = 20
  let e = 4
  
  // (a + b) * c
  let sum_ab = @azimuth.add(a, b)
  let product_abc = @azimuth.multiply(sum_ab, c)
  
  // 模拟减法：加上负数
  let negate_d = @azimuth.multiply(d, -1)
  let result1 = @azimuth.add(product_abc, negate_d)
  
  // 最终结果应该是 (10 + 15) * 3 - 20 = 55
  @azimuth.assert_eq(55, result1)
  
  // 测试另一个复杂表达式：a * b + c * d
  let product_ab = @azimuth.multiply(a, b)
  let product_cd = @azimuth.multiply(c, d)
  let result2 = @azimuth.add(product_ab, product_cd)
  
  // 10 * 15 + 3 * 20 = 150 + 60 = 210
  @azimuth.assert_eq(210, result2)
}

// 测试用例3: greet 函数的字符串边界测试
test greet_string_boundary_tests {
  // 测试空白字符
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, 	!", @azimuth.greet("\t"))
  @azimuth.assert_eq_string("Hello, 
!", @azimuth.greet("\n"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet("\r"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  @azimuth.assert_eq_string("Hello, 'single'!", @azimuth.greet("'single'"))
  @azimuth.assert_eq_string("Hello, \\backslash!", @azimuth.greet("\\backslash"))
  
  // 测试数字和字母混合
  @azimuth.assert_eq_string("Hello, test123!", @azimuth.greet("test123"))
  @azimuth.assert_eq_string("Hello, 123test!", @azimuth.greet("123test"))
}

// 测试用例4: 负数边界值的乘法测试
test negative_boundary_multiplication {
  let min_val = -2147483648
  
  // 测试最小值的特殊乘法情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
  
  // 测试其他负数边界情况
  @azimuth.assert_eq(2147483647, @azimuth.multiply(-1, -2147483647))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2, 1073741824))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(1073741824, -2))
}

// 测试用例5: divide_with_ceil 的正负数混合测试
test divide_with_ceil_mixed_signs {
  // 正数除以负数
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(5, -3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(3, -3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(11, -3))
  
  // 负数除以正数
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  
  // 负数除以负数
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-5, -3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-3, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3))
}

// 测试用例6: 实际应用场景 - 订单计算
test order_calculation_scenario {
  // 模拟订单计算场景
  let item1_price = 199
  let item2_price = 299
  let item3_price = 399
  let quantity1 = 2
  let quantity2 = 1
  let quantity3 = 3
  let discount = -100  // 折扣
  
  // 计算每个项目的总价
  let total1 = @azimuth.multiply(item1_price, quantity1)
  let total2 = @azimuth.multiply(item2_price, quantity2)
  let total3 = @azimuth.multiply(item3_price, quantity3)
  
  // 计算小计
  let subtotal = @azimuth.add(total1, total2)
  subtotal = @azimuth.add(subtotal, total3)
  
  // 应用折扣
  let final_total = @azimuth.add(subtotal, discount)
  
  // 验证最终总价：199*2 + 299*1 + 399*3 - 100 = 398 + 299 + 1197 - 100 = 1794
  @azimuth.assert_eq(1794, final_total)
  
  // 为不同客户生成个性化问候
  let vip_greeting = @azimuth.greet("VIP客户")
  let regular_greeting = @azimuth.greet("普通客户")
  
  @azimuth.assert_eq_string("Hello, VIP客户!", vip_greeting)
  @azimuth.assert_eq_string("Hello, 普通客户!", regular_greeting)
}

// 测试用例7: 数学恒等式验证
test mathematical_identities {
  // 测试加法恒等式
  let x = 42
  let y = 17
  
  // 交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 结合律模拟：(a + b) + c = a + (b + c)
  let z = 23
  let left_assoc = @azimuth.add(@azimuth.add(x, y), z)
  let right_assoc = @azimuth.add(x, @azimuth.add(y, z))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法交换律：a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律模拟：a * (b + c) = a * b + a * c
  let distrib_left = @azimuth.multiply(x, @azimuth.add(y, z))
  let distrib_right = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(distrib_left, distrib_right)
}

// 测试用例8: 极值情况下的运算稳定性
test extreme_value_stability {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加零的稳定性
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  
  // 测试最小值加零的稳定性
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
  
  // 测试最大值乘以1的稳定性
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(1, max_val))
  
  // 测试最小值乘以1的稳定性
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
  
  // 测试零乘以极值的稳定性
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

// 测试用例9: divide_with_ceil 的精度测试
test divide_with_ceil_precision_tests {
  // 测试接近整数的除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  
  // 测试大数除法
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(3000000, 3))
  @azimuth.assert_eq(1000001, @azimuth.divide_with_ceil(3000001, 3))
  
  // 测试小数除法（整数结果）
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(2, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(100, 100))
  
  // 测试负数精度
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
}

// 测试用例10: 字符串处理性能和长度测试
test greet_string_performance_tests {
  // 测试非常长的字符串
  let very_long_name = "ThisIsAnExtremelyLongUserNameThatMightBeUsedInSomeSystemToTestThePerformanceAndStabilityOfTheGreetFunctionWhenHandlingLongStrings"
  let long_result = @azimuth.greet(very_long_name)
  let expected_long = "Hello, " + very_long_name + "!"
  @azimuth.assert_eq_string(expected_long, long_result)
  
  // 测试包含各种Unicode字符的字符串
  let unicode_name = "用户🌙MoonBit测试123"
  let unicode_result = @azimuth.greet(unicode_name)
  let expected_unicode = "Hello, " + unicode_name + "!"
  @azimuth.assert_eq_string(expected_unicode, unicode_result)
  
  // 测试包含空格的复合名称
  let full_name = "John Doe Jr."
  let full_result = @azimuth.greet(full_name)
  @azimuth.assert_eq_string("Hello, John Doe Jr.!", full_result)
}