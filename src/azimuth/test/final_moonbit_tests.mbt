// 最终 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "string_length_validation" {
  // 测试字符串长度验证
  let short_name = "Amy"
  let medium_name = "Elizabeth"
  let long_name = " Alexandra Victoria "
  
  @azimuth.assert_eq_string("Hello, Amy!", @azimuth.greet(short_name))
  @azimuth.assert_eq_string("Hello, Elizabeth!", @azimuth.greet(medium_name))
  @azimuth.assert_eq_string("Hello,  Alexandra Victoria !", @azimuth.greet(long_name))
}

test "arithmetics_with_negative_numbers" {
  // 测试负数算术运算
  let a = -15
  let b = -7
  let c = 10
  
  // 负数相加
  @azimuth.assert_eq(-22, @azimuth.add(a, b))
  
  // 正负数相加
  @azimuth.assert_eq(-5, @azimuth.add(a, c))
  
  // 负数相乘
  @azimuth.assert_eq(105, @azimuth.multiply(a, b))
  
  // 正负数相乘
  @azimuth.assert_eq(-150, @azimuth.multiply(a, c))
}

test "overflow_edge_cases" {
  // 测试溢出边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加小正数
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 最小值减小负数
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 最大值乘以2
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值乘以-1（特殊情况）
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  
  // 加法恒等式：x + 0 = x
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等式：x * 1 = x
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  
  // 乘法零元：x * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, x))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算：(a + b) * (c - d) + e * f
  let a = 12
  let b = 8
  let c = 20
  let d = 5
  let e = 3
  let f = 7
  
  let sum_ab = @azimuth.add(a, b)        // 20
  let diff_cd = @azimuth.add(c, -d)      // 15
  let product1 = @azimuth.multiply(sum_ab, diff_cd)  // 300
  let product2 = @azimuth.multiply(e, f)  // 21
  let result = @azimuth.add(product1, product2)  // 321
  
  @azimuth.assert_eq(321, result)
}

test "real_world_calculations" {
  // 测试实际应用计算
  // 计算折扣价格：原价 * (1 - 折扣率) + 税费
  let original_price = 1000
  let discount_rate = 20  // 20%
  let tax_rate = 8       // 8%
  
  // 计算折后价格
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate) / 100
  
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax_amount)
  
  @azimuth.assert_eq(864, final_price)  // 1000*(1-0.2) + 1000*0.8*0.08 = 800 + 64 = 864
}

test "string_special_characters" {
  // 测试特殊字符字符串
  let punctuation = "Hello, World!"
  let symbols = "user@domain.com"
  let mixed = "John_Doe-123"
  
  @azimuth.assert_eq_string("Hello, Hello, World!!", @azimuth.greet(punctuation))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet(symbols))
  @azimuth.assert_eq_string("Hello, John_Doe-123!", @azimuth.greet(mixed))
}

test "calculation_precision" {
  // 测试计算精度
  // 使用整数运算模拟浮点运算
  let value = 12345
  let divisor = 100
  
  // 模拟 123.45 的整数表示
  let scaled_value = @azimuth.multiply(value, divisor)
  let result = scaled_value / divisor
  
  @azimuth.assert_eq(12345, result)
  
  // 测试大数计算
  let big_num1 = 1000000
  let big_num2 = 2000000
  let big_sum = @azimuth.add(big_num1, big_num2)
  let big_product = @azimuth.multiply(1000, 3000)
  
  @azimuth.assert_eq(3000000, big_sum)
  @azimuth.assert_eq(3000000, big_product)
}

test "conditional_calculations" {
  // 测试条件计算
  let x = 15
  let y = 25
  let z = 10
  
  // 如果 x < y，则计算 x + z，否则计算 y + z
  let condition1 = x < y
  let result1 = if condition1 { @azimuth.add(x, z) } else { @azimuth.add(y, z) }
  @azimuth.assert_eq(25, result1)  // 15 + 10 = 25
  
  // 如果 x > y，则计算 x * z，否则计算 y * z
  let condition2 = x > y
  let result2 = if condition2 { @azimuth.multiply(x, z) } else { @azimuth.multiply(y, z) }
  @azimuth.assert_eq(250, result2)  // 25 * 10 = 250
  
  // 嵌套条件
  let nested_result = 
    if x < y {
      if z > 5 { @azimuth.add(x, y) } else { @azimuth.multiply(x, y) }
    } else {
      @azimuth.add(x, z)
    }
  @azimuth.assert_eq(40, nested_result)  // 15 + 25 = 40
}

test "performance_optimization" {
  // 测试性能优化场景
  let base = 100
  
  // 使用加法代替乘法（对于小的乘数）
  let multiply_by_3 = @azimuth.multiply(base, 3)
  let add_three_times = @azimuth.add(@azimuth.add(base, base), base)
  @azimuth.assert_eq(multiply_by_3, add_three_times)
  
  // 使用位运算（模拟）代替乘法
  let multiply_by_4 = @azimuth.multiply(base, 4)
  let double_twice = @azimuth.multiply(@azimuth.multiply(base, 2), 2)
  @azimuth.assert_eq(multiply_by_4, double_twice)
  
  // 测试批量计算优化
  let num1 = 10
  let num2 = 20
  let num3 = 30
  let num4 = 40
  let num5 = 50
  
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(num1, num2), num3), num4), num5)
  
  // 使用分组计算优化
  let group1 = @azimuth.add(num1, num5)  // 10 + 50 = 60
  let group2 = @azimuth.add(num2, num4)  // 20 + 40 = 60
  let sum2 = @azimuth.add(@azimuth.add(group1, group2), num3)  // 60 + 60 + 30 = 150
  
  @azimuth.assert_eq(150, sum1)
  @azimuth.assert_eq(150, sum2)
}