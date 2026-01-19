// 额外的标准测试用例 - 补充现有测试覆盖范围

// 测试极端边界值的加法运算
test "extreme_boundary_addition" {
  // 测试接近边界的值
  @azimuth.assert_eq(2147483646, @azimuth.add(2147483645, 1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(-2147483646, -1))
  
  // 测试边界值与接近边界值的组合
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(2147483647, @azimuth.add(1, 2147483646))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-1, -2147483647))
}

// 测试极端边界值的乘法运算
test "extreme_boundary_multiplication" {
  // 测试大数相乘
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46340, 46341))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46341, 46340))
  
  // 测试负数大数相乘
  @azimuth.assert_eq(2147483647, @azimuth.multiply(-46340, -46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-46340, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(46340, -46341))
}

// 测试混合运算的优先级和组合
test "mixed_arithmetic_operations" {
  // 测试连续加法
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result1)
  
  // 测试连续乘法
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result2)
  
  // 测试加法和乘法混合（模拟优先级）
  let a = 2
  let b = 3
  let c = 4
  let d = 5
  
  // (a + b) * (c + d)
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(c, d)
  let result3 = @azimuth.multiply(sum1, sum2)
  @azimuth.assert_eq(35, result3)
  
  // a * b + c * d
  let product1 = @azimuth.multiply(a, b)
  let product2 = @azimuth.multiply(c, d)
  let result4 = @azimuth.add(product1, product2)
  @azimuth.assert_eq(26, result4)
}

// 测试字符串处理的边界情况
test "string_processing_edge_cases" {
  // 测试长字符串
  let long_name = "ThisIsAVeryLongNameThatMightCauseIssuesInSomeSystemsButShouldWorkFineInMoonBit"
  let result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, " + long_name + "!", result)
  
  // 测试包含引号的字符串
  @azimuth.assert_eq_string("Hello, \"John\"!", @azimuth.greet("\"John\""))
  
  // 测试包含反斜杠的字符串
  @azimuth.assert_eq_string("Hello, \\path\\!", @azimuth.greet("\\path\\"))
  
  // 测试包含各种空白字符的字符串
  @azimuth.assert_eq_string("Hello, \t\n\r!", @azimuth.greet("\t\n\r"))
}

// 测试负数运算的边界情况
test "negative_number_operations" {
  // 测试负数加法的各种组合
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-7, @azimuth.add(-10, 3))
  @azimuth.assert_eq(3, @azimuth.add(-7, 10))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  
  // 测试负数乘法的各种组合
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-21, @azimuth.multiply(-3, 7))
  @azimuth.assert_eq(-21, @azimuth.multiply(3, -7))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

// 测试零值在各种运算中的行为
test "zero_value_behavior" {
  // 测试加法中的零值
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 测试乘法中的零值
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  
  // 测试零值与边界值的组合
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483648, 0))
}

// 测试断言函数的边界情况
test "assertion_functions_boundary" {
  // 测试assert_eq在边界值上的表现
  @azimuth.assert_eq(2147483647, 2147483647)
  @azimuth.assert_eq(-2147483648, -2147483648)
  @azimuth.assert_eq(0, 0)
  
  // 测试assert_eq_string在特殊字符串上的表现
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
  @azimuth.assert_eq_string("特殊字符测试", "特殊字符测试")
  
  // 测试assert_true和assert_false
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_false(1 == 2)
}

// 测试连续运算的累积效应
test "cumulative_operations" {
  // 测试连续加法的累积效应
  let mut sum = 0
  sum = @azimuth.add(sum, 1)
  sum = @azimuth.add(sum, 2)
  sum = @azimuth.add(sum, 3)
  sum = @azimuth.add(sum, 4)
  sum = @azimuth.add(sum, 5)
  @azimuth.assert_eq(15, sum)
  
  // 测试连续乘法的累积效应
  let mut product = 1
  product = @azimuth.multiply(product, 2)
  product = @azimuth.multiply(product, 3)
  product = @azimuth.multiply(product, 4)
  @azimuth.assert_eq(24, product)
  
  // 测试混合连续运算
  let mut result = 10
  result = @azimuth.add(result, 5)  // 15
  result = @azimuth.multiply(result, 2)  // 30
  result = @azimuth.add(result, -10)  // 20
  result = @azimuth.multiply(result, 3)  // 60
  @azimuth.assert_eq(60, result)
}

// 测试实际应用场景
test "practical_application_scenarios" {
  // 场景1：计算购物车总价（使用加法）
  let item1_price = 199
  let item2_price = 299
  let item3_price = 99
  let shipping = 15
  let total = @azimuth.add(@azimuth.add(@azimuth.add(item1_price, item2_price), item3_price), shipping)
  @azimuth.assert_eq(612, total)
  
  // 场景2：计算订单总量（单价 × 数量）
  let unit_price = 25
  let quantity = 8
  let order_total = @azimuth.multiply(unit_price, quantity)
  @azimuth.assert_eq(200, order_total)
  
  // 场景3：生成个性化问候消息
  let customer_name = "张三"
  let greeting = @azimuth.greet(customer_name)
  @azimuth.assert_eq_string("Hello, 张三!", greeting)
  
  // 场景4：计算折扣价格（原价 × 折扣率）
  let original_price = 1000
  let discount_rate = 8  // 表示80%折扣（保留20%）
  let discounted_price = @azimuth.multiply(original_price, discount_rate)
  // 注意：这里假设discount_rate已经是百分比形式，实际应用中可能需要除法
  @azimuth.assert_eq(8000, discounted_price)
}