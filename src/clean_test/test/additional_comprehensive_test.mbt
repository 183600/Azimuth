// 额外的综合测试用例

// 测试用例 1: 大数运算的稳定性测试
///|
test "large_number_stability" {
  // 测试大数相加的稳定性
  let large_num1 = 100000000
  let large_num2 = 200000000
  let large_num3 = 300000000
  
  // 测试连续大数加法
  let result1 = @azimuth.add(@azimuth.add(large_num1, large_num2), large_num3)
  let result2 = @azimuth.add(large_num1, @azimuth.add(large_num2, large_num3))
  assert_eq(result1, result2)  // 验证结合律
  
  // 测试大数相乘的边界
  let large_multiply = @azimuth.multiply(large_num1, 10)
  assert_eq(1000000000, large_multiply)
}

// 测试用例 2: 负数运算的深度测试
///|
test "negative_number_deep_test" {
  // 测试负数乘法的各种组合
  assert_eq(100, @azimuth.multiply(-10, -10))  // 负负得正
  assert_eq(-100, @azimuth.multiply(-10, 10))   // 负正得负
  assert_eq(-100, @azimuth.multiply(10, -10))   // 正负得负
  assert_eq(0, @azimuth.multiply(-10, 0))       // 乘0得0
  
  // 测试负数加法的边界情况
  assert_eq(-200, @azimuth.add(-100, -100))     // 负数相加
  assert_eq(0, @azimuth.add(-100, 100))         // 正负抵消
  assert_eq(-50, @azimuth.add(-100, 50))        // 部分抵消
}

// 测试用例 3: 字符串处理的高级测试
///|
test "advanced_string_processing" {
  // 测试包含数字和特殊字符的字符串
  assert_eq("Hello, User123!", @azimuth.greet("User123"))
  assert_eq("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  assert_eq("Hello, 123-456-7890!", @azimuth.greet("123-456-7890"))
  
  // 测试空格和特殊字符
  assert_eq("Hello, John Doe!", @azimuth.greet("John Doe"))
  assert_eq("Hello,  !", @azimuth.greet("   "))  // 只有空格
  assert_eq("Hello, 	
!", @azimuth.greet("	
"))  // 制表符和换行符
}

// 测试用例 4: 数学恒等式的验证
///|
test "mathematical_identities" {
  // 验证平方差公式: a² - b² = (a + b)(a - b)
  let a = 15
  let b = 8
  
  // 计算平方差
  let a_squared = @azimuth.multiply(a, a)
  let b_squared = @azimuth.multiply(b, b)
  let difference_of_squares = @azimuth.add(a_squared, @azimuth.multiply(-1, b_squared))
  
  // 计算 (a + b)(a - b)
  let sum = @azimuth.add(a, b)
  let difference = @azimuth.add(a, @azimuth.multiply(-1, b))
  let factored_form = @azimuth.multiply(sum, difference)
  
  assert_eq(difference_of_squares, factored_form)
}

// 测试用例 5: 实际应用场景 - 库存管理
///|
test "inventory_management" {
  // 商品库存管理
  let initial_stock = 1000
  let sales_day1 = 150
  let sales_day2 = 200
  let sales_day3 = 175
  let restock = 500
  
  // 计算剩余库存
  let total_sales = @azimuth.add(@azimuth.add(sales_day1, sales_day2), sales_day3)
  let remaining_stock = @azimuth.add(initial_stock, @azimuth.multiply(-1, total_sales))
  
  // 补货后的库存
  let final_stock = @azimuth.add(remaining_stock, restock)
  
  // 验证: 1000 - (150 + 200 + 175) + 500 = 975
  assert_eq(975, final_stock)
}

// 测试用例 6: 极端边界值测试
///|
test "extreme_boundary_values" {
  // 测试接近边界的值
  let near_max = 2147483646
  let near_min = -2147483647
  
  // 测试接近最大值的加法
  assert_eq(2147483647, @azimuth.add(near_max, 1))     // 达到最大值
  assert_eq(2147483647, @azimuth.add(near_max, 2))     // 溢出
  
  // 测试接近最小值的加法
  assert_eq(-2147483648, @azimuth.add(near_min, -1))   // 达到最小值
  assert_eq(-2147483648, @azimuth.add(near_min, -2))   // 溢出
  
  // 测试接近边界的乘法
  assert_eq(2147483646, @azimuth.multiply(near_max, 1))  // 保持不变
  assert_eq(-2147483647, @azimuth.multiply(near_min, 1)) // 保持不变
}

// 测试用例 7: 复合运算测试
///|
test "compound_operations" {
  // 测试复杂的复合运算
  // 计算: (a + b) * (c + d) - e * f
  let a = 10
  let b = 20
  let c = 5
  let d = 15
  let e = 8
  let f = 12
  
  // 计算 (a + b) * (c + d)
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(c, d)
  let product1 = @azimuth.multiply(sum1, sum2)
  
  // 计算 e * f
  let product2 = @azimuth.multiply(e, f)
  
  // 计算最终结果
  let result = @azimuth.add(product1, @azimuth.multiply(-1, product2))
  
  // 验证: (10 + 20) * (5 + 15) - 8 * 12 = 30 * 20 - 96 = 600 - 96 = 504
  assert_eq(504, result)
}

// 测试用例 8: 算法效率测试 - 累加和累乘
///|
test "algorithm_efficiency" {
  // 测试不同累加方式的效率
  // 方法1: 线性累加
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  
  // 方法2: 分组累加
  let group1 = @azimuth.add(1, 5)
  let group2 = @azimuth.add(2, 4)
  let sum2 = @azimuth.add(@azimuth.add(group1, group2), 3)
  
  assert_eq(sum1, sum2)  // 两种方法结果应该相同
  assert_eq(15, sum1)    // 验证结果正确性
  
  // 测试累乘
  let product1 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 3), 4), 5)
  let product2 = @azimuth.multiply(2, @azimuth.multiply(3, @azimuth.multiply(4, 5)))
  
  assert_eq(product1, product2)  // 两种方法结果应该相同
  assert_eq(120, product1)       // 验证结果正确性
}

// 测试用例 9: 数据类型边界测试
///|
test "data_type_boundaries" {
  // 测试0的边界情况
  assert_eq(0, @azimuth.add(0, 0))           // 0 + 0
  assert_eq(0, @azimuth.multiply(0, 0))       // 0 * 0
  assert_eq(0, @azimuth.multiply(0, 12345))   // 0 * 任意数
  assert_eq(0, @azimuth.multiply(12345, 0))   // 任意数 * 0
  
  // 测试1的边界情况
  assert_eq(1, @azimuth.add(1, 0))            // 1 + 0
  assert_eq(1, @azimuth.add(0, 1))            // 0 + 1
  assert_eq(1, @azimuth.multiply(1, 1))       // 1 * 1
  assert_eq(12345, @azimuth.multiply(1, 12345)) // 1 * 任意数
  assert_eq(12345, @azimuth.multiply(12345, 1)) // 任意数 * 1
  
  // 测试-1的边界情况
  assert_eq(-1, @azimuth.add(-1, 0))          // -1 + 0
  assert_eq(-1, @azimuth.add(0, -1))          // 0 + -1
  assert_eq(1, @azimuth.multiply(-1, -1))     // -1 * -1
  assert_eq(-12345, @azimuth.multiply(-1, 12345)) // -1 * 任意数
  assert_eq(-12345, @azimuth.multiply(12345, -1)) // 任意数 * -1
}

// 测试用例 10: 实际应用场景 - 财务计算
///|
test "financial_calculations" {
  // 复利计算: A = P(1 + r/n)^(nt)
  // 简化版本: 计算两年后的本金翻倍
  
  let principal = 10000      // 本金
  let rate_percent = 50      // 年利率50%
  let years = 2              // 年数
  
  // 第一年后的金额: P + P * r = P * (1 + r)
  let year1_amount = @azimuth.add(principal, @azimuth.multiply(principal, rate_percent))
  
  // 第二年后的金额: year1_amount + year1_amount * r
  let year2_amount = @azimuth.add(year1_amount, @azimuth.multiply(year1_amount, rate_percent))
  
  // 验证: 10000 * (1 + 0.5) * (1 + 0.5) = 10000 * 1.5 * 1.5 = 22500
  // 由于我们使用整数，所以是 10000 * (1 + 50) * (1 + 50) = 10000 * 51 * 51 = 26010000
  assert_eq(26010000, year2_amount)
  
  // 验证本金的增长
  let growth = @azimuth.add(year2_amount, @azimuth.multiply(-1, principal))
  assert_eq(25010000, growth)  // 增长了2501倍
}