// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

// 测试加法函数的边界情况
test "add_boundary_values" {
  // 测试接近边界的值
  let max_val = 2147483646
  let min_val = -2147483647
  
  // 测试接近最大值的加法
  @clean_test.assert_eq(2147483647, @clean_test.add(max_val, 1))
  
  // 测试接近最小值的加法
  @clean_test.assert_eq(-2147483648, @clean_test.add(min_val, -1))
  
  // 测试正负边界值相加
  @clean_test.assert_eq(-1, @clean_test.add(max_val, min_val))
}

// 测试乘法函数的边界情况
test "multiply_boundary_values" {
  // 测试大数相乘
  let large_num = 46340  // sqrt(2147483647) 的近似值
  
  // 测试接近边界的乘法
  @clean_test.assert_eq(2147395600, @clean_test.multiply(large_num, large_num))
  
  // 测试负数边界
  @clean_test.assert_eq(-2147395600, @clean_test.multiply(large_num, -large_num))
}

// 测试字符串处理函数
test "greet_function_edge_cases" {
  // 测试空字符串
  @clean_test.assert_eq_string("Hello, !", @clean_test.greet(""))
  
  // 测试单个字符
  @clean_test.assert_eq_string("Hello, A!", @clean_test.greet("A"))
  
  // 测试包含空格的字符串
  @clean_test.assert_eq_string("Hello, John Doe!", @clean_test.greet("John Doe"))
  
  // 测试包含数字的字符串
  @clean_test.assert_eq_string("Hello, 2023!", @clean_test.greet("2023"))
}

// 测试数学运算的结合律
test "addition_associativity" {
  let a = 123
  let b = 456
  let c = 789
  
  // (a + b) + c = a + (b + c)
  let result1 = @clean_test.add(@clean_test.add(a, b), c)
  let result2 = @clean_test.add(a, @clean_test.add(b, c))
  
  @clean_test.assert_eq(result1, result2)
}

// 测试数学运算的交换律
test "addition_commutativity" {
  let a = 12345
  let b = 67890
  
  // a + b = b + a
  let result1 = @clean_test.add(a, b)
  let result2 = @clean_test.add(b, a)
  
  @clean_test.assert_eq(result1, result2)
}

// 测试数学运算的分配律
test "multiplication_distributivity" {
  let a = 12
  let b = 34
  let c = 56
  
  // a * (b + c) = a * b + a * c
  let left = @clean_test.multiply(a, @clean_test.add(b, c))
  let right = @clean_test.add(@clean_test.multiply(a, b), @clean_test.multiply(a, c))
  
  @clean_test.assert_eq(left, right)
}

// 测试实际应用场景：计算圆的面积和周长
test "circle_calculations" {
  // 简化的圆周率近似值
  let pi_approx = 3  // 使用整数近似
  
  let radius = 10
  
  // 计算周长：2 * π * r
  let circumference = @clean_test.multiply(@clean_test.multiply(2, pi_approx), radius)
  @clean_test.assert_eq(60, circumference)  // 2 * 3 * 10 = 60
  
  // 计算面积：π * r * r
  let area = @clean_test.multiply(pi_approx, @clean_test.multiply(radius, radius))
  @clean_test.assert_eq(300, area)  // 3 * 10 * 10 = 300
}

// 测试实际应用场景：计算平均数
test "average_calculation" {
  // 使用累加方式计算平均值
  let num1 = 10
  let num2 = 20
  let num3 = 30
  let num4 = 40
  let num5 = 50
  let count = 5
  
  // 计算总和
  let sum = @clean_test.add(@clean_test.add(@clean_test.add(@clean_test.add(num1, num2), num3), num4), num5)
  
  // 计算平均值（使用减法代替除法）
  // 30 * 5 = 150，所以如果 sum = 150，则平均值 = 30
  let expected_average_times_count = @clean_test.multiply(30, count)
  @clean_test.assert_eq(expected_average_times_count, sum)
}

// 测试实际应用场景：计算折扣价格
test "discount_calculation" {
  let original_price = 1000
  
  // 计算折扣金额（20% 折扣）
  // 20% = 20/100 = 1/5，所以 discount_amount = original_price / 5
  // 使用减法实现：original_price - original_price*4/5
  let price_times_4 = @clean_test.multiply(original_price, 4)
  let discount_amount = @clean_test.multiply(price_times_4, 1) / 5
  
  // 计算最终价格
  let final_price = @clean_test.add(original_price, -discount_amount)
  
  // 验证最终价格是 800
  @clean_test.assert_eq(800, final_price)
}

// 测试错误处理：除零保护
test "error_handling_scenarios" {
  // 测试大数相加的溢出保护
  let max_val = 2147483647
  
  // 这些调用应该返回边界值而不是溢出
  @clean_test.assert_eq(max_val, @clean_test.add(max_val, 1))
  @clean_test.assert_eq(max_val, @clean_test.add(max_val, max_val))
  
  // 测试大数相乘的溢出保护
  let large_num = 100000
  
  // 这些调用应该返回边界值而不是溢出
  @clean_test.assert_eq(max_val, @clean_test.multiply(large_num, large_num))
  @clean_test.assert_eq(max_val, @clean_test.multiply(max_val, 2))
}