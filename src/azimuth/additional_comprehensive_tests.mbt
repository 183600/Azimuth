// 额外的综合测试用例 - 补充现有测试覆盖

// 测试用例1: 复杂数学表达式组合测试
///|
test "complex_mathematical_expressions" {
  // 测试 (a + b) * c 的组合运算
  let a = 10
  let b = 20
  let c = 3
  let result = multiply(add(a, b), c)  // (10 + 20) * 3 = 30 * 3 = 90
  assert_eq(90, result)
  
  // 测试 a * b + c * d 的组合运算
  let a2 = 5
  let b2 = 4
  let c2 = 3
  let d2 = 2
  let result2 = add(multiply(a2, b2), multiply(c2, d2))  // 5*4 + 3*2 = 20 + 6 = 26
  assert_eq(26, result2)
  
  // 测试嵌套运算: (a + (b * c)) * d
  let a3 = 2
  let b3 = 3
  let c3 = 4
  let d3 = 5
  let result3 = multiply(add(a3, multiply(b3, c3)), d3)  // (2 + (3*4)) * 5 = (2 + 12) * 5 = 14 * 5 = 70
  assert_eq(70, result3)
}

// 测试用例2: 极端边界值组合测试
///|
test "extreme_boundary_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与接近最大值的运算
  assert_eq(max_val, add(max_val - 1, 2))  // 2147483646 + 2 = 2147483647 (边界)
  assert_eq(max_val, add(max_val - 100, 101))  // 2147483547 + 101 = 2147483647 (边界)
  
  // 测试最小值与接近最小值的运算
  assert_eq(min_val, add(min_val + 1, -2))  // -2147483647 + (-2) = -2147483648 (边界)
  assert_eq(min_val, add(min_val + 100, -101))  // -2147483548 + (-101) = -2147483648 (边界)
  
  // 测试最大值与最小值的乘法
  assert_eq(-2147483647, multiply(max_val, -1))  // 2147483647 * (-1) = -2147483647
  assert_eq(min_val, multiply(min_val, 1))   // -2147483648 * 1 = -2147483648
}

// 测试用例3: 字符串处理的高级测试
///|
test "advanced_string_processing" {
  // 测试包含空格的字符串
  assert_eq("Hello, Moon Bit!", greet("Moon Bit"))
  assert_eq("Hello,  !", greet(" "))
  
  // 测试包含特殊字符的字符串
  assert_eq("Hello, 中文!", greet("中文"))
  assert_eq("Hello, 🚀!", greet("🚀"))
  assert_eq("Hello, \n\t!", greet("\n\t"))
  
  // 测试非常长的字符串
  let long_name = "ThisIsAVeryLongNameThatMightCauseIssuesInSomeSystemsButShouldWorkFineInMoonBit"
  let expected = "Hello, " + long_name + "!"
  assert_eq(expected, greet(long_name))
}

// 测试用例4: 连续运算的一致性测试
///|
test "sequential_operations_consistency" {
  // 测试连续加法的一致性 - 使用固定数组
  let sum1 = add(add(add(add(add(0, 1), 2), 3), 4), 5)  // (((((0+1)+2)+3)+4)+5) = 15
  let sum2 = add(add(add(add(add(0, 5), 4), 3), 2), 1)  // (((((0+5)+4)+3)+2)+1) = 15
  
  assert_eq(sum1, sum2)  // 正向和反向求和应该相等
  
  // 测试连续乘法的一致性
  let product1 = multiply(multiply(multiply(1, 2), 3), 4)  // (((1*2)*3)*4) = 24
  let product2 = multiply(multiply(multiply(1, 4), 3), 2)  // (((1*4)*3)*2) = 24
  
  assert_eq(product1, product2)  // 正向和反向求积应该相等
}

// 测试用例5: 实际商业场景测试
///|
test "real_world_business_scenarios" {
  // 场景1: 计算折扣价格
  let original_price = 1000
  let discount_percentage = 20  // 20%折扣
  let discount_amount = multiply(original_price, discount_percentage) / 100
  let final_price = add(original_price, -discount_amount)
  assert_eq(800, final_price)  // 1000 - (1000 * 20 / 100) = 1000 - 200 = 800
  
  // 场景2: 计算复利 (简化版)
  let principal = 1000
  let rate = 5  // 5%利率
  let year1_interest = multiply(principal, rate) / 100
  let year1_amount = add(principal, year1_interest)  // 1000 + 50 = 1050
  let year2_interest = multiply(year1_amount, rate) / 100
  let year2_amount = add(year1_amount, year2_interest)  // 1050 + 52 = 1102
  let year3_interest = multiply(year2_amount, rate) / 100
  let year3_amount = add(year2_amount, year3_interest)  // 1102 + 55 = 1157
  assert_eq(1157, year3_amount)  // 简化计算，忽略小数部分
  
  // 场景3: 库存管理
  let initial_stock = 100
  let sold = 30
  let restocked = 50
  let current_stock = add(add(initial_stock, -sold), restocked)
  assert_eq(120, current_stock)  // 100 - 30 + 50 = 120
}

// 测试用例6: 数值精度和舍入测试
///|
test "numerical_precision_and_rounding" {
  // 测试整数除法的精度损失处理
  let numerator = 100
  let denominator = 3
  let quotient = numerator / denominator
  let remainder = numerator % denominator
  let reconstructed = add(multiply(quotient, denominator), remainder)
  assert_eq(numerator, reconstructed)  // 100 = (100/3)*3 + (100%3) = 33*3 + 1 = 99 + 1 = 100
  
  // 测试负数除法的精度
  let neg_numerator = -100
  let neg_quotient = neg_numerator / denominator
  let neg_remainder = neg_numerator % denominator
  let neg_reconstructed = add(multiply(neg_quotient, denominator), neg_remainder)
  assert_eq(neg_numerator, neg_reconstructed)
}

// 测试用例7: 错误输入的鲁棒性测试
///|
test "error_input_robustness" {
  // 测试重复边界值运算
  let max_val = 2147483647
  
  // 多次达到边界
  let result1 = add(add(add(add(add(max_val, 0), 0), 0), 0), 0)  // 应该保持不变
  assert_eq(max_val, result1)
  
  // 交替正负运算
  let alternate1 = add(0, 100)   // 0 + 100 = 100
  let alternate2 = add(alternate1, -50)   // 100 - 50 = 50
  let alternate3 = add(alternate2, -50)   // 50 - 50 = 0
  let alternate4 = add(alternate3, 100)   // 0 + 100 = 100
  assert_eq(100, alternate4)
}

// 测试用例8: 性能相关的批量操作测试
///|
test "performance_related_batch_operations" {
  // 测试大量相同操作的累积效果
  let base_value = 1
  
  // 批量加法
  let sum_result = add(add(add(add(add(add(add(add(add(add(0, base_value), base_value), base_value), base_value), base_value), base_value), base_value), base_value), base_value), base_value)
  assert_eq(10, sum_result)  // 1 + 1 + ... + 1 (10次) = 10
  
  // 批量乘法（几何级数）
  let product_result = multiply(multiply(multiply(multiply(multiply(1, 2), 2), 2), 2), 2)
  assert_eq(32, product_result)  // 2^5 = 32
}

// 测试用例9: 特殊数学性质验证
///|
test "special_mathematical_properties" {
  // 测试乘法对加法的分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  let left_side = multiply(a, add(b, c))     // 5 * (10 + 15) = 5 * 25 = 125
  let right_side = add(multiply(a, b), multiply(a, c))  // 5*10 + 5*15 = 50 + 75 = 125
  assert_eq(left_side, right_side)
  
  // 测试零元素的性质
  assert_eq(0, multiply(0, 12345))    // 0 * 任何数 = 0
  assert_eq(12345, add(12345, 0))     // 任何数 + 0 = 任何数
  assert_eq(0, add(0, 0))             // 0 + 0 = 0
  
  // 测试单位元素的性质
  assert_eq(12345, multiply(12345, 1))  // 任何数 * 1 = 任何数
  assert_eq(12345, multiply(1, 12345))  // 1 * 任何数 = 任何数
}

// 测试用例10: 字符串边界和内存安全测试
///|
test "string_boundary_and_memory_safety" {
  // 测试各种Unicode字符
  assert_eq("Hello, 世界!", greet("世界"))
  assert_eq("Hello, 🌙!", greet("🌙"))
  assert_eq("Hello, ñáéíóú!", greet("ñáéíóú"))
  
  // 测试包含引号和特殊字符的字符串
  assert_eq("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq("Hello, 'single'!", greet("'single'"))
  assert_eq("Hello, &amp;!", greet("&amp;"))
  
  // 测试数字和字母混合
  assert_eq("Hello, test123!", greet("test123"))
  assert_eq("Hello, 123test!", greet("123test"))
  assert_eq("Hello, t1e2s3t!", greet("t1e2s3t"))
}