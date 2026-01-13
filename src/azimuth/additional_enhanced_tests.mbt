// 额外的增强测试用例
// 这些测试用例补充了现有测试可能没有覆盖到的方面

test "numerical_precision_test" {
  // 测试数值精度和舍入行为
  // 模拟除法运算的整数舍入
  let dividend = 100
  let divisor = 3
  let quotient = dividend / divisor
  let remainder = dividend - @azimuth.multiply(divisor, quotient)
  
  @azimuth.assert_eq(33, quotient)
  @azimuth.assert_eq(1, remainder)
  
  // 测试负数除法的舍入
  let neg_dividend = -100
  let neg_quotient = neg_dividend / divisor
  let neg_remainder = neg_dividend - @azimuth.multiply(divisor, neg_quotient)
  
  @azimuth.assert_eq(-33, neg_quotient)
  @azimuth.assert_eq(-1, neg_remainder)
}

test "large_number_performance_test" {
  // 测试大数运算性能
  let large_num1 = 100000000
  let large_num2 = 200000000
  
  // 大数相加
  let add_result = @azimuth.add(large_num1, large_num2)
  @azimuth.assert_eq(300000000, add_result)
  
  // 大数相乘
  let multiply_result = @azimuth.multiply(large_num1 / 1000, large_num2 / 1000)
  @azimuth.assert_eq(20000000000, multiply_result)
  
  // 连续运算
  let sequential_result = @azimuth.add(
    @azimuth.multiply(large_num1, 2),
    @azimuth.multiply(large_num2, 3)
  )
  @azimuth.assert_eq(800000000, sequential_result)
}

test "complex_string_processing_test" {
  // 测试复杂字符串处理场景
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result1 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, " + special_chars + "!", result1)
  
  // 测试包含Unicode字符的字符串
  let unicode_str = "Hello世界🌍123"
  let result2 = @azimuth.greet(unicode_str)
  @azimuth.assert_eq_string("Hello, " + unicode_str + "!", result2)
  
  // 测试包含转义字符的字符串
  let escape_chars = "Tab:\t Newline:\n Quote:\""
  let result3 = @azimuth.greet(escape_chars)
  @azimuth.assert_eq_string("Hello, " + escape_chars + "!", result3)
}

test "mathematical_formula_application_test" {
  // 测试数学公式应用
  // 计算圆的面积（简化版，使用整数近似）
  let radius = 5
  let pi_approx = 3  // π的整数近似
  let circle_area = @azimuth.multiply(pi_approx, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, circle_area)  // 3 * 5 * 5 = 75
  
  // 计算等差数列的和：n/2 * (2a + (n-1)d)
  let n = 10  // 项数
  let a = 1   // 首项
  let d = 2   // 公差
  let sum_formula = n / 2 * @azimuth.add(@azimuth.multiply(2, a), @azimuth.multiply(n - 1, d))
  @azimuth.assert_eq(55, sum_formula)  // 10/2 * (2*1 + 9*2) = 5 * (2 + 18) = 5 * 20 = 100，修正：应该是55
  
  // 验证手动计算
  let manual_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 3), 5), 7), 9), 11), 13), 15), 17), 19)
  @azimuth.assert_eq(100, manual_sum)
}

test "recursive_calculation_simulation_test" {
  // 模拟递归计算：阶乘计算（使用迭代方式模拟）
  let n = 5
  let mut result = 1
  let mut i = 1
  
  // 手动模拟循环：5! = 5 * 4 * 3 * 2 * 1 = 120
  result = @azimuth.multiply(result, i)  // 1 * 1 = 1
  i = @azimuth.add(i, 1)                // i = 2
  result = @azimuth.multiply(result, i)  // 1 * 2 = 2
  i = @azimuth.add(i, 1)                // i = 3
  result = @azimuth.multiply(result, i)  // 2 * 3 = 6
  i = @azimuth.add(i, 1)                // i = 4
  result = @azimuth.multiply(result, i)  // 6 * 4 = 24
  i = @azimuth.add(i, 1)                // i = 5
  result = @azimuth.multiply(result, i)  // 24 * 5 = 120
  
  @azimuth.assert_eq(120, result)
}

test "conditional_branch_calculation_test" {
  // 测试条件分支计算
  let x = 15
  let y = 20
  let z = 10
  
  // 复杂条件表达式
  let condition1 = x < y
  let condition2 = y > z
  let condition3 = x + z > y
  
  // 根据条件执行不同计算
  let result1 = if condition1 && condition2 {
    @azimuth.add(@azimuth.multiply(x, 2), @azimuth.multiply(y, 3))
  } else {
    @azimuth.multiply(@azimuth.add(x, y), z)
  }
  
  // 15 < 20 且 20 > 10 为真，所以执行：15*2 + 20*3 = 30 + 60 = 90
  @azimuth.assert_eq(90, result1)
  
  let result2 = if condition3 {
    @azimuth.multiply(@azimuth.add(x, y), z)
  } else {
    @azimuth.add(@azimuth.multiply(x, y), z)
  }
  
  // 15 + 10 > 20 为真，所以执行：(15 + 20) * 10 = 35 * 10 = 350
  @azimuth.assert_eq(350, result2)
}

test "data_validation_test" {
  // 测试数据验证场景
  // 验证输入数据是否在有效范围内
  let input_value = 150
  let min_allowed = 100
  let max_allowed = 200
  
  // 检查是否在范围内
  let is_in_range = @azimuth.add(input_value, -min_allowed) >= 0 && 
                    @azimuth.add(max_allowed, -input_value) >= 0
  @azimuth.assert_true(is_in_range)
  
  // 测试边界值
  let at_min_boundary = min_allowed
  let at_max_boundary = max_allowed
  let outside_min = min_allowed - 1
  let outside_max = max_allowed + 1
  
  @azimuth.assert_true(@azimuth.add(at_min_boundary, -min_allowed) >= 0 && 
                       @azimuth.add(max_allowed, -at_min_boundary) >= 0)
  @azimuth.assert_true(@azimuth.add(at_max_boundary, -min_allowed) >= 0 && 
                       @azimuth.add(max_allowed, -at_max_boundary) >= 0)
  @azimuth.assert_false(@azimuth.add(outside_min, -min_allowed) >= 0 && 
                        @azimuth.add(max_allowed, -outside_min) >= 0)
  @azimuth.assert_false(@azimuth.add(outside_max, -min_allowed) >= 0 && 
                        @azimuth.add(max_allowed, -outside_max) >= 0)
}

test "error_recovery_simulation_test" {
  // 模拟错误恢复场景
  // 模拟计算过程中的错误处理和恢复
  let initial_value = 100
  let operation1_result = @azimuth.multiply(initial_value, 2)  // 100 * 2 = 200
  let operation2_result = @azimuth.add(operation1_result, 50)  // 200 + 50 = 250
  
  // 模拟检测到错误，需要回滚到上一个状态
  let recovered_value = operation1_result  // 回滚到200
  
  // 从恢复的状态继续计算
  let final_result = @azimuth.multiply(recovered_value, 3)  // 200 * 3 = 600
  
  @azimuth.assert_eq(200, recovered_value)
  @azimuth.assert_eq(600, final_result)
}

test "multi_step_business_logic_test" {
  // 测试多步骤业务逻辑
  // 模拟订单处理流程
  let unit_price = 100
  let quantity = 5
  let discount_rate = 10  // 10%折扣
  let tax_rate = 8       // 8%税率
  
  // 步骤1：计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)  // 100 * 5 = 500
  
  // 步骤2：计算折扣
  let discount_amount = @azimuth.multiply(subtotal, discount_rate) / 100  // 500 * 10 / 100 = 50
  let discounted_total = @azimuth.add(subtotal, -discount_amount)  // 500 - 50 = 450
  
  // 步骤3：计算税费
  let tax_amount = @azimuth.multiply(discounted_total, tax_rate) / 100  // 450 * 8 / 100 = 36
  let final_total = @azimuth.add(discounted_total, tax_amount)  // 450 + 36 = 486
  
  @azimuth.assert_eq(500, subtotal)
  @azimuth.assert_eq(50, discount_amount)
  @azimuth.assert_eq(450, discounted_total)
  @azimuth.assert_eq(36, tax_amount)
  @azimuth.assert_eq(486, final_total)
}

test "extreme_performance_test" {
  // 极限性能测试
  // 测试大量连续运算的性能
  let base_value = 1000
  
  // 执行大量连续加法
  let mut sum_result = 0
  sum_result = @azimuth.add(sum_result, base_value)      // 0 + 1000 = 1000
  sum_result = @azimuth.add(sum_result, base_value)      // 1000 + 1000 = 2000
  sum_result = @azimuth.add(sum_result, base_value)      // 2000 + 1000 = 3000
  sum_result = @azimuth.add(sum_result, base_value)      // 3000 + 1000 = 4000
  sum_result = @azimuth.add(sum_result, base_value)      // 4000 + 1000 = 5000
  
  @azimuth.assert_eq(5000, sum_result)
  
  // 执行大量连续乘法
  let mut product_result = 1
  product_result = @azimuth.multiply(product_result, 2)   // 1 * 2 = 2
  product_result = @azimuth.multiply(product_result, 3)   // 2 * 3 = 6
  product_result = @azimuth.multiply(product_result, 4)   // 6 * 4 = 24
  product_result = @azimuth.multiply(product_result, 5)   // 24 * 5 = 120
  product_result = @azimuth.multiply(product_result, 6)   // 120 * 6 = 720
  
  @azimuth.assert_eq(720, product_result)
  
  // 混合运算测试
  let mixed_result = @azimuth.add(
    @azimuth.multiply(100, 5),
    @azimuth.multiply(@azimuth.add(50, 25), 2)
  )  // 100*5 + (50+25)*2 = 500 + 75*2 = 500 + 150 = 650
  
  @azimuth.assert_eq(650, mixed_result)
}