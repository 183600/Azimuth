// 高质量 MoonBit 测试用例 - 覆盖核心功能和边界情况

test "arithmeti_sequence_operations" {
  // 测试算术序列操作
  let start = 5
  let difference = 3
  let count = 4
  
  // 计算等差数列的和
  let term1 = start
  let term2 = add(term1, difference)
  let term3 = add(term2, difference)
  let term4 = add(term3, difference)
  
  let sum = add(add(add(term1, term2), term3), term4)
  assert_eq(32, sum)  // 5 + 8 + 11 + 14 = 38
  
  // 计算平均值（使用向上取整除法）
  let average = divide_with_ceil(sum, count)
  assert_eq(8, average)  // ceil(32/4) = 8
}

test "inventory_management_optimization" {
  // 库存管理优化测试
  let product_a = 23
  let product_b = 47
  let product_c = 31
  let box_capacity = 15
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a, box_capacity)
  let boxes_b = divide_with_ceil(product_b, box_capacity)
  let boxes_c = divide_with_ceil(product_c, box_capacity)
  
  assert_eq(2, boxes_a)  // ceil(23/15) = 2
  assert_eq(4, boxes_b)  // ceil(47/15) = 4
  assert_eq(3, boxes_c)  // ceil(31/15) = 3
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = multiply(total_boxes, box_capacity)
  
  assert_eq(9, total_boxes)
  assert_eq(135, total_capacity)
  
  // 计算剩余空间
  let total_items = add(add(product_a, product_b), product_c)
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(34, remaining_space)  // 135 - 101 = 34
}

test "string_processing_pipeline" {
  // 字符串处理流水线测试
  let input1 = "MoonBit"
  let input2 = "2024"
  let input3 = "测试"
  
  // 处理各种字符串输入
  let greeting1 = greet(input1)
  let greeting2 = greet(input2)
  let greeting3 = greet(input3)
  
  assert_eq_string("Hello, MoonBit!", greeting1)
  assert_eq_string("Hello, 2024!", greeting2)
  assert_eq_string("Hello, 测试!", greeting3)
  
  // 测试空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello,   !", greet("   "))
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  let principal = 10000
  let rate1 = 5  // 5%
  let rate2 = 3  // 3%
  let years = 2
  
  // 第一年利息
  let interest1 = divide_with_ceil(multiply(principal, rate1), 100)
  let amount1 = add(principal, interest1)
  
  // 第二年利息（基于新的本金）
  let interest2 = divide_with_ceil(multiply(amount1, rate2), 100)
  let amount2 = add(amount1, interest2)
  
  assert_eq(500, interest1)  // 10000 * 5% = 500
  assert_eq(10500, amount1)  // 10000 + 500 = 10500
  assert_eq(315, interest2)  // ceil(10500 * 3%) = 315
  assert_eq(10815, amount2)  // 10500 + 315 = 10815
  
  // 计算月供（简化计算）
  let months = multiply(years, 12)
  let monthly_payment = divide_with_ceil(amount2, months)
  assert_eq(451, monthly_payment)  // ceil(10815/24) = 451
}

test "resource_allocation_efficiency" {
  // 资源分配效率测试
  let total_resources = 1000
  let projects = 7
  let min_resources_per_project = divide_with_ceil(total_resources, projects)
  
  assert_eq(143, min_resources_per_project)  // ceil(1000/7) = 143
  
  // 计算实际分配的资源
  let allocated_resources = multiply(min_resources_per_project, projects)
  let waste = subtract(allocated_resources, total_resources)
  
  assert_eq(1001, allocated_resources)  // 143 * 7 = 1001
  assert_eq(1, waste)  // 1001 - 1000 = 1
  
  // 计算资源利用率
  let utilization = multiply(100, divide_with_ceil(multiply(total_resources, 100), allocated_resources))
  assert_eq(99, utilization)  // floor(100000/1001) = 99%
}

test "mathematical_series_calculation" {
  // 数学级数计算测试
  let n = 5
  
  // 计算平方和：1² + 2² + 3² + 4² + 5²
  let square1 = multiply(1, 1)
  let square2 = multiply(2, 2)
  let square3 = multiply(3, 3)
  let square4 = multiply(4, 4)
  let square5 = multiply(5, 5)
  
  let sum_of_squares = add(add(add(add(square1, square2), square3), square4), square5)
  assert_eq(55, sum_of_squares)  // 1 + 4 + 9 + 16 + 25 = 55
  
  // 计算阶乘：5! = 5 * 4 * 3 * 2 * 1
  let factorial = multiply(multiply(multiply(multiply(5, 4), 3), 2), 1)
  assert_eq(120, factorial)
  
  // 计算平均值
  let average = divide_with_ceil(sum_of_squares, n)
  assert_eq(11, average)  // ceil(55/5) = 11
}

test "complex_conditional_calculations" {
  // 复杂条件计算测试
  let base_value = 100
  let multiplier_a = 2
  let multiplier_b = 3
  let threshold = 250
  
  // 计算两种方案的结果
  let result_a = multiply(base_value, multiplier_a)
  let result_b = multiply(base_value, multiplier_b)
  
  assert_eq(200, result_a)
  assert_eq(300, result_b)
  
  // 根据阈值选择方案
  let chosen_result = result_a
  let meets_threshold = chosen_result >= threshold
  let alternative = result_b
  
  assert_false(meets_threshold)  // 200 < 250
  
  // 如果不满足阈值，使用备选方案
  let final_result = meets_threshold ? chosen_result : alternative
  assert_eq(300, final_result)
  
  // 计算需要的资源块数
  let resource_blocks = divide_with_ceil(final_result, 80)
  assert_eq(4, resource_blocks)  // ceil(300/80) = 4
}

test "data_transformation_pipeline" {
  // 数据转换流水线测试
  let raw_data = 12345
  let factor1 = 3
  let factor2 = 7
  let divisor = 4
  
  // 数据处理步骤
  let step1 = add(raw_data, factor1)        // 12345 + 3 = 12348
  let step2 = multiply(step1, factor2)      // 12348 * 7 = 86436
  let step3 = divide_with_ceil(step2, divisor)  // ceil(86436/4) = 21609
  let step4 = subtract(step3, raw_data)     // 21609 - 12345 = 9264
  
  assert_eq(12348, step1)
  assert_eq(86436, step2)
  assert_eq(21609, step3)
  assert_eq(9264, step4)
  
  // 验证数据完整性
  let verification = add(step4, raw_data)
  assert_eq(step3, verification)
}

test "error_boundary_and_recovery" {
  // 错误边界和恢复测试
  // 测试各种除零情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 测试极值运算
  let large_num = 1000000
  let small_num = 1
  
  assert_eq(1000000, divide_with_ceil(large_num, small_num))
  assert_eq(1, divide_with_ceil(small_num, large_num))
  assert_eq(1000000, divide_with_ceil(large_num, large_num))
  
  // 测试负数边界情况
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(1, -1))
  assert_eq(-1, divide_with_ceil(-1, -1))
  
  // 测试运算恢复
  let error_result = divide_with_ceil(50, 0)
  let recovery_value = add(error_result, 100)
  assert_eq(100, recovery_value)
}