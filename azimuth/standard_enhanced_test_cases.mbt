// 标准 MoonBit 增强测试用例
// 为 azimuth 库添加的高质量测试覆盖

test "arithmetic_sequence_operations" {
  // 测试算术序列操作
  let start = 5
  let difference = 3
  let terms = 4
  
  // 计算等差数列的和
  let term1 = start
  let term2 = add(start, difference)
  let term3 = add(term2, difference)
  let term4 = add(term3, difference)
  
  let sequence_sum = add(add(add(term1, term2), term3), term4)
  assert_eq(32, sequence_sum)  // 5 + 8 + 11 + 14 = 38
}

test "geometric_progression_calculation" {
  // 测试等比数列计算
  let first_term = 2
  let ratio = 3
  let terms = 4
  
  // 计算等比数列
  let term1 = first_term
  let term2 = multiply(first_term, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  
  let product_sum = add(add(add(term1, term2), term3), term4)
  assert_eq(80, product_sum)  // 2 + 6 + 18 + 54 = 80
}

test "string_concatenation_validation" {
  // 测试字符串连接验证
  let prefix = "Hello"
  let suffix = "World"
  let space = " "
  
  // 通过 greet 函数验证字符串处理
  let result1 = greet(prefix)
  let result2 = greet(suffix)
  
  assert_eq_string("Hello, Hello!", result1)
  assert_eq_string("Hello, World!", result2)
}

test "division_rounding_precision" {
  // 测试除法舍入精度
  let numerator = 100
  let divisors = [3, 7, 11, 13, 17]
  
  // 测试不同除数的向上取整
  assert_eq(34, divide_with_ceil(numerator, 3))    // ceil(100/3) = 34
  assert_eq(15, divide_with_ceil(numerator, 7))    // ceil(100/7) = 15
  assert_eq(10, divide_with_ceil(numerator, 11))   // ceil(100/11) = 10
  assert_eq(8, divide_with_ceil(numerator, 13))    // ceil(100/13) = 8
  assert_eq(6, divide_with_ceil(numerator, 17))    // ceil(100/17) = 6
}

test "complex_business_workflow" {
  // 测试复杂业务工作流
  // 场景：计算项目成本和资源分配
  let team_size = 12
  let daily_rate = 150
  let project_duration = 15
  let overhead_percentage = 20
  
  // 计算基础成本
  let base_labor_cost = multiply(team_size, multiply(daily_rate, project_duration))
  
  // 计算管理费用
  let overhead_cost = divide_with_ceil(multiply(base_labor_cost, overhead_percentage), 100)
  
  // 计算总成本
  let total_cost = add(base_labor_cost, overhead_cost)
  
  assert_eq(27000, base_labor_cost)   // 12 * 150 * 15 = 27000
  assert_eq(5400, overhead_cost)      // ceil(27000 * 20 / 100) = 5400
  assert_eq(32400, total_cost)        // 27000 + 5400 = 32400
}

test "mathematical_function_composition" {
  // 测试数学函数组合
  let input = 8
  
  // 复合函数运算
  let step1 = multiply(input, 3)        // 8 * 3 = 24
  let step2 = add(step1, 10)            // 24 + 10 = 34
  let step3 = divide_with_ceil(step2, 5) // ceil(34/5) = 7
  let step4 = multiply(step3, 2)        // 7 * 2 = 14
  let final_result = subtract(step4, input) // 14 - 8 = 6
  
  assert_eq(6, final_result)
}

test "edge_case_zero_handling" {
  // 测试零值处理的边缘情况
  // 加法零值
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  assert_eq(0, add(0, 0))
  
  // 乘法零值
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(0, 42))
  assert_eq(0, multiply(0, 0))
  
  // 除法零值处理
  assert_eq(0, divide_with_ceil(0, 42))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(42, 0))
}

test "negative_number_arithmetic" {
  // 测试负数算术运算
  let positive = 15
  let negative = -8
  
  // 加法运算
  assert_eq(7, add(positive, negative))    // 15 + (-8) = 7
  assert_eq(-7, add(negative, positive))   // -8 + 15 = 7
  assert_eq(-16, add(negative, negative))  // -8 + (-8) = -16
  
  // 乘法运算
  assert_eq(-120, multiply(positive, negative))   // 15 * (-8) = -120
  assert_eq(64, multiply(negative, negative))     // (-8) * (-8) = 64
  
  // 除法运算
  assert_eq(-1, divide_with_ceil(negative, positive))   // ceil(-8/15) = -1
  assert_eq(2, divide_with_ceil(positive, negative))    // ceil(15/-8) = -1 (但实现可能返回0)
}

test "resource_optimization_problem" {
  // 测试资源优化问题
  // 场景：仓库存储优化
  let total_items = 157
  let box_types = 3
  
  // 不同类型的箱子容量
  let small_box_capacity = 10
  let medium_box_capacity = 25
  let large_box_capacity = 50
  
  // 计算每种箱子需要的数量
  let small_boxes = divide_with_ceil(50, small_box_capacity)     // ceil(50/10) = 5
  let medium_boxes = divide_with_ceil(60, medium_box_capacity)   // ceil(60/25) = 3
  let large_boxes = divide_with_ceil(47, large_box_capacity)     // ceil(47/50) = 1
  
  let total_boxes = add(add(small_boxes, medium_boxes), large_boxes)
  
  assert_eq(5, small_boxes)
  assert_eq(3, medium_boxes)
  assert_eq(1, large_boxes)
  assert_eq(9, total_boxes)
  
  // 验证总容量
  let total_capacity = add(
    multiply(small_boxes, small_box_capacity),
    add(multiply(medium_boxes, medium_box_capacity), multiply(large_boxes, large_box_capacity))
  )
  assert_eq(175, total_capacity)
}

test "performance_calculation_scenario" {
  // 测试性能计算场景
  // 场景：计算系统性能指标
  let operations = 10000
  let time_seconds = 60
  let parallel_threads = 4
  
  // 计算每秒操作数
  let ops_per_second = divide_with_ceil(operations, time_seconds)
  assert_eq(167, ops_per_second)  // ceil(10000/60) = 167
  
  // 计算每线程操作数
  let ops_per_thread = divide_with_ceil(operations, parallel_threads)
  assert_eq(2500, ops_per_thread)  // ceil(10000/4) = 2500
  
  // 计算理论吞吐量
  let theoretical_throughput = multiply(ops_per_second, parallel_threads)
  assert_eq(668, theoretical_throughput)  // 167 * 4 = 668
  
  // 计算效率
  let efficiency = divide_with_ceil(multiply(ops_per_thread, 100), multiply(ops_per_second, time_seconds))
  assert_eq(25, efficiency)  // ceil(2500*100 / 167*60) ≈ 25%
}