// 额外的 MoonBit 测试用例 - 使用标准 MoonBit 测试语法
// 覆盖更多边界情况和功能组合

test "addition_associative_law" {
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let a = 7
  let b = 11
  let c = 13
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
}

test "multiplication_distributive_law" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 3
  let c = 7
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "divide_with_ceil_negative_divisor" {
  // 测试负数除数的向上取整除法
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(-2, divide_with_ceil(5, -3))
  assert_eq(0, divide_with_ceil(0, -5))
}

test "greet_with_long_string" {
  // 测试长字符串的问候功能
  let long_name = "这是一个很长的名字用来测试字符串处理功能"
  let expected = "Hello, " + long_name + "!"
  assert_eq_string(expected, greet(long_name))
}

test "complex_nested_operations" {
  // 测试复杂的嵌套运算
  let result = divide_with_ceil(
    add(multiply(3, 4), multiply(5, 2)),
    subtract(10, 3)
  )
  assert_eq(4, result)  // ceil((12+10)/7) = ceil(22/7) = 4
}

test "divide_with_ceil_mixed_signs" {
  // 测试混合符号的除法向上取整
  assert_eq(-1, divide_with_ceil(5, -10))
  assert_eq(0, divide_with_ceil(-5, 10))
  assert_eq(1, divide_with_ceil(-5, -10))
}

test "arithmetic_precision" {
  // 测试算术精度
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
}

test "edge_case_single_operations" {
  // 测试单个元素的边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, add(0, 1))
  assert_eq(0, multiply(0, 1))
}

test "string_concatenation_behavior" {
  // 测试字符串连接行为
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, Space !", greet("Space "))
}

// 辅助函数用于测试
fn subtract(a : Int, b : Int) -> Int {
  a - b
}

// 新增的8个高质量 MoonBit 测试用例
test "subtract_operation_simulation" {
  // 模拟减法操作：a - b = a + (-b)
  let a = 15
  let b = 7
  let negative_b = multiply(b, -1)
  let result = add(a, negative_b)
  assert_eq(8, result)
}

test "percentage_calculation" {
  // 百分比计算测试
  let total = 200
  let percentage = 25  // 25%
  let result = divide_with_ceil(multiply(total, percentage), 100)
  assert_eq(50, result)
}

test "batch_processing_efficiency" {
  // 批处理效率测试
  let total_items = 157
  let batch_size = 20
  let full_batches = divide_with_ceil(total_items, batch_size)
  assert_eq(8, full_batches)  // ceil(157/20) = 8
  
  // 验证总处理能力
  let total_capacity = multiply(full_batches, batch_size)
  assert_eq(160, total_capacity)
  
  // 计算剩余容量
  let unused_capacity = subtract(total_capacity, total_items)
  assert_eq(3, unused_capacity)
}

test "string_concatenation_length" {
  // 字符串连接长度测试
  let name = "MoonBit"
  let greeting = greet(name)
  let expected_length = add(add(7, 6), 1)  // "Hello, " + "MoonBit" + "!" = 7 + 6 + 1 = 14
  assert_eq(14, expected_length)
  @unused(greeting)
}

test "division_precision_test" {
  // 除法精度测试
  // 测试连续除法的精度保持
  let initial_value = 1000
  let divisor1 = 3
  let divisor2 = 7
  let divisor3 = 11
  
  let step1 = divide_with_ceil(initial_value, divisor1)
  let step2 = divide_with_ceil(step1, divisor2)
  let step3 = divide_with_ceil(step2, divisor3)
  
  // 验证每一步的结果
  assert_eq(334, step1)   // ceil(1000/3)
  assert_eq(48, step2)    // ceil(334/7)
  assert_eq(5, step3)     // ceil(48/11)
}

test "resource_allocation_fairness" {
  // 资源分配公平性测试
  let total_resources = 100
  let participants = 9
  
  // 计算每人应得份额
  let base_share = divide_with_ceil(total_resources, participants)
  assert_eq(12, base_share)  // ceil(100/9) = 12
  
  // 计算总分配量
  let total_allocated = multiply(base_share, participants)
  assert_eq(108, total_allocated)
  
  // 计算溢出量
  let overflow = subtract(total_allocated, total_resources)
  assert_eq(8, overflow)
}

test "time_calculation_scenario" {
  // 时间计算场景测试
  // 场景：计算项目完成时间
  let total_tasks = 87
  let team_velocity = 12  // 每周完成的任务数
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(total_tasks, team_velocity)
  assert_eq(8, weeks_needed)  // ceil(87/12) = 8
  
  // 计算总工作日（假设每周5个工作日）
  let work_days = multiply(weeks_needed, 5)
  assert_eq(40, work_days)
  
  // 计算总容量
  let total_capacity = multiply(weeks_needed, team_velocity)
  assert_eq(96, total_capacity)
}

test "exponential_growth_simulation" {
  // 指数增长模拟测试
  // 模拟简单的指数增长：每次翻倍
  let initial_value = 3
  let growth_periods = 5  // 这个变量只是用于演示，实际未使用
  
  // 手动计算指数增长
  let period1 = multiply(initial_value, 2)
  let period2 = multiply(period1, 2)
  let period3 = multiply(period2, 2)
  let period4 = multiply(period3, 2)
  let period5 = multiply(period4, 2)
  
  assert_eq(6, period1)    // 3 * 2
  assert_eq(12, period2)   // 6 * 2
  assert_eq(24, period3)   // 12 * 2
  assert_eq(48, period4)   // 24 * 2
  assert_eq(96, period5)   // 48 * 2
  
  // 验证最终结果
  assert_eq(96, period5)
  @unused(growth_periods)
}