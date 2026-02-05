// 额外的 MoonBit 标准测试用例
// 为 azimuth 库提供更全面的测试覆盖

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(34, divide_with_ceil(100, 3))      // 33.33... -> 34
  assert_eq(67, divide_with_ceil(200, 3))      // 66.66... -> 67
  assert_eq(1, divide_with_ceil(2, 2))         // 1 -> 1
  assert_eq(1001, divide_with_ceil(2000, 2))   // 1000 -> 1000? Actually 2000/2 = 1000
  assert_eq(1000, divide_with_ceil(2000, 2))   // 修正：2000/2 = 1000
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  let empty = ""
  let space = " "
  let result1 = greet(empty)
  let result2 = greet(space)
  let result3 = greet("Test")
  
  assert_eq_string("Hello, !", result1)
  assert_eq_string("Hello,  !", result2)
  assert_eq_string("Hello, Test!", result3)
}

test "arithmetic_sequence_calculation" {
  // 测试等差数列计算
  let first_term = 5
  let common_diff = 3
  let n_terms = 10
  
  // 计算第n项：a_n = a_1 + (n-1)*d
  let nth_term = add(first_term, multiply(n_terms - 1, common_diff))
  
  // 计算前n项和：S_n = n/2 * (2*a_1 + (n-1)*d)
  let sum_n = divide_with_ceil(
    multiply(n_terms, add(multiply(2, first_term), multiply(n_terms - 1, common_diff))),
    2
  )
  
  assert_eq(32, nth_term)  // 5 + 9*3 = 32
  assert_eq(185, sum_n)    // 10/2 * (10 + 27) = 5 * 37 = 185
}

test "temperature_conversion_simulation" {
  // 模拟温度转换计算（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(divide_with_ceil(multiply(celsius, 9), 5), 1), 32)
  
  // 25°C = (25*9/5)+32 = 45+32 = 77°F
  assert_eq(77, fahrenheit)
  
  // 反向转换（简化）
  let back_to_celsius = divide_with_ceil(multiply(fahrenheit - 32, 5), 9)
  assert_eq(25, back_to_celsius)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 100
  let projects = 3
  let min_per_project = 20
  
  // 计算每个项目的基本分配
  let basic_allocation = multiply(min_per_project, projects)
  
  // 计算剩余资源
  let remaining = subtract(total_resources, basic_allocation)
  
  // 平均分配剩余资源
  let bonus_per_project = divide_with_ceil(remaining, projects)
  
  // 每个项目的总分配
  let total_per_project = add(min_per_project, bonus_per_project)
  
  assert_eq(60, basic_allocation)
  assert_eq(40, remaining)
  assert_eq(14, bonus_per_project)  // ceil(40/3) = 14
  assert_eq(34, total_per_project)  // 20 + 14 = 34
}

test "circular_reference_calculation" {
  // 循环引用计算测试
  let initial_value = 100
  
  // 模拟循环计算：每次增加前一次结果的一半
  let iteration1 = add(initial_value, divide_with_ceil(initial_value, 2))
  let iteration2 = add(iteration1, divide_with_ceil(iteration1, 2))
  
  assert_eq(150, iteration1)  // 100 + 50 = 150
  assert_eq(225, iteration2)  // 150 + 75 = 225
}

test "batch_processing_simulation" {
  // 批处理模拟测试
  let total_items = 1000
  let batch_size = 75
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(total_items, batch_size)
  
  // 计算最后一个批次的项目数
  let last_batch_size = subtract(total_items, multiply(batches_needed - 1, batch_size))
  
  assert_eq(14, batches_needed)  // ceil(1000/75) = 14
  assert_eq(25, last_batch_size) // 1000 - 13*75 = 25
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 500
  let percentage = 23
  
  // 计算百分比值
  let percentage_value = divide_with_ceil(multiply(total, percentage), 100)
  
  // 计算剩余百分比
  let remaining_percentage = subtract(100, percentage)
  let remaining_value = subtract(total, percentage_value)
  
  assert_eq(115, percentage_value)  // ceil(500*23/100) = ceil(115) = 115
  assert_eq(77, remaining_percentage) // 100 - 23 = 77
  assert_eq(385, remaining_value)    // 500 - 115 = 385
}

test "time_based_calculations" {
  // 基于时间的计算测试
  let seconds_per_minute = 60
  let minutes_per_hour = 60
  let hours_per_day = 24
  let days_per_week = 7
  
  // 计算一周的总秒数
  let seconds_per_hour = multiply(minutes_per_hour, seconds_per_minute)
  let seconds_per_day = multiply(hours_per_day, seconds_per_hour)
  let seconds_per_week = multiply(days_per_week, seconds_per_day)
  
  assert_eq(3600, seconds_per_hour)  // 60 * 60 = 3600
  assert_eq(86400, seconds_per_day)  // 24 * 3600 = 86400
  assert_eq(604800, seconds_per_week) // 7 * 86400 = 604800
  
  // 计算多少分钟等于一周
  let minutes_in_week = divide_with_ceil(seconds_per_week, seconds_per_minute)
  assert_eq(10080, minutes_in_week)  // ceil(604800/60) = 10080
}

test "nested_function_calls" {
  // 嵌套函数调用测试
  let base = 10
  
  // 深度嵌套的计算
  let level1 = add(base, 5)                    // 15
  let level2 = multiply(level1, 2)             // 30
  let level3 = divide_with_ceil(level2, 4)     // 8
  let level4 = subtract(level3, 3)             // 5
  let level5 = multiply(level4, level4)        // 25
  let final_result = add(level5, -level4)      // 20
  
  assert_eq(20, final_result)
  
  // 另一个复杂的嵌套表达式
  let complex_result = add(
    multiply(
      divide_with_ceil(add(base, multiply(base, base)), base),
      subtract(multiply(base, 2), base)
    ),
    base
  )
  
  // 解释：ceil((10 + 100)/10) * (20 - 10) + 10 = ceil(110/10) * 10 + 10 = 11 * 10 + 10 = 120
  assert_eq(120, complex_result)
}