// Azimuth 增强单元测试
// 包含8个高质量的 MoonBit 测试用例，使用标准测试语法
// 测试覆盖：基本功能、边界情况、错误处理和实际应用场景

test "arithmetic_sequence_operations" {
  // 算术序列操作测试
  let start = 5
  let difference = 3
  let count = 6
  
  // 计算等差数列的和：n/2 * (2a + (n-1)d)
  let first_term = start
  let last_term = add(start, multiply(difference, subtract(count, 1)))
  let sequence_sum = divide_with_ceil(multiply(count, add(first_term, last_term)), 2)
  
  assert_eq(5, first_term)           // 首项
  assert_eq(20, last_term)           // 末项：5 + 3*(6-1) = 20
  assert_eq(75, sequence_sum)        // 和：6/2 * (5+20) = 75
}

test "temperature_conversion_scenario" {
  // 温度转换场景测试
  // 摄氏度到华氏度的转换：F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = add(multiply(celsius, divide_with_ceil(9, 5)), 32)
  
  assert_eq(77, fahrenheit)          // 25 * 9/5 + 32 = 77
  
  // 验证冰点和沸点
  let freezing_c = 0
  let freezing_f = add(multiply(freezing_c, divide_with_ceil(9, 5)), 32)
  let boiling_c = 100
  let boiling_f = add(multiply(boiling_c, divide_with_ceil(9, 5)), 32)
  
  assert_eq(32, freezing_f)          // 冰点
  assert_eq(212, boiling_f)          // 沸点
}

test "inventory_restock_calculation" {
  // 库存补货计算测试
  let current_stock = 47
  let daily_demand = 8
  let lead_time_days = 5
  let safety_stock = 15
  
  // 计算总需求
  let total_demand = multiply(daily_demand, lead_time_days)
  assert_eq(40, total_demand)        // 8 * 5 = 40
  
  // 计算补货点
  let reorder_point = add(total_demand, safety_stock)
  assert_eq(55, reorder_point)       // 40 + 15 = 55
  
  // 计算需要补货的数量
  let restock_needed = subtract(reorder_point, current_stock)
  assert_eq(8, restock_needed)       // 55 - 47 = 8
}

test "complex_division_precision" {
  // 复杂除法精度测试
  // 测试各种除法情况的向上取整行为
  assert_eq(1, divide_with_ceil(1, 2))      // 0.5 -> 1
  assert_eq(2, divide_with_ceil(3, 2))      // 1.5 -> 2
  assert_eq(3, divide_with_ceil(5, 2))      // 2.5 -> 3
  assert_eq(4, divide_with_ceil(7, 2))      // 3.5 -> 4
  assert_eq(5, divide_with_ceil(9, 2))      // 4.5 -> 5
  
  // 测试大数的除法精度
  assert_eq(10001, divide_with_ceil(20001, 2))    // 10000.5 -> 10001
  assert_eq(66667, divide_with_ceil(200000, 3))   // 66666.66 -> 66667
}

test "mathematical_series_validation" {
  // 数学级数验证测试
  // 验证平方和公式：1² + 2² + ... + n² = n(n+1)(2n+1)/6
  let n = 5
  let square_sum_formula = divide_with_ceil(multiply(n, multiply(add(n, 1), add(multiply(2, n), 1))), 6)
  
  // 手动计算前5个平方数的和：1 + 4 + 9 + 16 + 25 = 55
  let manual_sum = add(add(add(add(1, 4), 9), 16), 25)
  
  assert_eq(55, square_sum_formula)
  assert_eq(55, manual_sum)
  assert_eq(square_sum_formula, manual_sum)
}

test "project_resource_allocation" {
  // 项目资源分配测试
  let total_tasks = 87
  let team_size = 6
  let working_days = 15
  let tasks_per_person = divide_with_ceil(total_tasks, team_size)
  
  assert_eq(15, tasks_per_person)     // ceil(87/6) = 15
  
  // 计算总工作能力
  let total_capacity = multiply(team_size, working_days)
  assert_eq(90, total_capacity)       // 6 * 15 = 90
  
  // 验证任务能否完成
  let tasks_remaining = subtract(total_capacity, total_tasks)
  assert_eq(3, tasks_remaining)       // 90 - 87 = 3
  
  // 计算每个人的实际任务数
  let actual_tasks_per_person = divide_with_ceil(total_tasks, team_size)
  assert_eq(15, actual_tasks_per_person)
}

test "string_greeting_multilingual" {
  // 多语言问候字符串测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Universo!", greet("Universo"))
  assert_eq_string("Hello, Monde!", greet("Monde"))
  assert_eq_string("Hello, мир!", greet("мир"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 12345!", greet("12345"))
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let annual_rate = 5    // 5%
  let years = 3
  
  // 简化的复利计算（使用整数运算）
  // 第一年：10000 + 5% = 10500
  let year1 = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  assert_eq(10500, year1)
  
  // 第二年：10500 + 5% = 11025
  let year2 = add(year1, divide_with_ceil(multiply(year1, annual_rate), 100))
  assert_eq(11025, year2)
  
  // 第三年：11025 + 5% = 11576
  let year3 = add(year2, divide_with_ceil(multiply(year2, annual_rate), 100))
  assert_eq(11576, year3)
  
  // 计算总收益
  let total_profit = subtract(year3, principal)
  assert_eq(1576, total_profit)
}