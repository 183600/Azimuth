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
  let last_term = @azimuth.add(start, @azimuth.multiply(difference, @azimuth.subtract(count, 1)))
  let sequence_sum = @azimuth.divide_with_ceil(@azimuth.multiply(count, @azimuth.add(first_term, last_term)), 2)
  
  @azimuth.assert_eq(5, first_term)           // 首项
  @azimuth.assert_eq(20, last_term)           // 末项：5 + 3*(6-1) = 20
  @azimuth.assert_eq(75, sequence_sum)        // 和：6/2 * (5+20) = 75
}

test "temperature_conversion_scenario" {
  // 温度转换场景测试
  // 摄氏度到华氏度的转换：F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, @azimuth.divide_with_ceil(9, 5)), 32)
  
  @azimuth.assert_eq(77, fahrenheit)          // 25 * 9/5 + 32 = 77
  
  // 验证冰点和沸点
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, @azimuth.divide_with_ceil(9, 5)), 32)
  let boiling_c = 100
  let boiling_f = @azimuth.add(@azimuth.multiply(boiling_c, @azimuth.divide_with_ceil(9, 5)), 32)
  
  @azimuth.assert_eq(32, freezing_f)          // 冰点
  @azimuth.assert_eq(212, boiling_f)          // 沸点
}

test "inventory_restock_calculation" {
  // 库存补货计算测试
  let current_stock = 47
  let daily_demand = 8
  let lead_time_days = 5
  let safety_stock = 15
  
  // 计算总需求
  let total_demand = @azimuth.multiply(daily_demand, lead_time_days)
  @azimuth.assert_eq(40, total_demand)        // 8 * 5 = 40
  
  // 计算补货点
  let reorder_point = @azimuth.add(total_demand, safety_stock)
  @azimuth.assert_eq(55, reorder_point)       // 40 + 15 = 55
  
  // 计算需要补货的数量
  let restock_needed = @azimuth.subtract(reorder_point, current_stock)
  @azimuth.assert_eq(8, restock_needed)       // 55 - 47 = 8
}

test "complex_division_precision" {
  // 复杂除法精度测试
  // 测试各种除法情况的向上取整行为
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))      // 0.5 -> 1
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))      // 1.5 -> 2
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(5, 2))      // 2.5 -> 3
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(7, 2))      // 3.5 -> 4
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(9, 2))      // 4.5 -> 5
  
  // 测试大数的除法精度
  @azimuth.assert_eq(10001, @azimuth.divide_with_ceil(20001, 2))    // 10000.5 -> 10001
  @azimuth.assert_eq(66667, @azimuth.divide_with_ceil(200000, 3))   // 66666.66 -> 66667
}

test "mathematical_series_validation" {
  // 数学级数验证测试
  // 验证平方和公式：1² + 2² + ... + n² = n(n+1)(2n+1)/6
  let n = 5
  let square_sum_formula = @azimuth.divide_with_ceil(@azimuth.multiply(n, @azimuth.multiply(@azimuth.add(n, 1), @azimuth.add(@azimuth.multiply(2, n), 1))), 6)
  
  // 手动计算前5个平方数的和：1 + 4 + 9 + 16 + 25 = 55
  let manual_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 4), 9), 16), 25)
  
  @azimuth.assert_eq(55, square_sum_formula)
  @azimuth.assert_eq(55, manual_sum)
  @azimuth.assert_eq(square_sum_formula, manual_sum)
}

test "project_resource_allocation" {
  // 项目资源分配测试
  let total_tasks = 87
  let team_size = 6
  let working_days = 15
  let tasks_per_person = @azimuth.divide_with_ceil(total_tasks, team_size)
  
  @azimuth.assert_eq(15, tasks_per_person)     // ceil(87/6) = 15
  
  // 计算总工作能力
  let total_capacity = @azimuth.multiply(team_size, working_days)
  @azimuth.assert_eq(90, total_capacity)       // 6 * 15 = 90
  
  // 验证任务能否完成
  let tasks_remaining = @azimuth.subtract(total_capacity, total_tasks)
  @azimuth.assert_eq(3, tasks_remaining)       // 90 - 87 = 3
  
  // 计算每个人的实际任务数
  let actual_tasks_per_person = @azimuth.divide_with_ceil(total_tasks, team_size)
  @azimuth.assert_eq(15, actual_tasks_per_person)
}

test "string_greeting_multilingual" {
  // 多语言问候字符串测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Universo!", @azimuth.greet("Universo"))
  @azimuth.assert_eq_string("Hello, Monde!", @azimuth.greet("Monde"))
  @azimuth.assert_eq_string("Hello, мир!", @azimuth.greet("мир"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let annual_rate = 5    // 5%
  let years = 3
  
  // 简化的复利计算（使用整数运算）
  // 第一年：10000 + 5% = 10500
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100))
  @azimuth.assert_eq(10500, year1)
  
  // 第二年：10500 + 5% = 11025
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, annual_rate), 100))
  @azimuth.assert_eq(11025, year2)
  
  // 第三年：11025 + 5% = 11576
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, annual_rate), 100))
  @azimuth.assert_eq(11576, year3)
  
  // 计算总收益
  let total_profit = @azimuth.subtract(year3, principal)
  @azimuth.assert_eq(1576, total_profit)
}