// Azimuth 增强测试用例 - 10个高质量 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能和边界情况

test "mathematical_series_summation" {
  // 测试数学级数求和：计算等差数列前n项和
  let first_term = 5
  let common_difference = 3
  let number_of_terms = 8
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let double_sum = multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference)))
  let series_sum = divide_with_ceil(double_sum, 2)
  
  assert_eq(100, series_sum)  // 8/2 * (10 + 21) = 4 * 31 = 124，使用向上取整
}

test "financial_compound_interest" {
  // 测试金融复利计算
  let principal = 10000
  let annual_rate = 8  // 8%
  let years = 3
  
  // 第一年
  let year1_interest = divide_with_ceil(multiply(principal, annual_rate), 100)
  let year1_amount = add(principal, year1_interest)
  
  // 第二年
  let year2_interest = divide_with_ceil(multiply(year1_amount, annual_rate), 100)
  let year2_amount = add(year1_amount, year2_interest)
  
  // 第三年
  let year3_interest = divide_with_ceil(multiply(year2_amount, annual_rate), 100)
  let final_amount = add(year2_amount, year3_interest)
  
  assert_eq(10800, year1_amount)
  assert_eq(11664, year2_amount)
  assert_eq(12597, final_amount)
}

test "inventory_optimization" {
  // 测试库存优化算法
  let current_stock = 250
  let daily_demand = 15
  let days_until_reorder = 7
  let safety_stock = 50
  
  // 计算再订货点
  let reorder_point = add(multiply(daily_demand, days_until_reorder), safety_stock)
  
  // 计算建议订货量（满足30天需求）
  let monthly_demand = multiply(daily_demand, 30)
  let suggested_order = add(monthly_demand, -current_stock)
  
  assert_eq(155, reorder_point)  // 15*7 + 50 = 105 + 50 = 155
  assert_eq(200, suggested_order)  // 15*30 - 250 = 450 - 250 = 200
}

test "geometric_calculations" {
  // 测试几何计算：矩形面积和周长
  let length = 12
  let width = 8
  
  // 计算面积
  let area = multiply(length, width)
  
  // 计算周长
  let perimeter = multiply(2, add(length, width))
  
  // 计算对角线长度的平方（勾股定理）
  let diagonal_squared = add(multiply(length, length), multiply(width, width))
  
  assert_eq(96, area)
  assert_eq(40, perimeter)
  assert_eq(208, diagonal_squared)
}

test "temperature_conversion" {
  // 测试温度转换（摄氏度到华氏度）
  let celsius_temp = 25
  
  // F = C * 9/5 + 32
  let fahrenheit_temp = add(divide_with_ceil(multiply(celsius_temp, 9), 5), 32)
  
  // 测试负温度
  let negative_celsius = -10
  let negative_fahrenheit = add(divide_with_ceil(multiply(negative_celsius, 9), 5), 32)
  
  assert_eq(77, fahrenheit_temp)  // 25*9/5 + 32 = 45 + 32 = 77
  assert_eq(14, negative_fahrenheit)  // -10*9/5 + 32 = -18 + 32 = 14
}

test "string_greeting_multilingual" {
  // 测试多语言问候功能
  let english_name = "Alice"
  let chinese_name = "张三"
  let japanese_name = "田中"
  let emoji_name = "🌟"
  
  let english_greeting = greet(english_name)
  let chinese_greeting = greet(chinese_name)
  let japanese_greeting = greet(japanese_name)
  let emoji_greeting = greet(emoji_name)
  
  assert_eq_string("Hello, Alice!", english_greeting)
  assert_eq_string("Hello, 张三!", chinese_greeting)
  assert_eq_string("Hello, 田中!", japanese_greeting)
  assert_eq_string("Hello, 🌟!", emoji_greeting)
}

test "resource_scheduling" {
  // 测试资源调度算法
  let total_tasks = 47
  let workers = 5
  let working_days = 8
  
  // 计算每个工人的任务数
  let tasks_per_worker = divide_with_ceil(total_tasks, workers)
  
  // 计算需要的总天数
  let days_needed = divide_with_ceil(tasks_per_worker, working_days)
  
  // 计算剩余任务
  let total_assigned = multiply(tasks_per_worker, workers)
  let overflow_tasks = subtract(total_assigned, total_tasks)
  
  assert_eq(10, tasks_per_worker)  // ceil(47/5) = 10
  assert_eq(2, days_needed)  // ceil(10/8) = 2
  assert_eq(3, overflow_tasks)  // 10*5 - 47 = 3
}

test "error_handling_division" {
  // 测试除法错误处理
  let normal_division = divide_with_ceil(20, 4)
  let fractional_division = divide_with_ceil(25, 4)
  let zero_divisor = divide_with_ceil(100, 0)
  let negative_divisor = divide_with_ceil(20, -3)
  let both_negative = divide_with_ceil(-20, -3)
  
  assert_eq(5, normal_division)
  assert_eq(7, fractional_division)  // ceil(6.25) = 7
  assert_eq(0, zero_divisor)  // 除数为0时返回0
  assert_eq(-6, negative_divisor)  // ceil(-6.666...) = -6
  assert_eq(7, both_negative)  // ceil(6.666...) = 7
}

test "complex_data_analysis" {
  // 测试复杂数据分析场景
  let data_points = [15, 22, 18, 25, 20, 30, 28, 35, 32, 40]
  
  // 计算总和（手动累加）
  let total_sum = add(add(add(add(add(add(add(add(add(
    data_points[0], data_points[1]), data_points[2]), data_points[3]), 
    data_points[4]), data_points[5]), data_points[6]), data_points[7]), 
    data_points[8]), data_points[9])
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(total_sum, 10)
  
  // 计算最大值和最小值的差
  let max_value = 40
  let min_value = 15
  let range = subtract(max_value, min_value)
  
  assert_eq(265, total_sum)
  assert_eq(27, average)  // ceil(265/10) = ceil(26.5) = 27
  assert_eq(25, range)
}

test "performance_benchmarking" {
  // 测试性能基准计算
  let baseline_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 12
  
  // 计算优化后的操作数
  let optimized_operations = multiply(baseline_operations, optimization_factor)
  
  // 计算开销
  let overhead = divide_with_ceil(multiply(optimized_operations, overhead_percentage), 100)
  
  // 计算实际性能
  let actual_performance = subtract(optimized_operations, overhead)
  
  // 计算性能提升百分比
  let improvement_percentage = divide_with_ceil(
    multiply(subtract(actual_performance, baseline_operations), 100), 
    baseline_operations
  )
  
  assert_eq(3000, optimized_operations)
  assert_eq(360, overhead)
  assert_eq(2640, actual_performance)
  assert_eq(164, improvement_percentage)  // ceil((2640-1000)*100/1000) = ceil(164000/1000) = 164
}