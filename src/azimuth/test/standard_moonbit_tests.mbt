// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(15, @azimuth.add(8, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 3
  let b = 4
  let c = 5
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)  // 3 × (4 + 5) = 3 × 4 + 3 × 5 = 27
}

test "divide_with_ceil_comprehensive_test" {
  // 全面测试向上取整除法功能
  // 正数除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))  // 12/3 = 4 -> 4
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, -3))  // 10/-3 = -3.333... -> -3
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3)) // -11/-3 = 3.666... -> 4
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))    // 除数为0返回0
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))    // 0除以任何数返回0
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, Español!", @azimuth.greet("Español"))
  @azimuth.assert_eq_string("Hello, Русский!", @azimuth.greet("Русский"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let product_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping = 12
  let discount = 20
  
  // 计算小计
  let subtotal = @azimuth.multiply(product_price, quantity)
  // 计算税费
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = @azimuth.add(subtotal, @azimuth.add(tax, shipping))
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(356, subtotal)
  @azimuth.assert_eq(29, tax)
  @azimuth.assert_eq(397, total_before_discount)
  @azimuth.assert_eq(377, final_total)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum_formula = @azimuth.divide_with_ceil(
    @azimuth.multiply(number_of_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  @azimuth.assert_eq(120, sum_formula)  // 10/2 * (6 + 18) = 5 * 24 = 120
}

test "time_management_calculation" {
  // 时间管理计算：项目完成时间
  let tasks_per_day = 8
  let total_tasks = 45
  let working_days_per_week = 5
  
  // 计算需要的工作日
  let workdays_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  // 计算需要的周数
  let weeks_needed = @azimuth.divide_with_ceil(workdays_needed, working_days_per_week)
  
  @azimuth.assert_eq(6, workdays_needed)  // ceil(45/8) = 6
  @azimuth.assert_eq(2, weeks_needed)     // ceil(6/5) = 2
}

test "resource_allocation_optimization" {
  // 资源分配优化
  let total_budget = 10000
  let project_a_cost = 3500
  let project_b_cost = 2800
  let project_c_cost = 4200
  
  // 计算已分配预算
  let allocated_budget = @azimuth.add(@azimuth.add(project_a_cost, project_b_cost), project_c_cost)
  // 计算剩余预算
  let remaining_budget = @azimuth.add(total_budget, -allocated_budget)
  
  // 计算可以启动的小项目数量（每个500）
  let small_projects_count = @azimuth.divide_with_ceil(remaining_budget, 500)
  
  @azimuth.assert_eq(10500, allocated_budget)
  @azimuth.assert_eq(-500, remaining_budget)
  @azimuth.assert_eq(0, small_projects_count)  // 预算超支，无法启动新项目
}

test "statistical_analysis_calculation" {
  // 统计分析计算
  let data_points = [15, 22, 18, 25, 20, 17, 23]
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data_points[0], data_points[1]), data_points[2]), data_points[3]), data_points[4]), data_points[5]), data_points[6])
  let count = 7
  let mean = @azimuth.divide_with_ceil(sum, count)
  
  // 计算方差（简化版本）
  let variance_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(data_points[0] - mean, data_points[0] - mean),
    @azimuth.multiply(data_points[1] - mean, data_points[1] - mean)),
    @azimuth.multiply(data_points[2] - mean, data_points[2] - mean)),
    @azimuth.multiply(data_points[3] - mean, data_points[3] - mean)),
    @azimuth.multiply(data_points[4] - mean, data_points[4] - mean)),
    @azimuth.multiply(data_points[5] - mean, data_points[5] - mean)),
    @azimuth.multiply(data_points[6] - mean, data_points[6] - mean))
  
  let variance = @azimuth.divide_with_ceil(variance_sum, count)
  
  @azimuth.assert_eq(140, sum)
  @azimuth.assert_eq(20, mean)
  @azimuth.assert_eq(76, variance)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况
  // 测试极端值
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))  // 接近Int最大值
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1)) // 接近Int最小值
  
  // 测试零的各种运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, 2147483647))
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483648, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
  
  // 测试除零安全性
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-2147483648, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试空字符串和特殊字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, \t\n!", @azimuth.greet("\t\n"))
}