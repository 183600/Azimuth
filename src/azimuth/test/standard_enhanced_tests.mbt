// 标准 MoonBit 增强测试用例
// 针对 azimuth 库的核心功能进行更深入的测试

test "addition_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 42
  let b = 17
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试负数情况
  let x = -15
  let y = 8
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
}

test "multiplication_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 7
  let c = 3
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)  // 5 * (7 + 3) = 5 * 7 + 5 * 3 = 50
}

test "divide_with_ceil_boundary_cases" {
  // 测试向上取整除法的边界情况
  // 测试相等的情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(10, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  
  // 测试被除数小于除数的情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 10))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  
  // 测试大数除法
  @azimuth.assert_eq(10001, @azimuth.divide_with_ceil(20001, 2))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算年度总成本
  let monthly_cost = 1200
  let months = 12
  let annual_bonus = 5000
  let tax_rate = 20  // 20%
  
  // 计算年度成本
  let annual_cost = @azimuth.multiply(monthly_cost, months)
  
  // 计算税费
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(annual_cost, tax_rate), 100)
  
  // 计算净成本（扣除奖金）
  let net_cost = @azimuth.add(@azimuth.add(annual_cost, tax_amount), -annual_bonus)
  
  @azimuth.assert_eq(14400, annual_cost)
  @azimuth.assert_eq(2880, tax_amount)
  @azimuth.assert_eq(12280, net_cost)
}

test "resource_allocation_calculation" {
  // 资源分配计算
  let total_budget = 10000
  let project_cost = 3500
  let team_size = 8
  let equipment_per_person = 2
  
  // 计算剩余预算
  let remaining_budget = @azimuth.add(total_budget, -project_cost)
  
  // 计算设备总数
  let total_equipment = @azimuth.multiply(team_size, equipment_per_person)
  
  // 计算每人可用预算
  let budget_per_person = @azimuth.divide_with_ceil(remaining_budget, team_size)
  
  @azimuth.assert_eq(6500, remaining_budget)
  @azimuth.assert_eq(16, total_equipment)
  @azimuth.assert_eq(813, budget_per_person)
}

test "time_management_calculation" {
  // 时间管理计算
  let total_tasks = 47
  let tasks_per_day = 5
  let working_days = 6
  
  // 计算需要的工作日
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = @azimuth.multiply(days_needed, 8)
  
  // 计算剩余任务容量
  let remaining_capacity = @azimuth.multiply(tasks_per_day, working_days)
  let unused_capacity = @azimuth.add(remaining_capacity, -total_tasks)
  
  @azimuth.assert_eq(10, days_needed)
  @azimuth.assert_eq(80, total_hours)
  @azimuth.assert_eq(-17, unused_capacity)
}

test "inventory_optimization" {
  // 库存优化计算
  let current_stock = 150
  let daily_sales = 12
  let delivery_time_days = 7
  let safety_buffer = 20
  
  // 计算预测销售量
  let projected_sales = @azimuth.multiply(daily_sales, delivery_time_days)
  
  // 计算再订货点
  let reorder_point = @azimuth.add(projected_sales, safety_buffer)
  
  // 计算是否需要订货
  let should_reorder = if current_stock < reorder_point { 1 } else { 0 }
  
  @azimuth.assert_eq(84, projected_sales)
  @azimuth.assert_eq(104, reorder_point)
  @azimuth.assert_eq(0, should_reorder)  // 不需要订货
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列求和
  let first_term = 3
  let common_difference = 4
  let number_of_terms = 6
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum = @azimuth.divide_with_ceil(
    @azimuth.multiply(
      number_of_terms,
      @azimuth.add(
        @azimuth.multiply(2, first_term),
        @azimuth.multiply(number_of_terms - 1, common_difference)
      )
    ),
    2
  )
  
  // 验证结果：3 + 7 + 11 + 15 + 19 + 23 = 78
  @azimuth.assert_eq(78, sum)
}