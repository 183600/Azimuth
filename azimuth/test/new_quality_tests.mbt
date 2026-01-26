// 新的高质量 MoonBit 测试用例
// 测试 azimuth 库的核心功能和边界情况

test "add_function_large_numbers" {
  // 测试大数加法运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(2147483646, @azimuth.add(2147483645, 1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(-2147483646, -1))
}

test "multiply_function_precision" {
  // 测试乘法函数的精度和边界情况
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46340, 46340))
}

test "divide_with_ceil_edge_scenarios" {
  // 测试向上取整除法的边缘场景
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 1))
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(2147483647, 1))
}

test "greet_function_complex_strings" {
  // 测试问候函数的复杂字符串处理
  @azimuth.assert_eq_string("Hello, line\nbreak\ttab!", @azimuth.greet("line\nbreak\ttab"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  @azimuth.assert_eq_string("Hello, \\backslash!", @azimuth.greet("\\backslash"))
  @azimuth.assert_eq_string("Hello, emoji 🎊🎉!", @azimuth.greet("emoji 🎊🎉"))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景测试
  let principal = 50000
  let annual_rate = 8
  let years = 5
  
  // 简化的复利计算
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100))
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, annual_rate), 100))
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, annual_rate), 100))
  
  // 验证复利增长
  @azimuth.assert_eq(54000, year1)
  @azimuth.assert_eq(58320, year2)
  @azimuth.assert_eq(62986, year3)
  
  // 计算月供
  let total_months = @azimuth.multiply(years, 12)
  let monthly_payment = @azimuth.divide_with_ceil(year3, total_months)
  @azimuth.assert_eq(875, monthly_payment)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 12345
  let departments = 7
  let base_allocation = @azimuth.divide_with_ceil(total_resources, departments)
  
  @azimuth.assert_eq(1764, base_allocation)
  
  // 验证分配结果
  let total_allocated = @azimuth.multiply(base_allocation, departments)
  @azimuth.assert_eq(12348, total_allocated)
  
  // 计算资源利用率
  let utilization = @azimuth.divide_with_ceil(@azimuth.multiply(total_resources, 100), total_allocated)
  @azimuth.assert_eq(99, utilization)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算测试
  let n = 10
  
  // 计算等差数列和: 1 + 2 + ... + n
  let arithmetic_sum = @azimuth.divide_with_ceil(@azimuth.multiply(n, @azimuth.add(n, 1)), 2)
  @azimuth.assert_eq(55, arithmetic_sum)
  
  // 计算等比数列和: 1 + 2 + 4 + ... + 2^(n-1)
  let geometric_sum = @azimuth.subtract(@azimuth.multiply(2, n), 1)
  @azimuth.assert_eq(19, geometric_sum)
  
  // 验证数列性质
  @azimuth.assert_eq(@azimuth.add(arithmetic_sum, geometric_sum), 74)
}

test "inventory_management_comprehensive" {
  // 综合库存管理测试
  let product_a = 125
  let product_b = 87
  let product_c = 156
  
  let box_capacity_a = 12
  let box_capacity_b = 8
  let box_capacity_c = 15
  
  // 计算每种产品需要的箱子数量
  let boxes_a = @azimuth.divide_with_ceil(product_a, box_capacity_a)
  let boxes_b = @azimuth.divide_with_ceil(product_b, box_capacity_b)
  let boxes_c = @azimuth.divide_with_ceil(product_c, box_capacity_c)
  
  @azimuth.assert_eq(11, boxes_a)
  @azimuth.assert_eq(11, boxes_b)
  @azimuth.assert_eq(11, boxes_c)
  
  // 计算总存储容量
  let total_boxes = @azimuth.add(@azimuth.add(boxes_a, boxes_b), boxes_c)
  let avg_capacity = @azimuth.divide_with_ceil(
    @azimuth.add(@azimuth.add(box_capacity_a, box_capacity_b), box_capacity_c), 3
  )
  let total_capacity = @azimuth.multiply(total_boxes, avg_capacity)
  
  @azimuth.assert_eq(33, total_boxes)
  @azimuth.assert_eq(12, avg_capacity)
  @azimuth.assert_eq(396, total_capacity)
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 测试各种除零情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-2147483648, 0))
  
  // 测试极值运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, 2147483647))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -2147483648))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 场景：项目管理与资源分配
  let project_duration = 18  // 月
  let team_size = 12
  let budget_per_month = 25000
  let tasks_per_person = 8
  
  // 计算项目总预算
  let total_budget = @azimuth.multiply(project_duration, budget_per_month)
  @azimuth.assert_eq(450000, total_budget)
  
  // 计算总任务数
  let total_tasks = @azimuth.multiply(team_size, tasks_per_person)
  @azimuth.assert_eq(96, total_tasks)
  
  // 计算每月需要完成的任务数
  let monthly_tasks = @azimuth.divide_with_ceil(total_tasks, project_duration)
  @azimuth.assert_eq(6, monthly_tasks)
  
  // 计算每个任务的平均预算
  let budget_per_task = @azimuth.divide_with_ceil(total_budget, total_tasks)
  @azimuth.assert_eq(4688, budget_per_task)
  
  // 验证预算分配合理性
  let allocated_budget = @azimuth.multiply(budget_per_task, total_tasks)
  @azimuth.assert_eq(450048, allocated_budget)
  
  // 计算预算余量
  let budget_buffer = @azimuth.subtract(allocated_budget, total_budget)
  @azimuth.assert_eq(48, budget_buffer)
}