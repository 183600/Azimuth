// 高质量聚焦测试用例 - 针对 azimuth 库核心功能
// 使用标准 MoonBit 测试语法编写

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  
  // 测试负数情况
  let x = -15
  let y = 8
  assert_eq(add(x, y), add(y, x))
  
  // 测试零的情况
  assert_eq(add(0, 123), add(123, 0))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 12
  let c = 7
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive_behavior" {
  // 测试向上取整除法的各种情况
  
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))  // 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(3, divide_with_ceil(10, -3))   // -3.333... -> -3
  assert_eq(4, divide_with_ceil(-11, -3))  // 3.666... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为零
  assert_eq(0, divide_with_ceil(0, 5))     // 零除以正数
}

test "greet_function_multilingual_support" {
  // 测试多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Mundo!", greet("Mundo"))
  assert_eq_string("Hello, Мир!", greet("Мир"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  
  // 测试特殊字符
  assert_eq_string("Hello, John-Doe!", greet("John-Doe"))
  assert_eq_string("Hello, O'Reilly!", greet("O'Reilly"))
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款计算
  let principal = 100000  // 本金
  let annual_rate = 5     // 年利率5%
  let years = 3           // 3年
  
  // 简化的复利计算
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, annual_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, annual_rate), 100))
  
  // 验证结果
  assert_eq(105000, year1_amount)
  assert_eq(110250, year2_amount)
  assert_eq(115763, year3_amount)
}

test "inventory_optimization_scenario" {
  // 库存优化场景
  let product_demand = 157    // 产品需求量
  let box_capacity = 24       // 每箱容量
  let safety_stock = 20       // 安全库存
  
  // 计算需要的箱子数
  let boxes_needed = divide_with_ceil(product_demand, box_capacity)
  
  // 计算总库存需求
  let total_inventory_needed = add(product_demand, safety_stock)
  
  // 计算实际需要准备的箱子数
  let actual_boxes = divide_with_ceil(total_inventory_needed, box_capacity)
  
  assert_eq(7, boxes_needed)        // ceil(157/24) = 7
  assert_eq(177, total_inventory_needed)  // 157 + 20 = 177
  assert_eq(8, actual_boxes)        // ceil(177/24) = 8
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等比数列
  let first_term = 3
  let ratio = 2
  let terms = 6
  
  // 计算等比数列的和
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  let term6 = multiply(term5, ratio)
  
  let sequence_sum = add(add(add(add(add(term1, term2), term3), term4), term5), term6)
  
  assert_eq(6, term2)
  assert_eq(12, term3)
  assert_eq(24, term4)
  assert_eq(48, term5)
  assert_eq(96, term6)
  assert_eq(189, sequence_sum)
}

test "resource_allocation_optimization" {
  // 资源分配优化
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 900
  
  // 计算已分配预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的额外小型项目数量（每个小型项目成本300）
  let small_projects = divide_with_ceil(remaining_budget, 300)
  
  assert_eq(3900, allocated_budget)
  assert_eq(1100, remaining_budget)
  assert_eq(4, small_projects)
}

test "time_management_efficiency" {
  // 时间管理效率计算
  let daily_working_hours = 8
  let task_complexity_factor = 3
  let base_task_time = 2  // 基础任务时间（小时）
  
  // 计算复杂任务所需时间
  let complex_task_time = multiply(base_task_time, task_complexity_factor)
  
  // 计算一天可以完成的任务数
  let tasks_per_day = divide_with_ceil(daily_working_hours, complex_task_time)
  
  // 计算完成15个任务需要的工作日
  let total_tasks = 15
  let workdays_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  assert_eq(6, complex_task_time)
  assert_eq(2, tasks_per_day)  // ceil(8/6) = 2
  assert_eq(8, workdays_needed)  // ceil(15/2) = 8
}

test "data_structures_array_simulation" {
  // 数据结构数组模拟
  // 模拟数组操作：计算数组元素的和与平均值
  let element1 = 25
  let element2 = 17
  let element3 = 33
  let element4 = 42
  let element5 = 19
  
  // 计算数组元素的和
  let array_sum = add(add(add(add(element1, element2), element3), element4), element5)
  
  // 计算平均值（向上取整）
  let array_length = 5
  let average = divide_with_ceil(array_sum, array_length)
  
  assert_eq(136, array_sum)
  assert_eq(28, average)  // ceil(136/5) = ceil(27.2) = 28
}