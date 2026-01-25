// 新增的高质量 MoonBit 测试用例
// 为 azimuth 库添加 10 个标准测试用例，覆盖核心功能和实际应用场景

test "arithmetic_sequence_calculation" {
  // 测试等差数列计算
  let first_term = 3
  let common_difference = 4
  let terms_count = 5
  
  // 计算第5项：a_n = a_1 + (n-1)*d
  let fifth_term = add(first_term, multiply(common_difference, terms_count - 1))
  assert_eq(19, fifth_term)  // 3 + 4*4 = 19
  
  // 计算前5项和：S_n = n/2 * (2*a_1 + (n-1)*d)
  let sum_first_five = divide_with_ceil(
    multiply(terms_count, add(multiply(2, first_term), multiply(common_difference, terms_count - 1))),
    2
  )
  assert_eq(55, sum_first_five)  // 5/2 * (6 + 16) = 55
}

test "time_conversion_practical" {
  // 测试实际时间转换场景
  let total_minutes = 157
  
  // 转换为小时和分钟
  let hours = divide_with_ceil(total_minutes, 60)
  let remaining_minutes = subtract(total_minutes, multiply(hours - 1, 60))
  
  assert_eq(3, hours)  // ceil(157/60) = 3 小时
  assert_eq(37, remaining_minutes)  // 157 - 2*60 = 37 分钟
  
  // 验证总分钟数
  let total_converted = add(multiply(hours - 1, 60), remaining_minutes)
  assert_eq(total_minutes, total_converted)
}

test "inventory_batch_processing" {
  // 测试库存批次处理
  let total_products = 234
  let batch_size = 25
  let defective_rate = 5  // 5%
  
  // 计算批次数量
  let batches_needed = divide_with_ceil(total_products, batch_size)
  assert_eq(10, batches_needed)  // ceil(234/25) = 10
  
  // 计算合格产品数量
  let defective_count = divide_with_ceil(multiply(total_products, defective_rate), 100)
  let qualified_products = subtract(total_products, defective_count)
  assert_eq(222, qualified_products)  // 234 - ceil(234*5/100) = 234 - 12 = 222
  
  // 验证处理能力
  let total_capacity = multiply(batches_needed, batch_size)
  assert_eq(250, total_capacity)
}

test "geometric_progression_analysis" {
  // 测试等比数列分析
  let first_term = 2
  let common_ratio = 3
  let term_position = 4
  
  // 计算第4项：a_n = a_1 * r^(n-1)
  let fourth_term = multiply(first_term, multiply(common_ratio, multiply(common_ratio, common_ratio)))
  assert_eq(54, fourth_term)  // 2 * 3^3 = 54
  
  // 计算前4项和：S_n = a_1 * (r^n - 1) / (r - 1)
  let sum_first_four = divide_with_ceil(
    multiply(first_term, subtract(multiply(common_ratio, multiply(common_ratio, multiply(common_ratio, common_ratio))), 1)),
    common_ratio - 1
  )
  assert_eq(80, sum_first_four)  // 2 * (81-1) / 2 = 80
}

test "string_formatting_validation" {
  // 测试字符串格式化验证
  // 测试各种特殊字符组合
  assert_eq_string("Hello, JavaScript!", greet("JavaScript"))
  assert_eq_string("Hello, C++!", greet("C++"))
  assert_eq_string("Hello, Python3.9!", greet("Python3.9"))
  assert_eq_string("Hello, HTML5!", greet("HTML5"))
  assert_eq_string("Hello, CSS3!", greet("CSS3"))
  
  // 测试包含空格的字符串
  assert_eq_string("Hello, React Native!", greet("React Native"))
  assert_eq_string("Hello, Node.js!", greet("Node.js"))
}

test "financial_compound_interest" {
  // 测试金融复利计算
  let principal = 10000
  let annual_rate = 8  // 8%
  let years = 3
  
  // 简化的年复利计算
  let year1_end = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2_end = add(year1_end, divide_with_ceil(multiply(year1_end, annual_rate), 100))
  let year3_end = add(year2_end, divide_with_ceil(multiply(year2_end, annual_rate), 100))
  
  // 验证复利增长
  assert_eq(10800, year1_end)  // 10000 + 8%
  assert_eq(11664, year2_end)  // 10800 + 8%
  assert_eq(12597, year3_end)  // 11664 + 8%
  
  // 计算月供（简化计算）
  let total_months = multiply(years, 12)
  let monthly_payment = divide_with_ceil(year3_end, total_months)
  assert_eq(350, monthly_payment)  // ceil(12597/36)
}

test "resource_allocation_efficiency" {
  // 测试资源分配效率
  let total_tasks = 89
  let team_size = 6
  let working_days = 15
  
  // 计算人均任务数
  let tasks_per_person = divide_with_ceil(total_tasks, team_size)
  assert_eq(15, tasks_per_person)  // ceil(89/6) = 15
  
  // 计算每天需要完成的任务数
  let daily_tasks_per_person = divide_with_ceil(tasks_per_person, working_days)
  assert_eq(1, daily_tasks_per_person)  // ceil(15/15) = 1
  
  // 验证总处理能力
  let total_capacity = multiply(multiply(team_size, daily_tasks_per_person), working_days)
  assert_eq(90, total_capacity)
  
  // 计算效率百分比
  let efficiency = divide_with_ceil(multiply(total_tasks, 100), total_capacity)
  assert_eq(99, efficiency)  // ceil(89*100/90) = 99%
}

test "coordinate_geometry_calculations" {
  // 测试坐标几何计算
  // 计算两点间距离的平方（避免开方运算）
  let x1 = 3
  let y1 = 4
  let x2 = 7
  let y2 = 1
  
  let delta_x = subtract(x2, x1)
  let delta_y = subtract(y2, y1)
  let distance_squared = add(multiply(delta_x, delta_x), multiply(delta_y, delta_y))
  
  assert_eq(4, delta_x)  // 7 - 3 = 4
  assert_eq(-3, delta_y)  // 1 - 4 = -3
  assert_eq(25, distance_squared)  // 4^2 + (-3)^2 = 16 + 9 = 25
  
  // 计算中点坐标
  let midpoint_x = divide_with_ceil(add(x1, x2), 2)
  let midpoint_y = divide_with_ceil(add(y1, y2), 2)
  
  assert_eq(5, midpoint_x)  // ceil(10/2) = 5
  assert_eq(3, midpoint_y)  // ceil(5/2) = 3
}

test "data_structure_performance" {
  // 测试数据结构性能
  // 模拟数组扩容策略
  let initial_capacity = 8
  let current_size = 6
  let new_elements = 5
  
  // 计算是否需要扩容
  let total_required = add(current_size, new_elements)
  let needs_expansion = total_required > initial_capacity
  
  assert_true(needs_expansion)  // 6 + 5 = 11 > 8
  
  // 计算新容量（通常翻倍）
  let new_capacity = multiply(initial_capacity, 2)
  assert_eq(16, new_capacity)
  
  // 计算扩容后的利用率
  let utilization_rate = divide_with_ceil(multiply(total_required, 100), new_capacity)
  assert_eq(69, utilization_rate)  // ceil(11*100/16) = 69%
}

test "comprehensive_business_workflow" {
  // 测试综合业务工作流
  // 场景：电商订单处理
  let order_items = 47
  let items_per_box = 8
  let shipping_cost_per_box = 15
  let handling_fee = 25
  let tax_rate = 8  // 8%
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(order_items, items_per_box)
  assert_eq(6, boxes_needed)  // ceil(47/8) = 6
  
  // 计算运输成本
  let shipping_cost = multiply(boxes_needed, shipping_cost_per_box)
  assert_eq(90, shipping_cost)  // 6 * 15 = 90
  
  // 计算总费用（含手续费）
  let subtotal = add(shipping_cost, handling_fee)
  assert_eq(115, subtotal)  // 90 + 25 = 115
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  assert_eq(10, tax_amount)  // ceil(115*8/100) = 10
  
  // 计算总费用
  let total_cost = add(subtotal, tax_amount)
  assert_eq(125, total_cost)  // 115 + 10 = 125
  
  // 讪�证每个箱子的平均成本
  let avg_cost_per_box = divide_with_ceil(total_cost, boxes_needed)
  assert_eq(21, avg_cost_per_box)  // ceil(125/6) = 21
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}