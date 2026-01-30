// 额外增强测试用例 - 为 azimuth 项目添加的高质量测试
// 包含各种边界情况、实际应用场景和复杂逻辑的测试

test "extreme_value_calculations" {
  // 测试极值计算
  azimuth::assert_eq(2000000, azimuth::add(1000000, 1000000))
  azimuth::assert_eq(-2000000, azimuth::add(-1000000, -1000000))
  azimuth::assert_eq(0, azimuth::add(1000000, -1000000))
  azimuth::assert_eq(1000000000000, azimuth::multiply(1000000, 1000000))
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 1))
  azimuth::assert_eq(100, azimuth::divide_with_ceil(100, 1))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(100, 100))
  azimuth::assert_eq(34, azimuth::divide_with_ceil(100, 3))
  azimuth::assert_eq(-33, azimuth::divide_with_ceil(-100, 3))
  azimuth::assert_eq(-34, azimuth::divide_with_ceil(100, -3))
}

test "string_processing_advanced" {
  // 测试高级字符串处理
  azimuth::assert_eq_string("Hello, MoonBit 2024!", azimuth::greet("MoonBit 2024"))
  azimuth::assert_eq_string("Hello, @developer!", azimuth::greet("@developer"))
  azimuth::assert_eq_string("Hello, spaced   out!", azimuth::greet("spaced   out"))
  azimuth::assert_eq_string("Hello, emoji 🎉🚀!", azimuth::greet("emoji 🎉🚀"))
  azimuth::assert_eq_string("Hello, 中文测试!", azimuth::greet("中文测试"))
}

test "complex_business_workflow" {
  // 测试复杂业务工作流
  // 场景：电子商务订单处理
  let product_price = 299
  let quantity = 4
  let shipping_cost = 25
  let tax_rate = 8  // 8%
  
  // 计算商品总价
  let subtotal = azimuth::multiply(product_price, quantity)
  
  // 计算税费
  let tax = azimuth::divide_with_ceil(azimuth::multiply(subtotal, tax_rate), 100)
  
  // 计算订单总额
  let total = azimuth::add(azimuth::add(subtotal, tax), shipping_cost)
  
  azimuth::assert_eq(1196, subtotal)
  azimuth::assert_eq(96, tax)
  azimuth::assert_eq(1317, total)
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  // 场景：项目团队资源分配
  let total_tasks = 87
  let team_members = 5
  let working_days = 10
  
  // 计算每人每天需要完成的任务数
  let total_person_days = azimuth::multiply(team_members, working_days)
  let tasks_per_person_per_day = azimuth::divide_with_ceil(total_tasks, total_person_days)
  
  azimuth::assert_eq(50, total_person_days)
  azimuth::assert_eq(2, tasks_per_person_per_day)
  
  // 验证总处理能力
  let total_capacity = azimuth::multiply(tasks_per_person_per_day, total_person_days)
  azimuth::assert_true(total_capacity >= total_tasks)
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 计算等差数列：2, 5, 8, 11, 14 的和
  let term1 = 2
  let difference = 3
  let num_terms = 5
  
  // 使用公式计算：和 = n/2 * (2a + (n-1)d)
  let series_sum = azimuth::divide_with_ceil(
    azimuth::multiply(num_terms, azimuth::add(azimuth::multiply(2, term1), azimuth::multiply(difference, azimuth::subtract(num_terms, 1)))),
    2
  )
  
  azimuth::assert_eq(40, series_sum)
}

test "inventory_management_comprehensive" {
  // 测试综合库存管理
  // 场景：多产品库存优化
  let product_a_qty = 45
  let product_b_qty = 78
  let product_c_qty = 23
  
  let box_capacity_a = 12
  let box_capacity_b = 15
  let box_capacity_c = 8
  
  // 计算每种产品需要的箱子数
  let boxes_a = azimuth::divide_with_ceil(product_a_qty, box_capacity_a)
  let boxes_b = azimuth::divide_with_ceil(product_b_qty, box_capacity_b)
  let boxes_c = azimuth::divide_with_ceil(product_c_qty, box_capacity_c)
  
  azimuth::assert_eq(4, boxes_a)
  azimuth::assert_eq(6, boxes_b)
  azimuth::assert_eq(3, boxes_c)
  
  // 计算总箱子数和总容量
  let total_boxes = azimuth::add(azimuth::add(boxes_a, boxes_b), boxes_c)
  let total_capacity = azimuth::add(
    azimuth::multiply(boxes_a, box_capacity_a),
    azimuth::add(azimuth::multiply(boxes_b, box_capacity_b), azimuth::multiply(boxes_c, box_capacity_c))
  )
  
  azimuth::assert_eq(13, total_boxes)
  azimuth::assert_eq(174, total_capacity)
}

test "time_and_scheduling_logic" {
  // 测试时间和调度逻辑
  // 场景：会议调度和时间管理
  let meeting_duration = 45  // 分钟
  let break_duration = 15    // 分钟
  let sessions_per_day = 6
  
  // 计算每日总时间
  let daily_meeting_time = azimuth::multiply(meeting_duration, sessions_per_day)
  let daily_break_time = azimuth::multiply(break_duration, azimuth::subtract(sessions_per_day, 1))
  let total_daily_time = azimuth::add(daily_meeting_time, daily_break_time)
  
  azimuth::assert_eq(270, daily_meeting_time)
  azimuth::assert_eq(75, daily_break_time)
  azimuth::assert_eq(345, total_daily_time)
  
  // 计算需要的小时数（向上取整）
  let hours_needed = azimuth::divide_with_ceil(total_daily_time, 60)
  azimuth::assert_eq(6, hours_needed)
}

test "error_handling_and_boundary_conditions" {
  // 测试错误处理和边界条件
  // 除零错误的全面测试
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(1, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(-1, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(999999, 0))
  
  // 极值边界测试
  azimuth::assert_eq(0, azimuth::multiply(0, 999999))
  azimuth::assert_eq(0, azimuth::multiply(999999, 0))
  azimuth::assert_eq(999999, azimuth::add(999999, 0))
  azimuth::assert_eq(-999999, azimuth::add(-999999, 0))
  
  // 边界除法测试
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 1))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(-1, -1))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 1))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, -1))
}

test "data_analysis_and_statistics" {
  // 测试数据分析和统计计算
  // 场景：基本统计分析
  let data_point_1 = 25
  let data_point_2 = 42
  let data_point_3 = 38
  let data_point_4 = 51
  let data_point_5 = 29
  
  // 计算总和
  let sum = azimuth::add(azimuth::add(azimuth::add(data_point_1, data_point_2), azimuth::add(data_point_3, data_point_4)), data_point_5)
  
  // 计算平均值（向下取整）
  let count = 5
  let average = azimuth::divide_with_ceil(sum, count)
  
  // 计算最大值和最小值的差
  let max_value = 51
  let min_value = 25
  let range = azimuth::subtract(max_value, min_value)
  
  azimuth::assert_eq(185, sum)
  azimuth::assert_eq(37, average)
  azimuth::assert_eq(26, range)
}