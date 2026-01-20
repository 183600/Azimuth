// Enhanced MoonBit Test Suite
// 包含10个精心设计的测试用例，重点关注实际应用场景和边界情况

test "divide_with_ceil_basic_functionality" {
  // 测试 divide_with_ceil 函数的基本功能
  // 正常情况下的向上取整除法
  azimuth::assert_eq(3, azimuth::divide_with_ceil(10, 4))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(11, 4))
  azimuth::assert_eq(2, azimuth::divide_with_ceil(8, 4))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 4))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 4))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  azimuth::assert_eq(-2, azimuth::divide_with_ceil(-10, 4))
  azimuth::assert_eq(-2, azimuth::divide_with_ceil(-9, 4))
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-11, 4))
  azimuth::assert_eq(-2, azimuth::divide_with_ceil(10, -4))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(-10, -4))
}

test "page_pagination_calculation" {
  // 实际应用：分页计算
  // 计算显示100个项目，每页显示23个项目，需要多少页
  let total_items = 100
  let items_per_page = 23
  let total_pages = azimuth::divide_with_ceil(total_items, items_per_page)
  azimuth::assert_eq(5, total_pages)
  
  // 测试其他分页场景
  azimuth::assert_eq(1, azimuth::divide_with_ceil(10, 20))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(50, 20))
  azimuth::assert_eq(10, azimuth::divide_with_ceil(100, 10))
}

test "resource_allocation_planning" {
  // 实际应用：资源分配规划
  // 有45个任务需要分配给8个处理器，每个处理器最多处理6个任务
  let total_tasks = 45
  let max_tasks_per_processor = 6
  let min_processors_needed = azimuth::divide_with_ceil(total_tasks, max_tasks_per_processor)
  azimuth::assert_eq(8, min_processors_needed)
  
  // 测试其他资源分配场景
  azimuth::assert_eq(4, azimuth::divide_with_ceil(15, 4))
  azimuth::assert_eq(7, azimuth::divide_with_ceil(31, 5))
}

test "shipping_container_optimization" {
  // 实际应用：集装箱优化
  // 有127件商品，每个集装箱最多装30件，需要多少个集装箱
  let total_products = 127
  let capacity_per_container = 30
  let containers_needed = azimuth::divide_with_ceil(total_products, capacity_per_container)
  azimuth::assert_eq(5, containers_needed)
  
  // 测试其他装载场景
  azimuth::assert_eq(3, azimuth::divide_with_ceil(60, 25))
  azimuth::assert_eq(10, azimuth::divide_with_ceil(95, 10))
}

test "time_slot_scheduling" {
  // 实际应用：时间段调度
  // 会议需要135分钟，每个时间槽30分钟，需要多少个时间槽
  let meeting_duration = 135
  let slot_duration = 30
  let slots_needed = azimuth::divide_with_ceil(meeting_duration, slot_duration)
  azimuth::assert_eq(5, slots_needed)
  
  // 测试其他调度场景
  azimuth::assert_eq(2, azimuth::divide_with_ceil(45, 30))
  azimuth::assert_eq(4, azimuth::divide_with_ceil(120, 35))
}

test "batch_processing_workflow" {
  // 实际应用：批处理工作流
  // 处理85个文件，每批最多处理20个文件，需要多少批
  let total_files = 85
  let batch_size = 20
  let batches_needed = azimuth::divide_with_ceil(total_files, batch_size)
  azimuth::assert_eq(5, batches_needed)
  
  // 测试其他批处理场景
  azimuth::assert_eq(1, azimuth::divide_with_ceil(15, 20))
  azimuth::assert_eq(8, azimuth::divide_with_ceil(100, 13))
}

test "complex_calculation_with_divide_ceil" {
  // 复杂计算：结合多种运算的实际场景
  // 场景：计算项目的总成本（包括向上取整的资源成本）
  let base_cost = 1000
  let resource_units = 17
  let cost_per_unit = 60
  let units_per_package = 5
  
  // 计算需要的包装数量（向上取整）
  let packages_needed = azimuth::divide_with_ceil(resource_units, units_per_package)
  azimuth::assert_eq(4, packages_needed)
  
  // 计算资源总成本
  let resource_cost = azimuth::multiply(packages_needed, cost_per_unit)
  azimuth::assert_eq(240, resource_cost)
  
  // 计算总成本
  let total_cost = azimuth::add(base_cost, resource_cost)
  azimuth::assert_eq(1240, total_cost)
}

test "financial_calculation_with_rounding" {
  // 金融计算：涉及向上取整的利息计算
  // 场景：贷款计算，利息按月计算，不足一月按一月计算
  let principal = 10000
  let annual_rate = 6  // 6%
  let months = 7
  
  // 计算总天数并向上取整到月
  let total_days = azimuth::multiply(months, 30)
  let rounded_months = azimuth::divide_with_ceil(total_days, 30)
  azimuth::assert_eq(7, rounded_months)
  
  // 计算利息
  let monthly_rate = azimuth::divide_with_ceil(annual_rate, 12)
  azimuth::assert_eq(1, monthly_rate)
  
  let total_interest = azimuth::multiply(principal, azimuth::multiply(monthly_rate, rounded_months)) / 100
  azimuth::assert_eq(700, total_interest)
  
  // 计算还款总额
  let total_repayment = azimuth::add(principal, total_interest)
  azimuth::assert_eq(10700, total_repayment)
}

test "edge_case_comprehensive_test" {
  // 边界情况综合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极大值的除法
  azimuth::assert_eq(1, azimuth::divide_with_ceil(max_val, max_val))
  azimuth::assert_eq(2, azimuth::divide_with_ceil(azimuth::add(max_val, 1), max_val))
  
  // 测试极小值的除法
  azimuth::assert_eq(1, azimuth::divide_with_ceil(min_val, min_val))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(min_val, max_val))
  
  // 测试特殊情况
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 1))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, -1))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, max_val))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, min_val))
  
  // 测试除数为1和-1的情况
  azimuth::assert_eq(5, azimuth::divide_with_ceil(5, 1))
  azimuth::assert_eq(-5, azimuth::divide_with_ceil(-5, 1))
  azimuth::assert_eq(-5, azimuth::divide_with_ceil(5, -1))
  azimuth::assert_eq(5, azimuth::divide_with_ceil(-5, -1))
}