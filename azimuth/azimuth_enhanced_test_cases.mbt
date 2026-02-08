// Azimuth 增强测试用例 - 10个高质量的 MoonBit 测试用例
// 专注于边界情况、性能和复杂业务场景

test "azimuth_performance_large_numbers" {
  // 大数运算性能测试
  let large_num1 = 1000000
  let large_num2 = 999999
  
  // 测试大数加法
  assert_eq(1999999, add(large_num1, large_num2))
  
  // 测试大数乘法
  assert_eq(999999000000, multiply(large_num1, large_num2))
  
  // 测试大数除法
  assert_eq(2, divide_with_ceil(large_num1, large_num2))
}

test "azimuth_complex_business_workflow" {
  // 复杂业务工作流测试 - 电商订单处理
  let orders_per_day = 150
  let days_per_month = 30
  let total_orders = multiply(orders_per_day, days_per_month)
  
  let items_per_order = 3
  let total_items = multiply(total_orders, items_per_order)
  
  let warehouse_capacity = 1000
  let warehouses_needed = divide_with_ceil(total_items, warehouse_capacity)
  
  assert_eq(4500, total_orders)  // 150 * 30
  assert_eq(13500, total_items)  // 4500 * 3
  assert_eq(14, warehouses_needed)  // ceil(13500/1000)
}

test "azimuth_financial_calculation_precision" {
  // 金融计算精度测试
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 简单利息计算
  let simple_interest = multiply(principal, multiply(rate, years))
  let total_amount = add(principal, divide_with_ceil(simple_interest, 100))
  
  assert_eq(1500, simple_interest)  // 10000 * 5 * 3
  assert_eq(11500, total_amount)   // 10000 + 1500
}

test "azimuth_resource_allocation_optimization" {
  // 资源分配优化测试
  let total_tasks = 89
  let team_members = 8
  
  // 平均分配任务
  let tasks_per_member = divide_with_ceil(total_tasks, team_members)
  
  // 计算实际分配的任务数
  let allocated_tasks = multiply(tasks_per_member, team_members)
  
  // 计算剩余任务
  let remaining_tasks = subtract(allocated_tasks, total_tasks)
  
  assert_eq(12, tasks_per_member)   // ceil(89/8)
  assert_eq(96, allocated_tasks)    // 12 * 8
  assert_eq(7, remaining_tasks)     // 96 - 89
}

test "azimuth_time_management_scenario" {
  // 时间管理场景测试
  let work_hours_per_day = 8
  let meeting_hours_per_day = 2
  let productive_hours = subtract(work_hours_per_day, meeting_hours_per_day)
  
  let tasks_per_hour = 2
  let tasks_per_day = multiply(productive_hours, tasks_per_hour)
  
  let work_days_per_week = 5
  let tasks_per_week = multiply(tasks_per_day, work_days_per_week)
  
  assert_eq(6, productive_hours)    // 8 - 2
  assert_eq(12, tasks_per_day)      // 6 * 2
  assert_eq(60, tasks_per_week)     // 12 * 5
}

test "azimuth_inventory_restock_calculation" {
  // 库存补货计算测试
  let current_stock = 245
  let safety_stock = 50
  let minimum_level = add(safety_stock, 100)
  
  let daily_consumption = 15
  let lead_time_days = 7
  let lead_time_demand = multiply(daily_consumption, lead_time_days)
  
  let reorder_point = add(minimum_level, lead_time_demand)
  let should_reorder = current_stock < reorder_point
  
  let order_quantity = multiply(daily_consumption, 30)  // 30天供应
  
  assert_eq(150, minimum_level)     // 50 + 100
  assert_eq(105, lead_time_demand)  // 15 * 7
  assert_eq(255, reorder_point)     // 150 + 105
  assert_eq(true, should_reorder)   // 245 < 255
  assert_eq(450, order_quantity)    // 15 * 30
}

test "azimuth_energy_consumption_analysis" {
  // 能耗分析测试
  let devices_count = 25
  let power_per_device = 150  // 瓦特
  let hours_per_day = 24
  
  // 每日能耗
  let daily_consumption = multiply(devices_count, multiply(power_per_device, hours_per_day))
  
  // 转换为千瓦时
  let daily_kwh = divide_with_ceil(daily_consumption, 1000)
  
  // 月度能耗
  let days_per_month = 30
  let monthly_kwh = multiply(daily_kwh, days_per_month)
  
  assert_eq(90000, daily_consumption)  // 25 * 150 * 24
  assert_eq(90, daily_kwh)            // ceil(90000/1000)
  assert_eq(2700, monthly_kwh)         // 90 * 30
}

test "azimuth_data_storage_planning" {
  // 数据存储规划测试
  let records_per_day = 5000
  let size_per_record_kb = 2  // KB
  let days_per_month = 30
  
  // 月度数据量
  let monthly_data_kb = multiply(records_per_day, multiply(size_per_record_kb, days_per_month))
  
  // 转换为MB
  let monthly_data_mb = divide_with_ceil(monthly_data_kb, 1024)
  
  // 年度数据量
  let months_per_year = 12
  let yearly_data_mb = multiply(monthly_data_mb, months_per_year)
  
  // 转换为GB
  let yearly_data_gb = divide_with_ceil(yearly_data_mb, 1024)
  
  assert_eq(300000, monthly_data_kb)   // 5000 * 2 * 30
  assert_eq(293, monthly_data_mb)      // ceil(300000/1024)
  assert_eq(3516, yearly_data_mb)      // 293 * 12
  assert_eq(4, yearly_data_gb)         // ceil(3516/1024)
}

test "azimuth_transportation_logistics" {
  // 物流运输测试
  let total_packages = 178
  let truck_capacity = 24
  
  // 需要的卡车数量
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  
  // 实际运输能力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  
  // 剩余空间
  let remaining_space = subtract(total_capacity, total_packages)
  
  // 每辆卡车的平均负载
  let avg_load_per_truck = divide_with_ceil(total_packages, trucks_needed)
  
  assert_eq(8, trucks_needed)          // ceil(178/24)
  assert_eq(192, total_capacity)       // 8 * 24
  assert_eq(14, remaining_space)       // 192 - 178
  assert_eq(23, avg_load_per_truck)    // ceil(178/8)
}

test "azimuth_error_handling_boundary" {
  // 错误处理和边界条件测试
  // 测试除零保护
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值运算
  assert_eq(0, multiply(0, 1000000))
  assert_eq(0, multiply(1000000, 0))
  assert_eq(1000000, add(1000000, 0))
  assert_eq(1000000, add(0, 1000000))
  
  // 测试负数边界
  assert_eq(-1000000, add(-500000, -500000))
  assert_eq(1000000, multiply(-1000, -1000))
  assert_eq(-1000000, multiply(1000, -1000))
  
  // 测试字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, x!", greet("x"))
}