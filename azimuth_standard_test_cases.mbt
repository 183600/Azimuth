// Azimuth 项目标准测试用例
// 包含 10 个高质量的 MoonBit 测试用例，覆盖核心功能

test "arithmetic_commutative_laws" {
  // 测试算术交换律：加法和乘法的交换性质
  let a = 17
  let b = 29
  
  // 加法交换律：a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律：a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(46, add(17, 29))
  assert_eq(493, multiply(17, 29))
}

test "divide_with_ceil_mixed_scenarios" {
  // 测试向上取整除法的混合场景
  // 正数除法
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(7, divide_with_ceil(41, 6))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-11, 4))
  assert_eq(-6, divide_with_ceil(-23, 4))
  
  // 混合符号除法
  assert_eq(-2, divide_with_ceil(7, -4))
  assert_eq(3, divide_with_ceil(-11, -4))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_international_support" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "complex_business_workflow" {
  // 测试复杂业务工作流：电商订单计算
  let product_price = 89
  let quantity = 6
  let shipping_per_item = 5
  let tax_rate = 8
  let discount_threshold = 500
  let discount_amount = 50
  
  // 计算商品总价
  let subtotal = multiply(product_price, quantity)
  
  // 计算运费
  let total_shipping = multiply(shipping_per_item, quantity)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = add(add(subtotal, total_shipping), tax_amount)
  
  // 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(534, subtotal)
  assert_eq(30, total_shipping)
  assert_eq(43, tax_amount)
  assert_eq(607, pre_discount_total)
  assert_eq(557, final_total)
}

test "mathematical_distributive_law" {
  // 测试数学分配律：a * (b + c) = a * b + a * c
  let a = 12
  let b = 7
  let c = 5
  
  // 左侧：a * (b + c)
  let left_side = multiply(a, add(b, c))
  
  // 右侧：a * b + a * c
  let right_side = add(multiply(a, b), multiply(a, c))
  
  // 验证分配律
  assert_eq(left_side, right_side)
  assert_eq(144, left_side)
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 10000
  let team_a_cost = 3500
  let team_b_cost = 2800
  let team_c_cost = 2200
  
  // 计算已分配预算
  let allocated_budget = add(add(team_a_cost, team_b_cost), team_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量（每个项目800成本）
  let small_projects = divide_with_ceil(remaining_budget, 800)
  
  assert_eq(8500, allocated_budget)
  assert_eq(1500, remaining_budget)
  assert_eq(2, small_projects)
}

test "time_management_calculations" {
  // 测试时间管理计算
  let total_tasks = 87
  let tasks_per_day = 8
  let working_days_per_week = 5
  
  // 计算需要的工作日
  let workdays_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(workdays_needed, working_days_per_week)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = multiply(workdays_needed, 8)
  
  assert_eq(11, workdays_needed)
  assert_eq(3, weeks_needed)
  assert_eq(88, total_hours)
}

test "inventory_turnover_calculation" {
  // 测试库存周转计算
  let initial_inventory = 1200
  let monthly_sales = 180
  let months_to_simulate = 6
  let new_shipment = 500
  
  // 计算总销售量
  let total_sales = multiply(monthly_sales, months_to_simulate)
  
  // 计算销售后的库存
  let inventory_after_sales = add(initial_inventory, -total_sales)
  
  // 计算最终库存（考虑新进货）
  let final_inventory = add(inventory_after_sales, new_shipment)
  
  // 计算周转率
  let turnover_rate = divide_with_ceil(multiply(total_sales, 100), initial_inventory)
  
  assert_eq(1080, total_sales)
  assert_eq(120, inventory_after_sales)
  assert_eq(620, final_inventory)
  assert_eq(90, turnover_rate)
}

test "data_analysis_statistics" {
  // 测试数据分析统计
  let q1_revenue = 45000
  let q2_revenue = 52000
  let q3_revenue = 48000
  let q4_revenue = 61000
  
  // 计算年度总收入
  let annual_revenue = add(add(add(q1_revenue, q2_revenue), q3_revenue), q4_revenue)
  
  // 计算季度平均收入
  let quarterly_average = divide_with_ceil(annual_revenue, 4)
  
  // 计算增长率
  let q2_growth = divide_with_ceil(multiply(q2_revenue - q1_revenue, 100), q1_revenue)
  let q3_growth = divide_with_ceil(multiply(q3_revenue - q2_revenue, 100), q2_revenue)
  let q4_growth = divide_with_ceil(multiply(q4_revenue - q3_revenue, 100), q3_revenue)
  
  assert_eq(206000, annual_revenue)
  assert_eq(51500, quarterly_average)
  assert_eq(16, q2_growth)
  assert_eq(-8, q3_growth)
  assert_eq(27, q4_growth)
}

test "engineering_project_calculation" {
  // 测试工程项目计算
  let foundation_area = 150
  let floor_count = 8
  let height_per_floor = 3
  let window_area_per_floor = 20
  
  // 计算总建筑面积
  let total_floor_area = multiply(foundation_area, floor_count)
  
  // 计算建筑总高度
  let total_height = multiply(floor_count, height_per_floor)
  
  // 计算窗户总面积
  let total_window_area = multiply(window_area_per_floor, floor_count)
  
  // 计算墙面面积（简化计算）
  let perimeter = multiply(4, 50)  // 假设正方形建筑，每边50米
  let wall_area = multiply(perimeter, total_height)
  
  // 计算窗户占墙面面积的百分比
  let window_percentage = divide_with_ceil(multiply(total_window_area, 100), wall_area)
  
  assert_eq(1200, total_floor_area)
  assert_eq(24, total_height)
  assert_eq(160, total_window_area)
  assert_eq(200, perimeter)
  assert_eq(4800, wall_area)
  assert_eq(4, window_percentage)
}