// 新增的简洁 MoonBit 测试用例
// 包含基本功能测试和实际应用场景

test "extreme_value_operations" {
  // 测试极端数值运算
  let large_num = 1000000
  let small_num = 1
  
  // 大数运算
  assert_eq(1000001, add(large_num, small_num))
  assert_eq(1000000, multiply(large_num, small_num))
  
  // 极端除法情况
  assert_eq(1, divide_with_ceil(small_num, large_num))
  assert_eq(large_num, divide_with_ceil(large_num, small_num))
}

test "mathematical_properties" {
  // 验证数学性质
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 单位元性质
  assert_eq(add(x, 0), x)
  assert_eq(multiply(x, 1), x)
}

test "string_greeting_cases" {
  // 测试字符串问候功能
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "complex_nested_calculations" {
  // 测试嵌套计算
  let base = 10
  let multiplier = 3
  let divisor = 4
  let offset = 7
  
  // 嵌套计算：((base * multiplier) / divisor) + offset
  let step1 = multiply(base, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, offset)
  
  assert_eq(30, step1)
  assert_eq(8, step2)
  assert_eq(15, result)
}

test "financial_compound_interest" {
  // 金融复利计算
  let principal = 10000
  let rate_percent = 5
  
  // 简化的复利计算（每年计算一次）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  assert_eq(10500, year1_amount)
  assert_eq(11025, year2_amount)
  assert_eq(11576, year3_amount)
}

test "resource_allocation" {
  // 资源分配测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 800
  let project_c_cost = 1500
  
  // 计算剩余预算
  let allocated = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining = add(total_budget, -allocated)
  
  // 计算可以启动的小项目数量
  let small_project_cost = 300
  let additional_projects = divide_with_ceil(remaining, small_project_cost)
  
  assert_eq(3500, allocated)
  assert_eq(1500, remaining)
  assert_eq(5, additional_projects)
}

test "time_management" {
  // 时间管理场景
  let daily_work_hours = 8
  let tasks_per_hour = 2
  let total_tasks = 85
  
  // 计算需要的总工作小时数
  let total_hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  
  // 计算需要的工作天数
  let workdays_needed = divide_with_ceil(total_hours_needed, daily_work_hours)
  
  assert_eq(43, total_hours_needed)
  assert_eq(6, workdays_needed)
}

test "inventory_logistics" {
  // 库存物流计算
  let warehouse_capacity = 1000
  let current_stock = 750
  let incoming_shipment = 300
  let items_per_pallet = 25
  
  // 计算接收后的总库存
  let total_after_receiving = add(current_stock, incoming_shipment)
  
  // 计算需要的托盘数量
  let pallets_needed = divide_with_ceil(total_after_receiving, items_per_pallet)
  
  assert_eq(1050, total_after_receiving)
  assert_eq(42, pallets_needed)
}

test "engineering_precision" {
  // 工程精度计算
  let measurement = 100
  let tolerance_percent = 5
  let adjustment_factor = 2
  
  // 计算容差范围
  let tolerance = divide_with_ceil(multiply(measurement, tolerance_percent), 100)
  
  // 计算调整后的测量值
  let adjusted_measurement = add(measurement, multiply(adjustment_factor, tolerance))
  
  // 计算最终验证值
  let final_value = divide_with_ceil(multiply(adjusted_measurement, 3), 4)
  
  assert_eq(5, tolerance)
  assert_eq(110, adjusted_measurement)
  assert_eq(83, final_value)
}

test "business_calculations" {
  // 业务计算测试
  let product_price = 25
  let quantity = 8
  let shipping_cost = 15
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  
  // 计算折扣前总价
  let total_before_discount = add(subtotal, shipping_cost)
  
  // 计算最终总价
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}