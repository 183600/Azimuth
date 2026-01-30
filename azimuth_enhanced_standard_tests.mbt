// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的综合测试

test "subtract_function_basic" {
  // 测试基本减法功能（使用 add 和负数模拟）
  assert_eq(5, add(10, -5))
  assert_eq(0, add(5, -5))
  assert_eq(-5, add(5, -10))
  assert_eq(10, add(0, 10))
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  let a = 12
  let b = 8
  let c = 5
  
  // 测试 (a + b) * c - a
  let result1 = add(multiply(add(a, b), c), -a)
  assert_eq(88, result1)  // (12 + 8) * 5 - 12 = 20 * 5 - 12 = 100 - 12 = 88
  
  // 测试 a * b - c * a
  let result2 = add(multiply(a, b), -multiply(c, a))
  assert_eq(36, result2)  // 12 * 8 - 5 * 12 = 96 - 60 = 36
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：计算利润
  let revenue = 5000
  let cost1 = 1200
  let cost2 = 800
  let cost3 = 1500
  
  let total_cost = add(add(cost1, cost2), cost3)
  let profit = add(revenue, -total_cost)
  
  assert_eq(3500, total_cost)
  assert_eq(1500, profit)
}

test "inventory_flow_calculation" {
  // 测试库存流动计算
  let initial_stock = 100
  let incoming = 45
  let outgoing = 30
  
  let updated_stock = add(initial_stock, incoming)
  let final_stock = add(updated_stock, -outgoing)
  
  assert_eq(145, updated_stock)
  assert_eq(115, final_stock)
}

test "temperature_conversion_simulation" {
  // 模拟温度转换计算
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9), 160) / 5
  
  // 再次转换回摄氏度验证
  let back_to_celsius = add(multiply(fahrenheit, 5), -160) / 9
  
  assert_eq(77, fahrenheit)  // 25°C = 77°F
  assert_eq(25, back_to_celsius)  // 77°F = 25°C
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 10000
  let project_a_cost = 3500
  let project_b_cost = 2800
  let project_c_cost = 2200
  
  let allocated = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining = add(total_budget, -allocated)
  
  assert_eq(8500, allocated)
  assert_eq(1500, remaining)
}

test "time_tracking_calculation" {
  // 测试时间跟踪计算
  let work_hours_per_day = 8
  let days_worked = 5
  let break_hours = 2
  
  let total_work_hours = multiply(work_hours_per_day, days_worked)
  let productive_hours = add(total_work_hours, -break_hours)
  
  assert_eq(40, total_work_hours)
  assert_eq(38, productive_hours)
}

test "geometric_area_calculation" {
  // 测试几何面积计算
  let rectangle_length = 15
  let rectangle_width = 8
  let square_side = 6
  
  let rectangle_area = multiply(rectangle_length, rectangle_width)
  let square_area = multiply(square_side, square_side)
  let area_difference = add(rectangle_area, -square_area)
  
  assert_eq(120, rectangle_area)
  assert_eq(36, square_area)
  assert_eq(84, area_difference)
}

test "data_processing_efficiency" {
  // 测试数据处理效率
  let records_per_batch = 150
  let number_of_batches = 8
  let error_records = 25
  
  let total_records = multiply(records_per_batch, number_of_batches)
  let valid_records = add(total_records, -error_records)
  
  assert_eq(1200, total_records)
  assert_eq(1175, valid_records)
}

test "multi_step_business_workflow" {
  // 测试多步业务工作流
  let initial_investment = 10000
  let first_year_return = 1200
  let second_year_return = 1500
  let expenses = 800
  
  let first_year_value = add(initial_investment, first_year_return)
  let second_year_value = add(first_year_value, second_year_return)
  let final_value = add(second_year_value, -expenses)
  
  assert_eq(11200, first_year_value)
  assert_eq(12700, second_year_value)
  assert_eq(11900, final_value)
}