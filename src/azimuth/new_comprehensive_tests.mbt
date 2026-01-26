// 新增的综合测试用例 - Azimuth 项目
// 使用标准 MoonBit 测试语法，包含 10 个高质量测试用例

test "mathematical_series_sum" {
  // 测试等差数列求和：1 + 3 + 5 + ... + 19 = 100
  let first_term = 1
  let common_diff = 2
  let num_terms = 10
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let series_sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(100, series_sum)
}

test "area_calculation_scenario" {
  // 测试面积计算场景：计算房间总面积
  let room1_length = 5
  let room1_width = 4
  let room2_length = 6
  let room2_width = 3
  
  let room1_area = multiply(room1_length, room1_width)
  let room2_area = multiply(room2_length, room2_width)
  let total_area = add(room1_area, room2_area)
  
  assert_eq(20, room1_area)
  assert_eq(18, room2_area)
  assert_eq(38, total_area)
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度到华氏度 F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  assert_eq(77, fahrenheit)
}

test "loan_repayment_calculation" {
  // 测试贷款还款计算
  let principal = 10000
  let monthly_payment = 500
  let months_needed = divide_with_ceil(principal, monthly_payment)
  
  assert_eq(20, months_needed)
}

test "batch_processing_efficiency" {
  // 测试批处理效率计算
  let total_items = 157
  let batch_size = 25
  let batches_needed = divide_with_ceil(total_items, batch_size)
  
  assert_eq(7, batches_needed)
}

test "digital_storage_conversion" {
  // 测试数字存储转换计算
  let megabytes = 850
  let kilobytes_per_mb = 1024
  let total_kilobytes = multiply(megabytes, kilobytes_per_mb)
  
  assert_eq(870400, total_kilobytes)
}

test "velocity_distance_time" {
  // 测试速度、距离、时间关系计算
  let speed = 60  // km/h
  let time = 2.5  // hours (represented as 5/2)
  let distance = multiply(speed, divide_with_ceil(multiply(5, 1), 2))
  
  assert_eq(150, distance)
}

test "compound_interest_simplified" {
  // 测试简化复利计算
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 简化计算：每年复利一次
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1158, year3)
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 900
  
  let allocated = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining = add(total_budget, -allocated)
  let additional_projects = divide_with_ceil(remaining, 600)
  
  assert_eq(3900, allocated)
  assert_eq(1100, remaining)
  assert_eq(2, additional_projects)
}

test "greeting_template_system" {
  // 测试问候模板系统
  let user_role = "Admin"
  let user_name = "张三"
  let department = "技术部"
  
  let full_title = user_role + " " + user_name + " - " + department
  let formal_greeting = greet(full_title)
  
  assert_eq_string("Hello, Admin 张三 - 技术部!", formal_greeting)
}