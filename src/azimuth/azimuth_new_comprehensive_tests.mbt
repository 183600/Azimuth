// Azimuth 项目的新增综合测试用例
// 使用标准 MoonBit 测试语法，覆盖各种边界情况和实际应用场景

test "mathematical_series_sum" {
  // 测试等差数列求和：1 + 2 + 3 + ... + 10
  let n = 10
  let sum = divide_with_ceil(multiply(n, add(n, 1)), 2)
  assert_eq(55, sum)
}

test "temperature_conversion" {
  // 模拟温度转换计算（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(divide_with_ceil(multiply(celsius, 9), 5), 1), 32)
  assert_eq(77, fahrenheit)
}

test "banking_interest_calculation" {
  // 银行利息计算（简化版）
  let principal = 1000
  let interest_rate = 5  // 5%
  let years = 3
  
  let interest_per_year = divide_with_ceil(multiply(principal, interest_rate), 100)
  let total_interest = multiply(interest_per_year, years)
  let final_amount = add(principal, total_interest)
  
  assert_eq(150, total_interest)
  assert_eq(1150, final_amount)
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_resources = 100
  let project_a_needs = 35
  let project_b_needs = 28
  let project_c_needs = 22
  
  let allocated = add(add(project_a_needs, project_b_needs), project_c_needs)
  let remaining = add(total_resources, -allocated)
  
  assert_eq(85, allocated)
  assert_eq(15, remaining)
}

test "time_zone_conversion" {
  // 时区转换计算（简化版）
  let local_time = 14  // 下午2点
  let time_difference = 8  // 与目标时区相差8小时
  
  let target_time = add(local_time, time_difference)
  let adjusted_time = if target_time >= 24 { add(target_time, -24) } else { target_time }
  
  assert_eq(22, adjusted_time)
}

test "shopping_cart_with_tax" {
  // 购物车含税计算
  let item1_price = 29
  let item1_quantity = 2
  let item2_price = 15
  let item2_quantity = 3
  let tax_rate = 8  // 8%
  
  let subtotal = add(multiply(item1_price, item1_quantity), multiply(item2_price, item2_quantity))
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax_amount)
  
  assert_eq(103, subtotal)
  assert_eq(9, tax_amount)
  assert_eq(112, total)
}

test "digital_storage_conversion" {
  // 数字存储单位转换
  let megabytes = 1024
  let gigabytes = divide_with_ceil(megabytes, 1024)
  let kilobytes = multiply(megabytes, 1024)
  
  assert_eq(1, gigabytes)
  assert_eq(1048576, kilobytes)
}

test "speed_distance_time_calculation" {
  // 速度、距离、时间计算
  let speed = 60  // km/h
  let time = 25  // 2.5 hours expressed as 25/10
  let distance = multiply(speed, divide_with_ceil(time, 10))
  
  assert_eq(180, distance)
}

test "array_index_calculation" {
  // 数组索引计算（模拟二维数组转一维数组）
  let row = 3
  let col = 4
  let cols_per_row = 5
  
  let linear_index = add(multiply(row, cols_per_row), col)
  
  assert_eq(19, linear_index)
}

test "probability_simulation" {
  // 概率模拟计算
  let total_outcomes = 100
  let favorable_outcomes = 25
  
  let probability_percent = divide_with_ceil(multiply(favorable_outcomes, 100), total_outcomes)
  
  assert_eq(25, probability_percent)
}