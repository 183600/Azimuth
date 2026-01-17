// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，包含多样化的测试场景

test "add_large_numbers" {
  // 测试大数加法
  assert_eq(azimuth::add(999999, 1), 1000000)
  assert_eq(azimuth::add(500000, 500000), 1000000)
  assert_eq(azimuth::add(123456, 654321), 777777)
}

test "add_sequence_calculation" {
  // 测试序列计算
  // 计算 1+2+3+...+10 的和
  let sum_1_to_10 = azimuth::add(
    azimuth::add(
      azimuth::add(
        azimuth::add(
          azimuth::add(
            azimuth::add(
              azimuth::add(
                azimuth::add(
                  azimuth::add(1, 2), 3), 4), 5), 6), 7), 8), 9), 10)
  assert_eq(55, sum_1_to_10)
}

test "multiply_power_calculations" {
  // 测试幂次计算
  // 2^4 = 16
  let power_2_4 = azimuth::multiply(
    azimuth::multiply(
      azimuth::multiply(2, 2), 2), 2)
  assert_eq(16, power_2_4)
  
  // 3^3 = 27
  let power_3_3 = azimuth::multiply(
    azimuth::multiply(3, 3), 3)
  assert_eq(27, power_3_3)
}

test "multiply_area_calculations" {
  // 测试面积计算
  // 圆形面积近似计算 (π ≈ 3.14, 但这里使用整数)
  let radius = 5
  let circle_area_approx = azimuth::multiply(
    azimuth::multiply(radius, radius), 3)
  assert_eq(75, circle_area_approx)
  
  // 梯形面积 = (上底 + 下底) * 高 / 2
  let top_base = 6
  let bottom_base = 10
  let height = 4
  let trapezoid_area = azimuth::multiply(
    azimuth::add(top_base, bottom_base), height) / 2
  assert_eq(32, trapezoid_area)
}

test "greet_multilingual_names" {
  // 测试多语言名称问候
  assert_eq(azimuth::greet("张三"), "Hello, 张三!")
  assert_eq(azimuth::greet("李四"), "Hello, 李四!")
  assert_eq(azimuth::greet("José"), "Hello, José!")
  assert_eq(azimuth::greet("François"), "Hello, François!")
}

test "greet_numeric_names" {
  // 测试数字名称问候
  assert_eq(azimuth::greet("123"), "Hello, 123!")
  assert_eq(azimuth::greet("007"), "Hello, 007!")
  assert_eq(azimuth::greet("2023"), "Hello, 2023!")
}

test "calculate_total_cost" {
  // 测试总成本计算
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 40
  let item2_quantity = 2
  
  let item1_total = azimuth::multiply(item1_price, item1_quantity)
  let item2_total = azimuth::multiply(item2_price, item2_quantity)
  let grand_total = azimuth::add(item1_total, item2_total)
  
  assert_eq(155, grand_total)
}

test "calculate_age_difference" {
  // 测试年龄差计算
  let age1 = 35
  let age2 = 28
  let age_diff = azimuth::add(age1, -age2)
  assert_eq(7, age_diff)
  
  // 反向计算
  let age_diff_reverse = azimuth::add(age2, -age1)
  assert_eq(-7, age_diff_reverse)
}

test "calculate_score_average" {
  // 测试分数平均值计算 (整数除法)
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let score4 = 80
  
  let total_score = azimuth::add(
    azimuth::add(
      azimuth::add(score1, score2), score3), score4)
  let average_score = total_score / 4
  
  assert_eq(87, average_score)
}

test "calculate_time_difference" {
  // 测试时间差计算
  let start_hour = 9
  let end_hour = 17
  let work_hours = azimuth::add(end_hour, -start_hour)
  assert_eq(8, work_hours)
  
  // 计算一周工作小时数
  let weekly_hours = azimuth::multiply(work_hours, 5)
  assert_eq(40, weekly_hours)
}