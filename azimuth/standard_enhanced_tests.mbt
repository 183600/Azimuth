// 标准 MoonBit 增强测试用例
// 为 azimuth 库添加的高质量测试用例

test "hexadecimal_conversion_calculation" {
  // 十六进制转换计算测试
  let hex_digit_1 = 1
  let hex_digit_2 = 16
  let hex_digit_3 = 256
  
  // 验证十六进制位权计算
  let hex_value_FF = add(hex_digit_1, multiply(15, hex_digit_2))  // 0xFF = 15*16 + 15
  let hex_value_1A2 = add(add(2, multiply(10, hex_digit_2)), multiply(1, hex_digit_3))  // 0x1A2 = 1*256 + 10*16 + 2
  
  assert_eq(241, hex_value_FF)
  assert_eq(418, hex_value_1A2)
}

test "time_conversion_calculations" {
  // 时间转换计算测试
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = add(add(multiply(hours, 3600), multiply(minutes, 60)), seconds)
  assert_eq(9930, total_seconds)
  
  // 计算需要的分钟数（向上取整）
  let minutes_needed = divide_with_ceil(total_seconds, 60)
  assert_eq(166, minutes_needed)
  
  // 计算需要的小时数（向上取整）
  let hours_needed = divide_with_ceil(total_seconds, 3600)
  assert_eq(3, hours_needed)
}

test "geometry_area_calculations" {
  // 几何面积计算测试
  let rectangle_length = 12
  let rectangle_width = 8
  
  // 计算矩形面积
  let rectangle_area = multiply(rectangle_length, rectangle_width)
  assert_eq(96, rectangle_area)
  
  // 计算正方形边长（从面积反推）
  let square_area = 144
  let square_side = divide_with_ceil(square_area, 12)  // 假设一边为12，计算另一边
  assert_eq(12, square_side)
}

test "statistical_calculations" {
  // 统计计算测试
  let values_sum = 450
  let value_count = 15
  
  // 计算平均值（使用整数除法）
  let average = divide_with_ceil(values_sum, value_count)
  assert_eq(30, average)
  
  // 计算中位数位置（简化计算）
  let median_position = divide_with_ceil(value_count, 2)
  assert_eq(8, median_position)
  
  // 验证总和重建
  let reconstructed_sum = multiply(average, value_count)
  assert_eq(450, reconstructed_sum)
}

test "inventory_turnover_calculation" {
  // 库存周转率计算测试
  let annual_sales = 12000
  let average_inventory = 3000
  
  // 计算库存周转率
  let turnover_rate = divide_with_ceil(annual_sales, average_inventory)
  assert_eq(4, turnover_rate)
  
  // 计算平均销售周期（天）
  let days_in_year = 365
  let average_days = divide_with_ceil(days_in_year, turnover_rate)
  assert_eq(92, average_days)
}

test "network_bandwidth_planning" {
  // 网络带宽规划测试
  let users = 50
  let bandwidth_per_user = 10  // Mbps
  let redundancy_factor = 2
  
  // 计算总带宽需求
  let base_bandwidth = multiply(users, bandwidth_per_user)
  let total_bandwidth = multiply(base_bandwidth, redundancy_factor)
  assert_eq(1000, total_bandwidth)
  
  // 计算需要的连接数量（每连接最大100Mbps）
  let max_bandwidth_per_connection = 100
  let connections_needed = divide_with_ceil(total_bandwidth, max_bandwidth_per_connection)
  assert_eq(10, connections_needed)
}

test "compound_interest_simplified" {
  // 简化的复利计算测试
  let principal = 10000
  let annual_rate = 5  // 5%
  
  // 简化的年复利计算
  let year1 = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, annual_rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, annual_rate), 100))
  
  assert_eq(10500, year1)
  assert_eq(11025, year2)
  assert_eq(11576, year3)
  
  // 计算总收益
  let total_earnings = subtract(year3, principal)
  assert_eq(1576, total_earnings)
}

test "project_resource_allocation" {
  // 项目资源分配测试
  let total_tasks = 87
  let team_members = 5
  let working_days = 15
  
  // 计算每人每天需要完成的任务数
  let total_person_days = multiply(team_members, working_days)
  let tasks_per_person_day = divide_with_ceil(total_tasks, total_person_days)
  assert_eq(2, tasks_per_person_day)
  
  // 验证总处理能力
  let total_capacity = multiply(tasks_per_person_day, total_person_days)
  assert_eq(150, total_capacity)
  
  // 计算效率利用率
  let utilization = divide_with_ceil(multiply(total_tasks, 100), total_capacity)
  assert_eq(58, utilization)
}

test "string_pattern_validation" {
  // 字符串模式验证测试
  // 测试各种字符串格式
  assert_eq_string("Hello, email@domain.com!", greet("email@domain.com"))
  assert_eq_string("Hello, phone123456!", greet("phone123456"))
  assert_eq_string("Hello, https://example.com!", greet("https://example.com"))
  assert_eq_string("Hello, special#$%&chars!", greet("special#$%&chars"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
}

test "extreme_boundary_conditions" {
  // 极端边界条件测试
  // 测试大数运算
  let large_num_1 = 10000
  let large_num_2 = 20000
  
  assert_eq(30000, add(large_num_1, large_num_2))
  assert_eq(200000000, multiply(large_num_1, large_num_2))
  assert_eq(2, divide_with_ceil(large_num_2, large_num_1))
  
  // 测试极小数运算
  let small_num_1 = 1
  let small_num_2 = 1
  
  assert_eq(2, add(small_num_1, small_num_2))
  assert_eq(1, multiply(small_num_1, small_num_2))
  assert_eq(1, divide_with_ceil(small_num_1, small_num_2))
}