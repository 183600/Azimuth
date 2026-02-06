// Azimuth 项目新增标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "mathematical_sequence_calculation" {
  // 数学序列计算测试
  // 计算等差数列前n项和：1 + 3 + 5 + 7 + 9 = 25
  let first_term = 1
  let common_diff = 2
  let num_terms = 5
  
  // 使用公式计算：n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  // 手动计算验证
  let manual_sum = add(add(add(add(1, 3), 5), 7), 9)
  
  assert_eq(25, sum_formula)
  assert_eq(25, manual_sum)
}

test "financial_compound_interest" {
  // 金融复利计算测试
  // 场景：计算投资增长
  let principal = 1000  // 本金
  let rate_percent = 5   // 年利率5%
  let years = 3          // 投资年限
  
  // 简化复利计算（每年计算一次）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  // 验证增长
  assert_eq(1050, year1)  // 1000 + 50
  assert_eq(1103, year2)  // 1050 + 53 (向上取整)
  assert_eq(1158, year3)  // 1103 + 55 (向上取整)
}

test "inventory_optimization" {
  // 库存优化算法测试
  // 场景：计算最优订货批量
  let annual_demand = 1200  // 年需求量
  let ordering_cost = 50    // 每次订货成本
  let holding_cost_percent = 20  // 年持有成本率
  let unit_cost = 10        // 单位成本
  
  // 简化的EOQ计算（向上取整）
  let holding_cost_per_unit = divide_with_ceil(multiply(unit_cost, holding_cost_percent), 100)
  let eoq_numerator = multiply(2, multiply(annual_demand, ordering_cost))
  let eoq_denominator = holding_cost_per_unit
  let optimal_order_quantity = divide_with_ceil(eoq_numerator, eoq_denominator)
  
  // 计算年订货次数
  let orders_per_year = divide_with_ceil(annual_demand, optimal_order_quantity)
  
  assert_eq(2, holding_cost_per_unit)  // 10 * 20% = 2
  assert_eq(120000, eoq_numerator)     // 2 * 1200 * 50 = 120000
  assert_eq(60000, optimal_order_quantity)  // ceil(120000/2) = 60000
  assert_eq(1, orders_per_year)        // ceil(1200/60000) = 1
}

test "temperature_conversion" {
  // 温度转换算法测试
  // 场景：华氏度与摄氏度转换（简化整数计算）
  let fahrenheit = 98
  
  // 华氏度转摄氏度：C = (F-32)*5/9
  let celsius_exact = multiply(fahrenheit - 32, 5)
  let celsius = divide_with_ceil(celsius_exact, 9)
  
  // 摄氏度转华氏度：F = C*9/5 + 32
  let back_to_fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  assert_eq(330, celsius_exact)     // (98-32)*5 = 330
  assert_eq(37, celsius)           // ceil(330/9) = 37
  assert_eq(99, back_to_fahrenheit)  // ceil(37*9/5) + 32 = 99
}

test "project_management_timeline" {
  // 项目管理时间线计算测试
  // 场景：计算项目完成时间
  let total_tasks = 85
  let team_velocity = 18  // 每个冲刺完成的任务数
  let sprint_duration = 2  // 每个冲刺的周数
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, team_velocity)
  
  // 计算总项目时间（周）
  let total_weeks = multiply(sprints_needed, sprint_duration)
  
  // 计算剩余任务容量
  let total_capacity = multiply(sprints_needed, team_velocity)
  let unused_capacity = subtract(total_capacity, total_tasks)
  
  assert_eq(5, sprints_needed)      // ceil(85/18) = 5
  assert_eq(10, total_weeks)        // 5 * 2 = 10
  assert_eq(90, total_capacity)     // 5 * 18 = 90
  assert_eq(5, unused_capacity)     // 90 - 85 = 5
}

test "data_structure_array_simulation" {
  // 数据结构数组模拟测试
  // 场景：模拟数组操作和索引计算
  let array_size = 20
  let element_size = 4
  let base_address = 1000
  
  // 计算第5个元素的地址（0-based索引）
  let element_index = 4
  let element_offset = multiply(element_index, element_size)
  let element_address = add(base_address, element_offset)
  
  // 计算数组总大小
  let total_array_size = multiply(array_size, element_size)
  
  // 计算可以容纳的完整页数（每页16字节）
  let page_size = 16
  let pages_needed = divide_with_ceil(total_array_size, page_size)
  
  assert_eq(16, element_offset)      // 4 * 4 = 16
  assert_eq(1016, element_address)   // 1000 + 16 = 1016
  assert_eq(80, total_array_size)    // 20 * 4 = 80
  assert_eq(5, pages_needed)         // ceil(80/16) = 5
}

test "string_processing_multilingual" {
  // 多语言字符串处理测试
  // 场景：处理不同语言的问候语
  let english_name = "World"
  let chinese_name = "世界"
  let spanish_name = "Mundo"
  let french_name = "Monde"
  
  // 生成多语言问候
  let english_greeting = greet(english_name)
  let chinese_greeting = greet(chinese_name)
  let spanish_greeting = greet(spanish_name)
  let french_greeting = greet(french_name)
  
  // 验证问候语格式
  assert_eq_string("Hello, World!", english_greeting)
  assert_eq_string("Hello, 世界!", chinese_greeting)
  assert_eq_string("Hello, Mundo!", spanish_greeting)
  assert_eq_string("Hello, Monde!", french_greeting)
  
  // 验证字符串长度
  assert_eq(13, english_greeting.length())
  assert_eq(10, chinese_greeting.length())
  assert_eq(13, spanish_greeting.length())
  assert_eq(13, french_greeting.length())
}

test "geometric_calculation" {
  // 几何计算测试
  // 场景：计算矩形的周长和面积相关计算
  let length = 12
  let width = 8
  
  // 计算周长和面积
  let perimeter = multiply(2, add(length, width))
  let area = multiply(length, width)
  
  // 计算正方形边长（面积相同）
  let square_side = divide_with_ceil(area, length)
  
  // 计算需要的地砖数量（每块地砖覆盖2x2区域）
  let tile_area = 4
  let tiles_needed = divide_with_ceil(area, tile_area)
  
  assert_eq(40, perimeter)          // 2 * (12 + 8) = 40
  assert_eq(96, area)               // 12 * 8 = 96
  assert_eq(8, square_side)         // ceil(96/12) = 8
  assert_eq(24, tiles_needed)       // ceil(96/4) = 24
}

test "network_bandwidth_calculation" {
  // 网络带宽计算测试
  // 场景：计算数据传输时间
  let file_size_mb = 850  // 文件大小（MB）
  let network_speed_mbps = 50  // 网络速度（Mbps）
  
  // 计算理论传输时间（秒）
  let transfer_time_seconds = divide_with_ceil(file_size_mb, network_speed_mbps)
  
  // 转换为分钟
  let transfer_time_minutes = divide_with_ceil(transfer_time_seconds, 60)
  
  // 考虑网络开销，增加20%的缓冲时间
  let buffer_percentage = 20
  let buffer_time = divide_with_ceil(multiply(transfer_time_seconds, buffer_percentage), 100)
  let total_time_with_buffer = add(transfer_time_seconds, buffer_time)
  
  assert_eq(17, transfer_time_seconds)      // ceil(850/50) = 17
  assert_eq(1, transfer_time_minutes)       // ceil(17/60) = 1
  assert_eq(4, buffer_time)                 // ceil(17*20/100) = 4
  assert_eq(21, total_time_with_buffer)     // 17 + 4 = 21
}

test "cryptography_simple_hash" {
  // 简单密码学哈希模拟测试
  // 场景：模拟简单的校验和计算
  let input1 = 12
  let input2 = 8
  let input3 = 15
  let input4 = 6
  let input5 = 23
  let input6 = 11
  let prime_modulus = 97
  
  // 计算简单校验和
  let checksum_part1 = add(add(add(input1, input2), input3), input4)
  let checksum_part2 = add(add(input5, input6), checksum_part1)
  let simple_checksum = divide_with_ceil(checksum_part2, prime_modulus)
  
  // 计算加权校验和
  let weighted_sum = add(add(add(multiply(input1, 1), multiply(input2, 2)), 
                              multiply(input3, 3)), multiply(input4, 4))
  let weighted_checksum = divide_with_ceil(weighted_sum, prime_modulus)
  
  assert_eq(41, checksum_part1)    // 12 + 8 + 15 + 6 = 41
  assert_eq(75, checksum_part2)    // 23 + 11 + 41 = 75
  assert_eq(1, simple_checksum)    // ceil(75/97) = 1
  assert_eq(2, weighted_checksum)  // ceil(124/97) = 2
}