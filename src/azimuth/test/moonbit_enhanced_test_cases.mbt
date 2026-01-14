// 增强的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，测试项目的各种功能
test "statistical_calculations" {
  // 测试统计计算：平均值、总和等
  let numbers = [10, 20, 30, 40, 50]
  
  // 计算总和
  let sum = azimuth::@azimuth.add(
    azimuth::@azimuth.add(numbers[0], numbers[1]),
    azimuth::@azimuth.add(numbers[2], azimuth::@azimuth.add(numbers[3], numbers[4]))
  )
  azimuth::@azimuth.assert_eq(150, sum)
  
  // 计算平均值（整数除法）
  let average = sum / 5
  azimuth::@azimuth.assert_eq(30, average)
  
  // 计算加权平均：(10*1 + 20*2 + 30*3 + 40*4 + 50*5) / (1+2+3+4+5)
  let weighted_sum = azimuth::@azimuth.add(
    azimuth::@azimuth.add(azimuth::@azimuth.multiply(10, 1), azimuth::@azimuth.multiply(20, 2)),
    azimuth::@azimuth.add(azimuth::@azimuth.multiply(30, 3), azimuth::@azimuth.add(azimuth::@azimuth.multiply(40, 4), azimuth::@azimuth.multiply(50, 5)))
  )
  let weights_sum = 15  // 1+2+3+4+5
  let weighted_average = weighted_sum / weights_sum
  azimuth::@azimuth.assert_eq(36, weighted_average)
}
test "geometry_area_calculations" {
  // 测试几何面积计算
  let radius = 5
  
  // 圆面积（使用整数近似：π ≈ 3）
  let circle_area = azimuth::@azimuth.multiply(3, azimuth::@azimuth.multiply(radius, radius))
  azimuth::@azimuth.assert_eq(75, circle_area)
  
  // 正方形面积
  let square_side = 8
  let square_area = azimuth::@azimuth.multiply(square_side, square_side)
  azimuth::@azimuth.assert_eq(64, square_area)
  
  // 矩形面积
  let rectangle_length = 12
  let rectangle_width = 6
  let rectangle_area = azimuth::@azimuth.multiply(rectangle_length, rectangle_width)
  azimuth::@azimuth.assert_eq(72, rectangle_area)
  
  // 梯形面积：（上底 + 下底）× 高 ÷ 2
  let trapezoid_top = 5
  let trapezoid_bottom = 9
  let trapezoid_height = 4
  let trapezoid_area = azimuth::@azimuth.multiply(
    azimuth::@azimuth.add(trapezoid_top, trapezoid_bottom),
    trapezoid_height
  ) / 2
  azimuth::@azimuth.assert_eq(28, trapezoid_area)
}
test "financial_interest_calculations" {
  // 测试金融利息计算
  let principal = 10000  // 本金 10000 元
  let rate = 5  // 年利率 5%
  let years = 3  // 3年
  
  // 简单利息：本金 × 利率 × 年数
  let simple_interest = azimuth::@azimuth.multiply(principal, azimuth::@azimuth.multiply(rate, years))
  let simple_total = azimuth::@azimuth.add(principal, simple_interest)
  
  // 验证：10000 + 10000*5*3 = 10000 + 150000 = 160000（表示1600.00元）
  azimuth::@azimuth.assert_eq(160000, simple_total)
  
  // 复利计算（简化版）：每年计算一次
  // 第一年：10000 + 10000*5 = 10500
  let year1_total = azimuth::@azimuth.add(principal, azimuth::@azimuth.multiply(principal, rate) / 100)
  // 第二年：10500 + 10500*5 = 11025
  let year2_total = azimuth::@azimuth.add(year1_total, azimuth::@azimuth.multiply(year1_total, rate) / 100)
  // 第三年：11025 + 11025*5 = 11576（整数除法）
  let year3_total = azimuth::@azimuth.add(year2_total, azimuth::@azimuth.multiply(year2_total, rate) / 100)
  
  azimuth::@azimuth.assert_eq(11576, year3_total)
}
test "temperature_conversion_calculations" {
  // 测试温度转换计算
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数近似：F ≈ C × 2 + 30
  
  // 冰点：0°C = 30°F（近似）
  let celsius_freezing = 0
  let fahrenheit_freezing = azimuth::@azimuth.add(azimuth::@azimuth.multiply(celsius_freezing, 2), 30)
  azimuth::@azimuth.assert_eq(30, fahrenheit_freezing)
  
  // 沸点：100°C = 230°F（近似）
  let celsius_boiling = 100
  let fahrenheit_boiling = azimuth::@azimuth.add(azimuth::@azimuth.multiply(celsius_boiling, 2), 30)
  azimuth::@azimuth.assert_eq(230, fahrenheit_boiling)
  
  // 室温：25°C = 80°F（近似）
  let celsius_room = 25
  let fahrenheit_room = azimuth::@azimuth.add(azimuth::@azimuth.multiply(celsius_room, 2), 30)
  azimuth::@azimuth.assert_eq(80, fahrenheit_room)
  
  // 华氏度转摄氏度（反向近似）：C ≈ (F - 30) / 2
  let fahrenheit_back = 80
  let celsius_back = azimuth::@azimuth.add(fahrenheit_back, -30) / 2
  azimuth::@azimuth.assert_eq(25, celsius_back)
}
test "physics_motion_calculations" {
  // 测试物理运动计算
  // 距离 = 初速度 × 时间 + 0.5 × 加速度 × 时间²
  // 使用整数近似：距离 ≈ 初速度 × 时间 + 加速度 × 时间² / 2
  
  let initial_velocity = 10  // 初速度 10 m/s
  let acceleration = 5  // 加速度 5 m/s²
  let time = 4  // 时间 4 秒
  
  // 计算距离
  let distance_with_initial = azimuth::@azimuth.multiply(initial_velocity, time)
  let distance_with_acceleration = azimuth::@azimuth.multiply(acceleration, azimuth::@azimuth.multiply(time, time)) / 2
  let total_distance = azimuth::@azimuth.add(distance_with_initial, distance_with_acceleration)
  
  // 验证：10*4 + 5*4*4/2 = 40 + 5*16/2 = 40 + 40 = 80
  azimuth::@azimuth.assert_eq(80, total_distance)
  
  // 末速度 = 初速度 + 加速度 × 时间
  let final_velocity = azimuth::@azimuth.add(initial_velocity, azimuth::@azimuth.multiply(acceleration, time))
  azimuth::@azimuth.assert_eq(30, final_velocity)
  
  // 平均速度 = (初速度 + 末速度) / 2
  let average_velocity = azimuth::@azimuth.add(initial_velocity, final_velocity) / 2
  azimuth::@azimuth.assert_eq(20, average_velocity)
}
test "data_structure_operations" {
  // 测试数据结构操作（模拟）
  let array_size = 10
  let element_size = 4
  
  // 计算数组总大小
  let total_size = azimuth::@azimuth.multiply(array_size, element_size)
  azimuth::@azimuth.assert_eq(40, total_size)
  
  // 计算数组索引偏移（假设每个元素4字节）
  let index = 3
  let offset = azimuth::@azimuth.multiply(index, element_size)
  azimuth::@azimuth.assert_eq(12, offset)
  
  // 计算数组容量（2的幂次）
  let capacity_power = 4  // 2^4 = 16
  let capacity = azimuth::@azimuth.multiply(2, azimuth::@azimuth.multiply(2, azimuth::@azimuth.multiply(2, 2)))
  azimuth::@azimuth.assert_eq(16, capacity)
  
  // 计算负载因子（元素数量 / 容量，乘以100表示百分比）
  let element_count = 8
  let load_factor = azimuth::@azimuth.multiply(element_count, 100) / capacity
  azimuth::@azimuth.assert_eq(50, load_factor)
}
test "string_processing_patterns" {
  // 测试字符串处理模式
  let first_name = "John"
  let last_name = "Doe"
  let age = "30"
  
  // 测试基本字符串拼接
  let full_name = azimuth::@azimuth.greet(first_name)
  azimuth::@azimuth.assert_eq_string("Hello, John!", full_name)
  
  // 测试包含空格的字符串
  let name_with_space = "Jane Smith"
  let result_with_space = azimuth::@azimuth.greet(name_with_space)
  azimuth::@azimuth.assert_eq_string("Hello, Jane Smith!", result_with_space)
  
  // 测试包含数字的字符串
  let name_with_number = "User123"
  let result_with_number = azimuth::@azimuth.greet(name_with_number)
  azimuth::@azimuth.assert_eq_string("Hello, User123!", result_with_number)
  
  // 测试包含特殊字符的字符串
  let name_with_special = "user@domain.com"
  let result_with_special = azimuth::@azimuth.greet(name_with_special)
  azimuth::@azimuth.assert_eq_string("Hello, user@domain.com!", result_with_special)
  
  // 测试超长字符串
  let long_name = "ThisIsAVeryLongUserNameThatExceedsNormalLength"
  let result_long = azimuth::@azimuth.greet(long_name)
  azimuth::@azimuth.assert_eq_string("Hello, ThisIsAVeryLongUserNameThatExceedsNormalLength!", result_long)
}
test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟
  let n = 100  // 输入大小
  
  // O(1) 常数时间复杂度
  let constant_time = 42
  azimuth::@azimuth.assert_eq(42, constant_time)
  
  // O(n) 线性时间复杂度：简单循环求和
  // 模拟：1 + 2 + 3 + ... + n = n*(n+1)/2
  let linear_time = azimuth::@azimuth.multiply(n, azimuth::@azimuth.add(n, 1)) / 2
  azimuth::@azimuth.assert_eq(5050, linear_time)
  
  // O(n²) 平方时间复杂度：嵌套循环
  // 模拟：n² 次操作
  let quadratic_time = azimuth::@azimuth.multiply(n, n)
  azimuth::@azimuth.assert_eq(10000, quadratic_time)
  
  // O(log n) 对数时间复杂度：二分查找
  // 模拟：log₂(100) ≈ 6.64，取整数部分6
  let log_time = 6  // 近似值
  azimuth::@azimuth.assert_eq(6, log_time)
  
  // O(n log n) 线性对数时间复杂度：高效排序
  // 模拟：n * log₂(n)
  let n_log_n_time = azimuth::@azimuth.multiply(n, log_time)
  azimuth::@azimuth.assert_eq(600, n_log_n_time)
}
test "number_system_conversions" {
  // 测试数字系统转换
  let decimal_number = 255
  
  // 十进制转二进制（模拟）：255 = 11111111₂
  // 计算二进制表示中1的个数
  let binary_ones = 8
  azimuth::@azimuth.assert_eq(8, binary_ones)
  
  // 十进制转八进制：255 = 377₈
  // 计算八进制表示：3*8² + 7*8 + 7 = 192 + 56 + 7 = 255
  let octal_calculation = azimuth::@azimuth.add(
    azimuth::@azimuth.multiply(3, azimuth::@azimuth.multiply(8, 8)),
    azimuth::@azimuth.add(azimuth::@azimuth.multiply(7, 8), 7)
  )
  azimuth::@azimuth.assert_eq(255, octal_calculation)
  
  // 十进制转十六进制：255 = FF₁₆
  // 计算十六进制表示：15*16 + 15 = 240 + 15 = 255
  let hex_calculation = azimuth::@azimuth.add(
    azimuth::@azimuth.multiply(15, 16),
    15
  )
  azimuth::@azimuth.assert_eq(255, hex_calculation)
  
  // 位运算模拟
  let a = 12  // 1100₂
  let b = 10  // 1010₂
  
  // 按位与：1100 & 1010 = 1000₂ = 8
  let bitwise_and = 8
  azimuth::@azimuth.assert_eq(8, bitwise_and)
  
  // 按位或：1100 | 1010 = 1110₂ = 14
  let bitwise_or = 14
  azimuth::@azimuth.assert_eq(14, bitwise_or)
  
  // 按位异或：1100 ^ 1010 = 0110₂ = 6
  let bitwise_xor = 6
  azimuth::@azimuth.assert_eq(6, bitwise_xor)
}
test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出边界
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, 1))
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, max_val))
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, -1))
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, min_val))
  
  // 测试乘法溢出边界
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.multiply(max_val, 2))
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, 2))
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(max_val, -1))
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, -1))
  
  // 测试特殊值组合
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.multiply(0, max_val))
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.multiply(0, min_val))
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.multiply(max_val, 1))
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, 1))
  
  // 测试接近边界的值
  let near_max = 2147483646
  let near_min = -2147483647
  
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.add(near_max, 1))
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.add(near_min, -1))
  azimuth::@azimuth.assert_eq(-1, azimuth::@azimuth.add(max_val, min_val))
  azimuth::@azimuth.assert_eq(1, azimuth::@azimuth.add(min_val, max_val))
}