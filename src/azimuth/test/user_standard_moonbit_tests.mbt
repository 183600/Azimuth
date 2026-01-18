// 用户请求的标准 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "statistical_calculations" {
  // 统计计算测试：计算平均值、中位数、众数
  // 计算五个数的平均值
  let numbers_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  let average = numbers_sum / 5
  @azimuth.assert_eq(30, average)
  
  // 计算加权平均：(10*2 + 20*3 + 30*5) / (2+3+5)
  let weighted_sum = @azimuth.add(@azimuth.add(@azimuth.multiply(10, 2), @azimuth.multiply(20, 3)), @azimuth.multiply(30, 5))
  let total_weight = @azimuth.add(@azimuth.add(2, 3), 5)
  let weighted_average = weighted_sum / total_weight
  @azimuth.assert_eq(23, weighted_average)
}

test "geometric_calculations" {
  // 几何计算测试：面积、周长、体积
  // 矩形面积：长 × 宽
  let length = 12
  let width = 8
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
  
  // 梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 5
  let bottom_base = 15
  let trapezoid_height = 8
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), trapezoid_height) / 2
  @azimuth.assert_eq(80, trapezoid_area)
}

test "financial_calculations" {
  // 金融计算测试：利息、折扣、税收
  // 简单利息：本金 × 利率 × 时间
  let principal = 10000
  let rate = 5  // 5%
  let time = 3  // 3年
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), time) / 100
  @azimuth.assert_eq(1500, interest)
  
  // 折扣计算：原价 × 折扣率
  let original_price = 500
  let discount_rate = 20  // 20%
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(400, final_price)
  
  // 复合税收：基础税 + 额外税
  let base_amount = 1000
  let base_tax_rate = 10  // 10%
  let additional_tax_rate = 5  // 5%
  let base_tax = @azimuth.multiply(base_amount, base_tax_rate) / 100
  let additional_tax = @azimuth.multiply(base_amount, additional_tax_rate) / 100
  let total_tax = @azimuth.add(base_tax, additional_tax)
  @azimuth.assert_eq(150, total_tax)
}

test "physics_calculations" {
  // 物理计算测试：速度、加速度、能量
  // 平均速度：距离 ÷ 时间
  let distance = 300
  let time = 5
  let average_speed = distance / time
  @azimuth.assert_eq(60, average_speed)
  
  // 加速度：(末速度 - 初速度) ÷ 时间
  let final_velocity = 80
  let initial_velocity = 20
  let acceleration_time = 3
  let acceleration = @azimuth.add(final_velocity, -initial_velocity) / acceleration_time
  @azimuth.assert_eq(20, acceleration)
  
  // 动能：质量 × 速度² ÷ 2
  let mass = 10
  let velocity = 15
  let kinetic_energy = @azimuth.multiply(@azimuth.multiply(mass, velocity), velocity) / 2
  @azimuth.assert_eq(1125, kinetic_energy)
}

test "temperature_conversion" {
  // 温度转换测试：摄氏度、华氏度、开尔文
  // 摄氏度转华氏度（简化公式）：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度（简化公式）：C = (F - 30) ÷ 2
  let fahrenheit_temp = 100
  let celsius_temp = @azimuth.add(fahrenheit_temp, -30) / 2
  @azimuth.assert_eq(35, celsius_temp)
  
  // 开尔文转摄氏度（简化公式）：K = C + 273
  let celsius_for_kelvin = 20
  let kelvin = @azimuth.add(celsius_for_kelvin, 273)
  @azimuth.assert_eq(293, kelvin)
}

test "data_structure_operations" {
  // 数据结构操作测试：数组索引、栈操作、队列操作
  // 二维数组转一维数组索引计算
  let row = 3
  let col = 4
  let width = 5
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(19, index)
  
  // 栈操作模拟：push + pop
  let stack_size = 0
  let push_operations = 5
  let pop_operations = 2
  let final_stack_size = @azimuth.add(@azimuth.multiply(1, push_operations), @azimuth.multiply(-1, pop_operations))
  @azimuth.assert_eq(3, final_stack_size)
  
  // 循环队列索引计算
  let queue_size = 10
  let current_index = 8
  let enqueue_count = 5
  let new_index = @azimuth.add(current_index, enqueue_count) % queue_size
  @azimuth.assert_eq(3, new_index)
}

test "algorithm_complexity" {
  // 算法复杂度测试：线性、二次、对数
  // 线性复杂度：O(n)
  let n = 100
  let linear_operations = @azimuth.multiply(n, 1)
  @azimuth.assert_eq(100, linear_operations)
  
  // 二次复杂度：O(n²)
  let quadratic_operations = @azimuth.multiply(n, n)
  @azimuth.assert_eq(10000, quadratic_operations)
  
  // 对数复杂度模拟（简化）：O(log n)
  // 使用整数除法模拟对数运算
  let log_operations = 0
  let temp_n = n
  let log_approx = 0
  // 简化的对数计算：100 -> 50 -> 25 -> 12 -> 6 -> 3 -> 1 (约7次)
  @azimuth.assert_eq(7, log_approx)
}

test "string_encoding_processing" {
  // 字符串编码处理测试
  // 测试不同字符编码的字符串
  let ascii_string = "Hello"
  @azimuth.assert_eq_string("Hello, Hello!", @azimuth.greet(ascii_string))
  
  let unicode_string = "你好"
  @azimuth.assert_eq_string("Hello, 你好!", @azimuth.greet(unicode_string))
  
  let emoji_string = "🌟"
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet(emoji_string))
  
  let mixed_string = "Test123测试!@#"
  @azimuth.assert_eq_string("Hello, Test123测试!@#!", @azimuth.greet(mixed_string))
}

test "number_system_conversions" {
  // 数字系统转换测试
  // 十进制转二进制（模拟）
  let decimal = 13
  // 13 = 8 + 4 + 1 = 1101(二进制)
  let binary_8 = @azimuth.multiply(1, 8)
  let binary_4 = @azimuth.multiply(1, 4)
  let binary_1 = @azimuth.multiply(1, 1)
  let binary_sum = @azimuth.add(@azimuth.add(binary_8, binary_4), binary_1)
  @azimuth.assert_eq(13, binary_sum)
  
  // 十进制转八进制（模拟）
  // 13 = 1*8 + 5 = 15(八进制)
  let octal_8 = @azimuth.multiply(1, 8)
  let octal_5 = @azimuth.multiply(5, 1)
  let octal_sum = @azimuth.add(octal_8, octal_5)
  @azimuth.assert_eq(13, octal_sum)
  
  // 十进制转十六进制（模拟）
  // 13 = D(十六进制)
  let hex_10 = @azimuth.multiply(0, 16)
  let hex_3 = @azimuth.multiply(13, 1)
  let hex_sum = @azimuth.add(hex_10, hex_3)
  @azimuth.assert_eq(13, hex_sum)
}

test "combinatorial_calculations" {
  // 组合数学计算测试
  // 排列数（简化）：P(n,k) = n! / (n-k)!
  // 计算 P(5,2) = 5! / 3! = 120 / 6 = 20
  let n = 5
  let k = 2
  let n_factorial = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(5, 4), 3), 2), 1)
  let n_minus_k_factorial = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(3, 2), 1), 1)
  let permutation = n_factorial / n_minus_k_factorial
  @azimuth.assert_eq(20, permutation)
  
  // 组合数（简化）：C(n,k) = n! / (k! * (n-k)!)
  // 计算 C(5,2) = 5! / (2! * 3!) = 120 / (2 * 6) = 10
  let k_factorial = @azimuth.multiply(@azimuth.multiply(2, 1), 1)
  let combination = n_factorial / @azimuth.multiply(k_factorial, n_minus_k_factorial)
  @azimuth.assert_eq(10, combination)
  
  // 斐波那契数列（前10项和）
  // 1, 1, 2, 3, 5, 8, 13, 21, 34, 55
  let fib_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 1), 2), 3), 5), 8), 13), 21), 34), 55)
  @azimuth.assert_eq(143, fib_sum)
}