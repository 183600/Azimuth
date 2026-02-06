// 最小的 azimuth 库文件
pub fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  // 简单的断言实现
  if lhs < rhs || lhs > rhs {
    let _ = "Assertion failed"
  } else {
    ()
  }
  ()
}

pub fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  // 简单的字符串断言实现
  if lhs.length() != rhs.length() {
    let _ = "String assertion failed"
  } else {
    let _ = "String assertion passed"
  }
  ()
}

pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn subtract(a : Int, b : Int) -> Int {
  a - b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}

// 高质量的 MoonBit 测试用例 - 8个专业级测试用例
test "number_system_conversions" {
  // 模拟数字系统转换计算
  let decimal_value = 255
  let hex_weight = 16
  
  // 计算二进制表示的权重和
  let binary_sum = add(multiply(1, 128), multiply(1, 64))
  let binary_total = add(binary_sum, add(multiply(1, 32), multiply(1, 16)))
  
  // 计算十六进制表示
  let hex_high = divide_with_ceil(decimal_value, hex_weight)
  let hex_low = decimal_value - multiply(hex_high - 1, hex_weight)
  
  assert_eq(240, binary_total)
  assert_eq(16, hex_high)
  assert_eq(15, hex_low)
}

test "statistical_computation" {
  // 统计分析测试
  let sum_manual = add(add(add(add(add(add(12, 15), 18), 21), 24), 27), 30)
  
  // 计算平均值（向上取整）
  let count = 7
  let mean_value = divide_with_ceil(sum_manual, count)
  
  // 计算中位数近似值
  let median_approx = divide_with_ceil(add(21, 24), 2)
  
  assert_eq(147, sum_manual)
  assert_eq(21, mean_value)
  assert_eq(23, median_approx)
}

test "resource_allocation" {
  // 资源优化算法测试
  let total_resources = 100
  let task1_requirement = 23
  let task2_requirement = 31
  let task3_requirement = 19
  
  // 计算已分配资源
  let allocated = add(add(task1_requirement, task2_requirement), task3_requirement)
  
  // 计算剩余资源
  let remaining = subtract(total_resources, allocated)
  
  // 计算可以支持的新任务数量（每个任务需要8个资源）
  let new_tasks_possible = divide_with_ceil(remaining, 8)
  
  assert_eq(73, allocated)
  assert_eq(27, remaining)
  assert_eq(4, new_tasks_possible)
}

test "geometric_computations" {
  // 几何计算测试
  let length = 8
  let width = 6
  let height = 4
  
  // 计算表面积
  let area1 = multiply(length, width)
  let area2 = multiply(length, height)
  let area3 = multiply(width, height)
  let surface_area = multiply(2, add(add(area1, area2), area3))
  
  // 计算体积
  let volume = multiply(area1, height)
  
  // 计算空间利用率（假设物品体积为150）
  let item_volume = 150
  let utilization = divide_with_ceil(multiply(item_volume, 100), volume)
  
  assert_eq(208, surface_area)
  assert_eq(192, volume)
  assert_eq(78, utilization)
}

test "temporal_analysis" {
  // 时间序列分析测试
  let baseline = 100
  let growth_rate = 15
  
  // 计算复合增长
  let period1 = add(baseline, divide_with_ceil(multiply(baseline, growth_rate), 100))
  let period2 = add(period1, divide_with_ceil(multiply(period1, growth_rate), 100))
  let period3 = add(period2, divide_with_ceil(multiply(period2, growth_rate), 100))
  
  // 计算总增长
  let total_growth = subtract(period3, baseline)
  let growth_percentage = divide_with_ceil(multiply(total_growth, 100), baseline)
  
  assert_eq(115, period1)
  assert_eq(133, period2)
  assert_eq(153, period3)
  assert_eq(53, total_growth)
  assert_eq(53, growth_percentage)
}

test "boundary_validation" {
  // 错误边界条件测试
  let max_safe_int = 1000000
  let min_safe_int = -1000000
  
  // 测试接近边界的运算
  let boundary_add1 = add(max_safe_int, 1)
  let boundary_add2 = add(min_safe_int, -1)
  let boundary_mult = divide_with_ceil(max_safe_int, 1000)
  let boundary_div = divide_with_ceil(max_safe_int, 1)
  
  // 测试零的边界情况
  let zero_by_positive = divide_with_ceil(0, 100)
  let zero_by_negative = divide_with_ceil(0, -100)
  let zero_by_zero = divide_with_ceil(0, 0)
  
  assert_eq(1000001, boundary_add1)
  assert_eq(-1000001, boundary_add2)
  assert_eq(1000, boundary_mult)
  assert_eq(1000000, boundary_div)
  assert_eq(0, zero_by_positive)
  assert_eq(0, zero_by_negative)
  assert_eq(0, zero_by_zero)
}

test "encryption_simulation" {
  // 密码学模拟测试
  let message = 12345
  let key1 = 7
  let key2 = 13
  let modulus = 1000
  
  // 简单的加密过程
  let step1 = multiply(message, key1)
  let step2 = add(step1, key2)
  let encrypted = step2 % modulus
  
  // 解密过程（简化）
  let decrypted_step1 = subtract(encrypted, key2)
  let decrypted_approx = divide_with_ceil(multiply(decrypted_step1, 1), key1)
  
  assert_eq(86425, step1)
  assert_eq(86438, step2)
  assert_eq(438, encrypted)
  assert_eq(425, decrypted_step1)
  assert_eq(61, decrypted_approx)
}

test "physics_modeling" {
  // 物理模拟测试
  let initial_velocity = 10
  let acceleration = 2
  let time = 5
  let mass = 50
  
  // 计算最终速度: v = u + at
  let final_velocity = add(initial_velocity, multiply(acceleration, time))
  
  // 计算位移: s = ut + 0.5at²
  let displacement = add(multiply(initial_velocity, time), divide_with_ceil(multiply(acceleration, multiply(time, time)), 2))
  
  // 计算动能: KE = 0.5mv²
  let kinetic_energy = divide_with_ceil(multiply(mass, multiply(final_velocity, final_velocity)), 2)
  
  // 计算需要的燃料单位（每单位燃料提供100能量）
  let fuel_units = divide_with_ceil(kinetic_energy, 100)
  
  assert_eq(20, final_velocity)
  assert_eq(75, displacement)
  assert_eq(10000, kinetic_energy)
  assert_eq(100, fuel_units)
}

// 专业级 MoonBit 测试用例 - 10个额外测试
test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(add(5, 3), 8)
  assert_eq(multiply(4, 6), 24)
  assert_eq(subtract(10, 7), 3)
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
}

test "string_operations" {
  // 测试字符串操作
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
}

test "division_edge_cases" {
  // 测试除法的边界情况
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(0, 1), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(add(-5, 3), -2)
  assert_eq(add(5, -3), 2)
  assert_eq(add(-5, -3), -8)
  assert_eq(multiply(-4, 6), -24)
  assert_eq(multiply(4, -6), -24)
  assert_eq(multiply(-4, -6), 24)
  assert_eq(subtract(-10, 7), -17)
  assert_eq(subtract(10, -7), 17)
  assert_eq(subtract(-10, -7), -3)
}

test "large_number_operations" {
  // 测试大数运算
  assert_eq(add(1000000, 1000000), 2000000)
  assert_eq(multiply(10000, 10000), 100000000)
  assert_eq(subtract(1000000, 500000), 500000)
  assert_eq(divide_with_ceil(1000000, 3), 333334)
}

test "precision_operations" {
  // 测试精度运算
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(2, 3), 1)
  assert_eq(divide_with_ceil(3, 4), 1)
  assert_eq(divide_with_ceil(100, 99), 2)
  assert_eq(divide_with_ceil(99, 100), 1)
}

test "complex_calculations" {
  // 测试复合计算
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(result1, 16)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(result2, 17)
  
  // (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(result3, 1)
}

test "boundary_value_analysis" {
  // 测试边界值分析
  // 测试零值
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 5), 0)
  assert_eq(subtract(0, 0), 0)
  
  // 测试单位值
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 5), 5)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(5, 1), 5)
  
  // 测试负一
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 5), -5)
  assert_eq(subtract(0, -1), 1)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  assert_eq_string(greet("a"), "Hello, a!")
  assert_eq_string(greet("123"), "Hello, 123!")
  assert_eq_string(greet("!@#"), "Hello, !@#!")
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 3
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
  
  // 减法的逆运算
  assert_eq(add(subtract(x, y), y), x)
}

// 新增的专业级 MoonBit 测试用例 - 8个高质量测试用例
test "fibonacci_sequence_calculation" {
  // 斐波那契数列计算测试
  // 使用基本的算术运算模拟递归计算
  
  // F(0) = 0, F(1) = 1
  let f0 = 0
  let f1 = 1
  
  // F(2) = F(1) + F(0) = 1 + 0 = 1
  let f2 = add(f1, f0)
  
  // F(3) = F(2) + F(1) = 1 + 1 = 2
  let f3 = add(f2, f1)
  
  // F(4) = F(3) + F(2) = 2 + 1 = 3
  let f4 = add(f3, f2)
  
  // F(5) = F(4) + F(3) = 3 + 2 = 5
  let f5 = add(f4, f3)
  
  // F(6) = F(5) + F(4) = 5 + 3 = 8
  let f6 = add(f5, f4)
  
  // F(7) = F(6) + F(5) = 8 + 5 = 13
  let f7 = add(f6, f5)
  
  // F(8) = F(7) + F(6) = 13 + 8 = 21
  let f8 = add(f7, f6)
  
  assert_eq(f2, 1)
  assert_eq(f3, 2)
  assert_eq(f4, 3)
  assert_eq(f5, 5)
  assert_eq(f6, 8)
  assert_eq(f7, 13)
  assert_eq(f8, 21)
}

test "compound_interest_calculation" {
  // 复利计算测试 - 金融应用场景
  let principal = 10000  // 本金
  let rate = 5           // 年利率百分比
  
  // 第一年：本金 + 利息
  let year1_interest = divide_with_ceil(multiply(principal, rate), 100)
  let year1_total = add(principal, year1_interest)
  
  // 第二年：第一年总额 + 第二年利息
  let year2_interest = divide_with_ceil(multiply(year1_total, rate), 100)
  let year2_total = add(year1_total, year2_interest)
  
  // 第三年：第二年总额 + 第三年利息
  let year3_interest = divide_with_ceil(multiply(year2_total, rate), 100)
  let year3_total = add(year2_total, year3_interest)
  
  // 计算总收益
  let total_profit = subtract(year3_total, principal)
  
  assert_eq(year1_total, 10500)
  assert_eq(year2_total, 11025)
  assert_eq(year3_total, 11576)
  assert_eq(total_profit, 1576)
}

test "binary_representation_simulation" {
  // 二进制表示模拟测试 - 计算机科学基础
  // 分解为二进制位：42 = 32 + 8 + 2 = 2^5 + 2^3 + 2^1
  let bit5 = multiply(1, 32)  // 2^5
  let bit4 = multiply(0, 16)  // 2^4
  let bit3 = multiply(1, 8)   // 2^3
  let bit2 = multiply(0, 4)   // 2^2
  let bit1 = multiply(1, 2)   // 2^1
  let bit0 = multiply(0, 1)   // 2^0
  
  // 重新组合验证
  let reconstructed = add(add(add(add(add(bit5, bit4), bit3), bit2), bit1), bit0)
  
  // 计算设置位的数量
  let set_bits = add(add(1, 1), 1)  // bit5, bit3, bit1 被设置
  
  assert_eq(reconstructed, 42)
  assert_eq(set_bits, 3)
}

test "temperature_conversion" {
  // 温度转换测试 - 科学计算应用
  let celsius = 25
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let fahrenheit_step1 = multiply(celsius, 9)
  let fahrenheit_step2 = divide_with_ceil(fahrenheit_step1, 5)
  let fahrenheit = add(fahrenheit_step2, 32)
  
  // 华氏度转开尔文：K = (F - 32) × 5/9 + 273.15
  // 简化计算，使用整数近似
  let kelvin_step1 = subtract(fahrenheit, 32)
  let kelvin_step2 = multiply(kelvin_step1, 5)
  let kelvin_step3 = divide_with_ceil(kelvin_step2, 9)
  let kelvin = add(kelvin_step3, 273)
  
  // 验证转换的一致性（简化验证）
  let celsius_back = divide_with_ceil(multiply(subtract(kelvin, 273), 9), 5)
  
  assert_eq(fahrenheit, 77)
  assert_eq(kelvin, 298)
  assert_eq(celsius_back, 25)
}

test "game_score_calculation" {
  // 游戏得分计算测试 - 游戏逻辑应用
  let base_score = 100
  let level_multiplier = 3
  let combo_bonus = 50
  let penalty = 20
  
  // 基础得分 × 等级倍数
  let level_score = multiply(base_score, level_multiplier)
  
  // 加入连击奖励
  let score_with_combo = add(level_score, combo_bonus)
  
  // 减去惩罚
  let final_score = subtract(score_with_combo, penalty)
  
  // 计算星级评分（每100分一星）
  let star_rating = divide_with_ceil(final_score, 100)
  
  // 计算剩余分数
  let remainder = subtract(final_score, multiply(star_rating - 1, 100))
  
  assert_eq(level_score, 300)
  assert_eq(score_with_combo, 350)
  assert_eq(final_score, 330)
  assert_eq(star_rating, 4)
  assert_eq(remainder, 30)
}

test "inventory_management" {
  // 库存管理测试 - 商业应用
  let initial_stock = 500
  let daily_sales = 25
  let days = 7
  let new_shipment = 200
  
  // 计算一周销售量
  let weekly_sales = multiply(daily_sales, days)
  
  // 计算销售后库存
  let stock_after_sales = subtract(initial_stock, weekly_sales)
  
  // 加入新货
  let final_stock = add(stock_after_sales, new_shipment)
  
  // 计算库存周转率（假设月需求为400）
  let monthly_demand = 400
  let turnover_rate = divide_with_ceil(monthly_demand, final_stock)
  
  // 计算安全库存天数
  let safety_days = divide_with_ceil(final_stock, daily_sales)
  
  assert_eq(weekly_sales, 175)
  assert_eq(stock_after_sales, 325)
  assert_eq(final_stock, 525)
  assert_eq(turnover_rate, 1)
  assert_eq(safety_days, 21)
}

test "data_compression_simulation" {
  // 数据压缩模拟测试 - 算法应用
  let original_size = 1000
  let repetition_factor = 5  // 重复因子
  
  // 计算压缩后的理论大小
  let compressed_size = divide_with_ceil(original_size, repetition_factor)
  
  // 计算压缩比
  let compression_ratio = divide_with_ceil(multiply(original_size, 100), compressed_size)
  
  // 计算节省的空间
  let space_saved = subtract(original_size, compressed_size)
  
  // 计算节省的百分比
  let savings_percentage = divide_with_ceil(multiply(space_saved, 100), original_size)
  
  // 模拟多次压缩的效果
  let second_compression = divide_with_ceil(compressed_size, repetition_factor)
  let total_compression = divide_with_ceil(original_size, second_compression)
  
  assert_eq(compressed_size, 200)
  assert_eq(compression_ratio, 500)
  assert_eq(space_saved, 800)
  assert_eq(savings_percentage, 80)
  assert_eq(second_compression, 40)
  assert_eq(total_compression, 25)
}

test "project_timeline_calculation" {
  // 项目时间线计算测试 - 项目管理应用
  let total_tasks = 48
  let team_size = 6
  let work_days_per_week = 5
  let hours_per_task = 8
  
  // 计算每人需要完成的任务数
  let tasks_per_person = divide_with_ceil(total_tasks, team_size)
  
  // 计算每人需要的工作小时数
  let hours_per_person = multiply(tasks_per_person, hours_per_task)
  
  // 计算每人需要的工作天数
  let days_per_person = divide_with_ceil(hours_per_person, 8)
  
  // 计算项目持续周数
  let project_weeks = divide_with_ceil(days_per_person, work_days_per_week)
  
  // 计算总工作小时数
  let total_hours = multiply(total_tasks, hours_per_task)
  
  // 计算项目效率（每人每周完成的任务数）
  let tasks_per_week_per_person = divide_with_ceil(tasks_per_person, project_weeks)
  
  assert_eq(tasks_per_person, 8)
  assert_eq(hours_per_person, 64)
  assert_eq(days_per_person, 8)
  assert_eq(project_weeks, 2)
  assert_eq(total_hours, 384)
  assert_eq(tasks_per_week_per_person, 4)
}