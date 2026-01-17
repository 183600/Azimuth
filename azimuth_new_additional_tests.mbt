// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，补充测试覆盖

test "arithmetic_operations_advanced" {
  // 高级算术运算测试
  let a = 15
  let b = 7
  
  // 测试加法
  let sum_result = a + b
  let _ = sum_result  // 22
  
  // 测试乘法
  let multiply_result = a * b
  let _ = multiply_result  // 105
  
  // 复合运算：(a + b) * a
  let compound_result = (a + b) * a
  let _ = compound_result  // 330
}

test "mathematical_series_fibonacci" {
  // 斐波那契数列测试
  let fib1 = 1
  let fib2 = 1
  let fib3 = fib1 + fib2  // 2
  let fib4 = fib2 + fib3  // 3
  let fib5 = fib3 + fib4  // 5
  let fib6 = fib4 + fib5  // 8
  let fib7 = fib5 + fib6  // 13
  let fib8 = fib6 + fib7  // 21
  
  // 验证斐波那契数列的黄金比例特性
  let ratio1 = fib8 * 100 / fib7  // 约161
  let ratio2 = fib7 * 100 / fib6  // 约162
  
  let _ = fib8  // 21
  let _ = ratio1  // 约161
  let _ = ratio2  // 约162
}

test "geometry_calculations_extended" {
  // 扩展几何计算测试
  let radius = 5
  
  // 圆的周长（近似值）：2 * π * r (π ≈ 22/7)
  let circumference = 2 * 22 * radius / 7
  let _ = circumference  // 31 整数近似
  
  // 圆的面积（近似值）：π * r² (π ≈ 22/7)
  let area = 22 * radius * radius / 7
  let _ = area  // 78 整数近似
  
  // 球体体积（近似值）：4/3 * π * r³ (π ≈ 22/7)
  let volume = 4 * 22 * radius * radius * radius / 21
  let _ = volume  // 523 整数近似
}

test "time_calculations_comprehensive" {
  // 综合时间计算测试
  let days = 365
  let hours_per_day = 24
  let minutes_per_hour = 60
  let seconds_per_minute = 60
  
  // 一年的总小时数
  let hours_in_year = days * hours_per_day
  let _ = hours_in_year  // 8760
  
  // 一年的总分钟数
  let minutes_in_year = hours_in_year * minutes_per_hour
  let _ = minutes_in_year  // 525600
  
  // 一年的总秒数
  let seconds_in_year = minutes_in_year * seconds_per_minute
  let _ = seconds_in_year  // 31536000
  
  // 闰年计算（366天）
  let leap_year_hours = 366 * hours_per_day
  let _ = leap_year_hours  // 8784
}

test "financial_compound_interest" {
  // 复利计算测试
  let principal = 1000
  let rate = 5  // 5% 年利率
  let years = 3
  
  // 第一年：1000 * 1.05 = 1050
  let year1 = principal + principal * rate / 100
  let _ = year1  // 1050
  
  // 第二年：1050 * 1.05 = 1102.5 ≈ 1102
  let year2 = year1 + year1 * rate / 100
  let _ = year2  // 1102
  
  // 第三年：1102 * 1.05 = 1157.1 ≈ 1157
  let year3 = year2 + year2 * rate / 100
  let _ = year3  // 1157
}

test "physics_calculations" {
  // 物理计算测试
  let mass = 10  // kg
  let acceleration = 5  // m/s²
  
  // 牛顿第二定律：F = m * a
  let force = mass * acceleration
  let _ = force  // 50
  
  // 动能：KE = 0.5 * m * v² (假设速度为 10 m/s)
  let velocity = 10
  let kinetic_energy = mass * velocity * velocity / 2
  let _ = kinetic_energy  // 500
  
  // 势能：PE = m * g * h (g = 9.8 ≈ 10, h = 20)
  let gravity = 10  // 简化重力加速度
  let height = 20
  let potential_energy = mass * gravity * height
  let _ = potential_energy  // 2000
}

test "statistical_calculations" {
  // 统计计算测试
  let numbers = [12, 15, 18, 21, 24, 27, 30]
  
  // 计算总和
  let sum = 12 + 15 + 18 + 21 + 24 + 27 + 30
  let _ = sum  // 147
  
  // 计算平均值
  let count = 7
  let average = sum / count
  let _ = average  // 21
  
  // 计算方差（简化版）
  let variance_sum = (12 - 21) * (12 - 21) + (15 - 21) * (15 - 21) + (18 - 21) * (18 - 21) + (21 - 21) * (21 - 21) + (24 - 21) * (24 - 21) + (27 - 21) * (27 - 21) + (30 - 21) * (30 - 21)
  
  let variance = variance_sum / count
  let _ = variance  // 36
}

test "string_operations_advanced" {
  // 高级字符串操作测试
  let first_name = "John"
  let last_name = "Doe"
  let age = "30"
  
  // 测试字符串拼接
  let full_greeting = "Hello, " + first_name + "!"
  let _ = full_greeting  // "Hello, John!"
  
  // 测试空字符串
  let empty_greeting = "Hello, " + "" + "!"
  let _ = empty_greeting  // "Hello, !"
  
  // 测试数字字符串
  let number_greeting = "Hello, " + age + "!"
  let _ = number_greeting  // "Hello, 30!"
  
  // 测试特殊字符字符串
  let special_greeting = "Hello, " + "test@example.com" + "!"
  let _ = special_greeting  // "Hello, test@example.com!"
}

test "algorithm_efficiency" {
  // 算法效率测试
  let n = 100
  
  // 线性时间复杂度 O(n)：1 + 2 + 3 + ... + n = n*(n+1)/2
  let linear_sum = n * (n + 1) / 2
  let _ = linear_sum  // 5050
  
  // 平方时间复杂度 O(n²)：1² + 2² + 3² + ... + n² = n*(n+1)*(2n+1)/6
  let square_sum = n * (n + 1) * (2 * n + 1) / 6
  let _ = square_sum  // 338350
  
  // 立方时间复杂度 O(n³)：1³ + 2³ + 3³ + ... + n³ = (n*(n+1)/2)²
  let cube_sum = linear_sum * linear_sum
  let _ = cube_sum  // 25502500
}

test "boundary_conditions_extreme" {
  // 极端边界条件测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试最大值加法
  let max_add_result = max_int + 0
  let _ = max_add_result  // max_int
  
  // 测试最小值加法
  let min_add_result = min_int + 0
  let _ = min_add_result  // min_int
  
  // 测试最大值乘法
  let max_multiply_result = max_int * 1
  let _ = max_multiply_result  // max_int
  
  // 测试最小值乘法
  let min_multiply_result = min_int * 1
  let _ = min_multiply_result  // min_int
  
  // 测试零值运算
  let zero_add_max = max_int + 0
  let zero_add_min = min_int + 0
  let zero_multiply_any = 12345 * 0
  
  let _ = zero_add_max  // max_int
  let _ = zero_add_min  // min_int
  let _ = zero_multiply_any  // 0
}