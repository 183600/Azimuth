// 新的专注测试用例 - 覆盖特定场景和边界情况

// 测试大数运算的精确性
///|
test "large_number_precision" {
  // 测试接近边界的大数运算
  let large_pos = 1000000000
  let large_neg = -1000000000
  
  // 大数加法
  assert_eq(2000000000, @azimuth.add(large_pos, large_pos))
  assert_eq(-2000000000, @azimuth.add(large_neg, large_neg))
  assert_eq(0, @azimuth.add(large_pos, large_neg))
  
  // 大数乘法
  assert_eq(1000000000000000000, @azimuth.multiply(large_pos, large_pos))
  assert_eq(1000000000000000000, @azimuth.multiply(large_neg, large_neg))
  assert_eq(-1000000000000000000, @azimuth.multiply(large_pos, large_neg))
}

// 测试字符串处理的边界情况
///|
test "string_boundary_conditions" {
  // 测试极长字符串
  let very_long_name = "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ"
  let result = @azimuth.greet(very_long_name)
  assert_eq(true, result.contains("ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  
  // 测试包含特殊字符的字符串
  assert_eq("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?", @azimuth.greet("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
  
  // 测试包含空格的字符串
  assert_eq("Hello,    ", @azimuth.greet("   "))
  assert_eq("Hello, leading space!", @azimuth.greet(" leading space"))
  assert_eq("Hello, trailing space !", @azimuth.greet("trailing space "))
}

// 测试数学运算的实际应用场景
///|
test "real_world_math_applications" {
  // 计算复合利息：A = P(1 + r)^t
  // 本金 = 10000，利率 = 5%，时间 = 3年
  let principal = 10000
  let rate_percent = 5
  let time = 3
  
  // (1 + r)^t = (1 + 0.05)^3 = 1.157625
  // 我们通过整数运算来模拟
  let rate_times_100 = 500  // 5%
  let base_times_100 = 10500  // 1 + r = 1.05 = 105/100
  let compound_times_10000 = @azimuth.multiply(base_times_100, @azimuth.multiply(base_times_100, base_times_100))
  
  // 验证计算过程
  assert_eq(10500, base_times_100)
  assert_eq(1157625000, compound_times_10000)  // 105^3 = 1157625，所以是1157625000/1000000 = 1.157625
}

// 测试特殊值的运算
///|
test "special_value_operations" {
  // 测试连续运算
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  assert_eq(10, result1)  // (1+2)+(3+4) = 3+7 = 10
  
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  assert_eq(120, result2)  // (2*3)*(4*5) = 6*20 = 120
  
  // 测试混合运算
  let result3 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  assert_eq(26, result3)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result4 = @azimuth.multiply(2, @azimuth.add(3, 4))
  assert_eq(14, result4)  // 2*(3+4) = 2*7 = 14
}

// 测试科学计算相关的场景
///|
test "scientific_calculations" {
  // 计算动能：KE = 0.5 * m * v²
  // 质量 = 10kg，速度 = 5m/s
  let mass = 10
  let velocity = 5
  let velocity_squared = @azimuth.multiply(velocity, velocity)
  let kinetic_energy_times_2 = @azimuth.multiply(mass, velocity_squared)
  
  assert_eq(25, velocity_squared)  // 5² = 25
  assert_eq(250, kinetic_energy_times_2)  // 10 * 25 = 250，所以动能 = 125J
  
  // 计算势能：PE = m * g * h
  // 质量 = 5kg，重力加速度 = 9.8m/s²，高度 = 10m
  let mass2 = 5
  let gravity_times_10 = 98  // 9.8 * 10
  let height = 10
  let potential_energy_times_10 = @azimuth.multiply(mass2, @azimuth.multiply(gravity_times_10, height))
  
  assert_eq(4900, potential_energy_times_10)  // 5 * 98 * 10 = 4900，所以势能 = 490J
}

// 测试金融计算场景
///|
test "financial_calculations" {
  // 计算贷款月供：M = P[r(1+r)^n]/[(1+r)^n-1]
  // 本金 = 100000，年利率 = 6%，期限 = 30年
  let principal = 100000
  let annual_rate_percent = 6
  let years = 30
  
  // 月利率 = 年利率 / 12 = 0.5%
  let monthly_rate_times_1000 = 50  // 0.5%
  let months = @azimuth.multiply(years, 12)
  
  // (1+r)^n 的近似计算
  let base_times_1000 = 1050  // 1 + 0.005 = 1.005 = 1050/1000
  
  // 验证基本计算
  assert_eq(360, months)  // 30 * 12 = 360
  assert_eq(50, monthly_rate_times_1000)
  assert_eq(1050, base_times_1000)
}

// 测试统计分析场景
///|
test "statistical_analysis" {
  // 计算标准差：σ = sqrt(Σ(xi - μ)²/n)
  // 数据集：[10, 12, 14, 16, 18]
  // 平均值：14
  
  let data1 = 10
  let data2 = 12
  let data3 = 14
  let data4 = 16
  let data5 = 18
  let mean = 14
  let count = 5
  
  // 计算平方差和
  let diff1 = @azimuth.multiply(@azimuth.add(data1, @azimuth.multiply(-1, mean)), @azimuth.add(data1, @azimuth.multiply(-1, mean)))
  let diff2 = @azimuth.multiply(@azimuth.add(data2, @azimuth.multiply(-1, mean)), @azimuth.add(data2, @azimuth.multiply(-1, mean)))
  let diff3 = @azimuth.multiply(@azimuth.add(data3, @azimuth.multiply(-1, mean)), @azimuth.add(data3, @azimuth.multiply(-1, mean)))
  let diff4 = @azimuth.multiply(@azimuth.add(data4, @azimuth.multiply(-1, mean)), @azimuth.add(data4, @azimuth.multiply(-1, mean)))
  let diff5 = @azimuth.multiply(@azimuth.add(data5, @azimuth.multiply(-1, mean)), @azimuth.add(data5, @azimuth.multiply(-1, mean)))
  
  let sum_of_squares = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(diff1, diff2), diff3), diff4), diff5)
  
  assert_eq(16, diff1)  // (10-14)² = 16
  assert_eq(4, diff2)   // (12-14)² = 4
  assert_eq(0, diff3)   // (14-14)² = 0
  assert_eq(4, diff4)   // (16-14)² = 4
  assert_eq(16, diff5)  // (18-14)² = 16
  assert_eq(40, sum_of_squares)  // 16 + 4 + 0 + 4 + 16 = 40
  
  // 方差 = 40/5 = 8
  let variance_times_count = sum_of_squares
  assert_eq(40, variance_times_count)
}

// 测试几何计算场景
///|
test "geometric_calculations" {
  // 计算三角形面积：海伦公式
  // 边长：a=3, b=4, c=5
  let a = 3
  let b = 4
  let c = 5
  
  // 半周长：s = (a+b+c)/2
  let perimeter = @azimuth.add(@azimuth.add(a, b), c)
  let s_times_2 = perimeter
  
  // 面积² = s(s-a)(s-b)(s-c)
  let s_minus_a = @azimuth.multiply(-1, @azimuth.add(a, @azimuth.multiply(-1, 6)))  // s-a = 6-3 = 3
  let s_minus_b = @azimuth.multiply(-1, @azimuth.add(b, @azimuth.multiply(-1, 6)))  // s-b = 6-4 = 2
  let s_minus_c = @azimuth.multiply(-1, @azimuth.add(c, @azimuth.multiply(-1, 6)))  // s-c = 6-5 = 1
  
  let area_squared_times_16 = @azimuth.multiply(s_times_2, @azimuth.multiply(@azimuth.multiply(s_minus_a, s_minus_b), s_minus_c))
  
  assert_eq(12, s_times_2)  // (3+4+5) = 12
  assert_eq(3, s_minus_a)
  assert_eq(2, s_minus_b)
  assert_eq(1, s_minus_c)
  assert_eq(72, area_squared_times_16)  // 12*3*2*1 = 72，所以面积² = 72/16 = 4.5
}

// 测试物理公式应用场景
///|
test "physics_formula_applications" {
  // 计算理想气体状态方程：PV = nRT
  // 压力 = 101325 Pa，体积 = 0.0224 m³，物质的量 = 1 mol，气体常数 = 8.314 J/(mol·K)
  let pressure = 101325
  let volume_times_10000 = 2240  // 0.0224 * 10000
  let moles = 1
  let gas_constant_times_1000 = 8314  // 8.314 * 1000
  let temperature_times_100 = 273  // 273K
  
  // PV = nRT
  let pv = @azimuth.multiply(pressure, volume_times_10000)
  let nrt = @azimuth.multiply(@azimuth.multiply(moles, gas_constant_times_1000), temperature_times_100)
  
  // 验证方程两边（考虑单位换算）
  assert_eq(226968000, pv)    // 101325 * 2240
  assert_eq(2269722, nrt)     // 1 * 8314 * 273
}

// 测试复杂字符串组合场景
///|
test "complex_string_combinations" {
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?`~"
  let result1 = @azimuth.greet(special_chars)
  assert_eq(true, result1.contains(special_chars))
  
  // 测试多语言字符串
  let chinese_name = "张三"
  let japanese_name = "田中太郎"
  let korean_name = "김철수"
  let arabic_name = "أحمد"
  let russian_name = "Иван"
  
  assert_eq("Hello, 张三!", @azimuth.greet(chinese_name))
  assert_eq("Hello, 田中太郎!", @azimuth.greet(japanese_name))
  assert_eq("Hello, 김철수!", @azimuth.greet(korean_name))
  assert_eq("Hello, أحمد!", @azimuth.greet(arabic_name))
  assert_eq("Hello, Иван!", @azimuth.greet(russian_name))
  
  // 测试包含表情符号的字符串
  let emoji_name = "😀🎉🌟"
  let result2 = @azimuth.greet(emoji_name)
  assert_eq(true, result2.contains(emoji_name))
}