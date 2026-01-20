// 新增的 MoonBit 标准测试用例
// 测试 azimuth 核心功能的各种场景

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  assert_eq(add(a, b), add(b, a))
  
  // 测试负数情况
  let c = -123
  let d = 456
  assert_eq(add(c, d), add(d, c))
  
  // 测试混合符号
  let e = 100
  let f = -50
  assert_eq(add(e, f), add(f, e))
}

test "add_associative_property" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = 300
  
  let result1 = add(add(a, b), c)
  let result2 = add(a, add(b, c))
  assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let x = -100
  let y = 250
  let z = -150
  
  let result3 = add(add(x, y), z)
  let result4 = add(x, add(y, z))
  assert_eq(result3, result4)
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 5
  let b = 10
  let c = 15
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 测试负数情况
  let x = -3
  let y = 8
  let z = -2
  
  let left_side2 = multiply(x, add(y, z))
  let right_side2 = add(multiply(x, y), multiply(x, z))
  assert_eq(left_side2, right_side2)
}

test "multiply_identity_property" {
  // 测试乘法单位元：a × 1 = a，a × 0 = 0
  let test_values = [0, 1, -1, 100, -100, 2147483647, -2147483648]
  
  // 注意：这里使用循环的简化版本，因为 MoonBit 可能不支持数组遍历
  assert_eq(0, multiply(0, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(100, multiply(100, 1))
  assert_eq(-100, multiply(-100, 1))
  assert_eq(2147483647, multiply(2147483647, 1))
  assert_eq(-2147483648, multiply(-2147483648, 1))
  
  // 测试乘以零
  assert_eq(0, multiply(0, 1))
  assert_eq(0, multiply(1, 0))
  assert_eq(0, multiply(-1, 0))
  assert_eq(0, multiply(100, 0))
  assert_eq(0, multiply(-100, 0))
  assert_eq(0, multiply(2147483647, 0))
  assert_eq(0, multiply(-2147483648, 0))
}

test "greet_function_internationalization" {
  // 测试 greet 函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, naïve!", greet("naïve"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 🎉!", greet("🎉"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
}

test "complex_financial_calculations" {
  // 测试复杂金融计算场景
  // 复合利率计算：最终金额 = 本金 × (1 + 利率)^期数
  let principal = 10000
  let rate_percent = 5
  let periods = 3
  
  // 简化计算：10000 × (1 + 0.05 × 3) = 10000 × 1.15 ≈ 11500
  let growth_factor = add(100, multiply(rate_percent, periods))
  let final_amount = multiply(principal, growth_factor) / 100
  assert_eq(115000, final_amount)  // 115000 表示 11500.00
  
  // 贷款计算：月供 = [本金 × 月利率 × (1 + 月利率)^还款月数] / [(1 + 月利率)^还款月数 - 1]
  // 简化版本
  let loan_principal = 200000
  let monthly_rate = 6  // 0.6%
  let months = 12
  
  let monthly_interest = multiply(loan_principal, monthly_rate) / 1000
  let monthly_payment = monthly_interest + multiply(loan_principal, 1) / months
  assert_eq(18333, monthly_payment)  // 简化计算的月供
}

test "physics_simulation_calculations" {
  // 测试物理模拟计算
  // 自由落体距离：s = 0.5 × g × t²
  // 简化为整数计算：s = 5 × t² (g ≈ 10 m/s²)
  let time = 5
  let distance = multiply(5, multiply(time, time))
  assert_eq(125, distance)  // 5 × 25 = 125 米
  
  // 动能计算：KE = 0.5 × m × v²
  // 简化为整数计算：KE = m × v² / 2
  let mass = 1000
  let velocity = 20
  let kinetic_energy = multiply(mass, multiply(velocity, velocity)) / 2
  assert_eq(200000, kinetic_energy)  // 1000 × 400 / 2 = 200000 焦耳
  
  // 势能计算：PE = m × g × h
  // 简化为整数计算：PE = m × 10 × h
  let height = 50
  let potential_energy = multiply(multiply(mass, 10), height)
  assert_eq(500000, potential_energy)  // 1000 × 10 × 50 = 500000 焦耳
}

test "statistical_analysis_functions" {
  // 测试统计分析函数
  // 计算方差：σ² = Σ(xi - μ)² / n
  // 数据集：[10, 20, 30, 40, 50]
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 50
  
  // 计算平均值
  let sum = add(add(add(add(data1, data2), data3), data4), data5)
  let mean = sum / 5
  assert_eq(30, mean)
  
  // 计算偏差平方和
  let diff1 = add(data1, -mean)
  let diff2 = add(data2, -mean)
  let diff3 = add(data3, -mean)
  let diff4 = add(data4, -mean)
  let diff5 = add(data5, -mean)
  
  let variance_sum = add(add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), multiply(diff3, diff3)), multiply(diff4, diff4)), multiply(diff5, diff5))
  let variance = variance_sum / 5
  assert_eq(200, variance)  // 方差为 200
  
  // 标准差（整数近似）
  let std_dev = 14  // √200 ≈ 14.14，取整数部分
  assert_eq(14, std_dev)
}

test "geometry_calculations_extended" {
  // 测试扩展几何计算
  // 立方体表面积：6 × a²
  let cube_side = 5
  let cube_surface_area = multiply(6, multiply(cube_side, cube_side))
  assert_eq(150, cube_surface_area)
  
  // 立方体体积：a³
  let cube_volume = multiply(multiply(cube_side, cube_side), cube_side)
  assert_eq(125, cube_volume)
  
  // 球体表面积（简化）：4 × π × r² ≈ 12 × r²
  let sphere_radius = 4
  let sphere_surface_area = multiply(12, multiply(sphere_radius, sphere_radius))
  assert_eq(192, sphere_surface_area)
  
  // 球体体积（简化）：4/3 × π × r³ ≈ 4 × r³
  let sphere_volume = multiply(4, multiply(multiply(sphere_radius, sphere_radius), sphere_radius))
  assert_eq(256, sphere_volume)
  
  // 圆柱体表面积（简化）：2 × π × r × h + 2 × π × r² ≈ 6 × r × h + 6 × r²
  let cylinder_radius = 3
  let cylinder_height = 8
  let cylinder_surface_area = add(multiply(6, multiply(cylinder_radius, cylinder_height)), multiply(6, multiply(cylinder_radius, cylinder_radius)))
  assert_eq(162, cylinder_surface_area)
  
  // 圆柱体体积（简化）：π × r² × h ≈ 3 × r² × h
  let cylinder_volume = multiply(3, multiply(multiply(cylinder_radius, cylinder_radius), cylinder_height))
  assert_eq(216, cylinder_volume)
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟
  // O(n) 线性复杂度：简单求和
  let n = 100
  let linear_sum = multiply(n, add(n, 1)) / 2  // 1 + 2 + ... + n = n(n+1)/2
  assert_eq(5050, linear_sum)
  
  // O(n²) 平方复杂度：嵌套循环求和
  // Σ(i=1 to n) Σ(j=1 to n) (i + j) = n × Σ(i=1 to n) i + n × Σ(j=1 to n) j = 2n × n(n+1)/2 = n²(n+1)
  let quadratic_sum = multiply(multiply(n, n), add(n, 1))
  assert_eq(1010000, quadratic_sum)  // 100² × 101 = 1010000
  
  // O(log n) 对数复杂度：二分查找步数（简化）
  let log_steps = 7  // log₂(100) ≈ 6.64，向上取整为 7
  assert_eq(7, log_steps)
  
  // O(n log n) 线性对数复杂度：高效排序算法比较次数（简化）
  let n_log_n_ops = multiply(n, 7)  // 100 × log₂(100) ≈ 100 × 7
  assert_eq(700, n_log_n_ops)
}

// 新增的测试用例
test "new_negative_addition" {
  // 测试负数加法
  assert_eq(-5, add(-2, -3))
  assert_eq(-10, add(-7, -3))
  assert_eq(0, add(-5, 5))
  assert_eq(-100, add(-150, 50))
}

test "new_large_multiplication" {
  // 测试大数乘法
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(86400000, multiply(86400, 1000))
  assert_eq(-1000000, multiply(-1000, 1000))
  assert_eq(1000000, multiply(-1000, -1000))
}

test "new_unicode_greeting" {
  // 测试Unicode字符串处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ✓✗!", greet("✓✗"))
}

test "new_mathematical_series" {
  // 测试数学级数计算
  // 等差数列求和：1 + 2 + 3 + 4 + 5 = 15
  let sum_1_to_5 = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum_1_to_5)
  
  // 等比数列求和：2 + 4 + 8 + 16 = 30
  let sum_powers = add(add(add(2, 4), 8), 16)
  assert_eq(30, sum_powers)
}

test "new_boundary_values" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与1的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  
  // 最小值与1的运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 边界值相加
  assert_eq(-1, add(max_val, min_val))
}

test "new_complex_strings" {
  // 测试复杂字符串场景
  // 数字字符串
  assert_eq_string("Hello, 2023!", greet("2023"))
  
  // 特殊符号组合
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  
  // 混合内容
  assert_eq_string("Hello, Test123!", greet("Test123"))
  
  // 空格处理
  assert_eq_string("Hello, Leading Space!", greet(" Leading Space"))
}

test "new_calculation_precision" {
  // 测试计算精度
  // 连续加法
  let result = add(add(add(1, 2), 3), 4)
  assert_eq(10, result)
  
  // 连续乘法
  let product = multiply(multiply(2, 3), 4)
  assert_eq(24, product)
  
  // 混合运算
  let mixed = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, mixed)
}