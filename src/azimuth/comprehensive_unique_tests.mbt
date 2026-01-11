// 综合独特测试用例 - 覆盖高级场景和边界情况

// 测试用例 1: 性能压力测试 - 大量连续运算
///|
test "performance_stress_test" {
  // 测试大量连续加法运算的性能
  let result = add(
    add(
      add(
        add(
          add(1000000, 2000000),
          add(3000000, 4000000)
        ),
        add(5000000, 6000000)
      ),
      add(7000000, 8000000)
    ),
    add(9000000, 10000000)
  )
  assert_eq(55000000, result)
  
  // 测试大量连续乘法运算的性能
  let product = multiply(
    multiply(
      multiply(2, 3),
      multiply(4, 5)
    ),
    multiply(6, 7)
  )
  assert_eq(5040, product)
}

// 测试用例 2: 嵌套运算测试 - 复杂的嵌套表达式
///|
test "nested_operations_test" {
  // 测试深度嵌套的加法和乘法组合
  // 计算: (1 + 2) * (3 + 4) + (5 + 6) * (7 + 8)
  let part1 = multiply(
    add(1, 2),
    add(3, 4)
  )
  let part2 = multiply(
    add(5, 6),
    add(7, 8)
  )
  let result = add(part1, part2)
  assert_eq(186, result)  // 3 * 7 + 11 * 15 = 21 + 165 = 186
  
  // 测试更复杂的嵌套: ((a+b)*(c+d)) + ((e+f)*(g+h)) + ((i+j)*(k+l))
  let complex = add(
    add(
      multiply(add(1, 2), add(3, 4)),
      multiply(add(5, 6), add(7, 8))
    ),
    multiply(add(9, 10), add(11, 12))
  )
  assert_eq(623, complex)  // 21 + 165 + 437 = 623
}

// 测试用例 3: 字符串边界测试 - 极长字符串和特殊字符
///|
test "string_boundary_test" {
  // 测试包含数字、字母、符号的复杂字符串
  let complex1 = "User123_Admin!@#"
  assert_eq("Hello, User123_Admin!@#!", greet(complex1))
  
  // 测试包含空格的字符串
  let complex2 = "John Doe Jr."
  assert_eq("Hello, John Doe Jr.!", greet(complex2))
  
  // 测试包含多种Unicode字符的字符串
  let complex3 = "🌙⭐🌟✨💫"
  assert_eq("Hello, 🌙⭐🌟✨💫!", greet(complex3))
  
  // 测试包含HTML标签风格的字符串
  let complex4 = "<div>content</div>"
  assert_eq("Hello, <div>content</div>!", greet(complex4))
  
  // 测试包含路径风格的字符串
  let complex5 = "/home/user/documents"
  assert_eq("Hello, /home/user/documents!", greet(complex5))
}

// 测试用例 4: 数学恒等式测试 - 更高级的数学性质
///|
test "mathematical_identities_test" {
  // 测试平方差公式: a² - b² = (a+b)(a-b)
  let a = 15
  let b = 8
  let a_squared = multiply(a, a)
  let b_squared = multiply(b, b)
  let difference = add(a_squared, multiply(-1, b_squared))
  
  let sum = add(a, b)
  let diff = add(a, multiply(-1, b))
  let product = multiply(sum, diff)
  
  assert_eq(difference, product)  // 225 - 64 = 161, (15+8)(15-8) = 23*7 = 161
  
  // 测试立方和公式: a³ + b³ = (a+b)(a²-ab+b²)
  let x = 4
  let y = 2
  let x_cubed = multiply(x, multiply(x, x))
  let y_cubed = multiply(y, multiply(y, y))
  let sum_cubes = add(x_cubed, y_cubed)
  
  let x_squared = multiply(x, x)
  let y_squared = multiply(y, y)
  let xy = multiply(x, y)
  let formula = multiply(
    add(x, y),
    add(add(x_squared, multiply(-1, xy)), y_squared)
  )
  
  assert_eq(sum_cubes, formula)  // 64 + 8 = 72, (4+2)(16-8+4) = 6*12 = 72
}

// 测试用例 5: 实际商业场景测试 - 复杂的商业计算
///|
test "business_scenario_test" {
  // 模拟订单总价计算：商品价格 * 数量 + 税费 - 折扣
  
  // 商品1: 价格299, 数量2
  let item1_price = 299
  let item1_quantity = 2
  let item1_total = multiply(item1_price, item1_quantity)
  
  // 商品2: 价格199, 数量3
  let item2_price = 199
  let item2_quantity = 3
  let item2_total = multiply(item2_price, item2_quantity)
  
  // 商品3: 价格99, 数量1
  let item3_price = 99
  let item3_quantity = 1
  let item3_total = multiply(item3_price, item3_quantity)
  
  // 小计
  let subtotal = add(add(item1_total, item2_total), item3_total)
  
  // 税费 (8% = 8/100, 我们计算税后总价)
  let tax_rate_numerator = 108  // (100 + 8)
  let tax_rate_denominator = 100
  let total_with_tax_times_100 = multiply(subtotal, tax_rate_numerator)
  
  // 折扣 (50元)
  let discount = 50
  let final_total_times_100 = add(
    total_with_tax_times_100,
    multiply(-100, discount)
  )
  
  // 验证计算: (299*2 + 199*3 + 99*1) = 598 + 597 + 99 = 1294
  // 含税: 1294 * 1.08 = 1397.52
  // 减折扣: 1397.52 - 50 = 1347.52
  // 乘以100: 134752
  assert_eq(134752, final_total_times_100)
}

// 测试用例 6: 错误恢复测试 - 极端条件下的行为
///|
test "error_recovery_test" {
  // 测试连续溢出操作后的恢复
  let overflow1 = add(2147483647, 1)  // 应该返回最大值
  let overflow2 = add(overflow1, 1)    // 应该仍然返回最大值
  let normal1 = add(overflow2, -100)   // 应该返回最大值-100
  assert_eq(2147483647, overflow1)
  assert_eq(2147483647, overflow2)
  assert_eq(2147483547, normal1)
  
  // 测试乘法溢出后的恢复
  let mul_overflow1 = multiply(46342, 46342)  // 应该返回最大值
  let mul_overflow2 = multiply(mul_overflow1, 2)  // 应该返回最大值
  let mul_normal = multiply(mul_overflow2, 0)  // 应该返回0
  assert_eq(2147483647, mul_overflow1)
  assert_eq(2147483647, mul_overflow2)
  assert_eq(0, mul_normal)
  
  // 测试最小值操作后的恢复
  let min_val = -2147483648
  let min_add = add(min_val, 1)  // 应该返回最小值+1
  let min_recover = add(min_add, -1)  // 应该返回最小值
  assert_eq(-2147483647, min_add)
  assert_eq(-2147483648, min_recover)
}

// 测试用例 7: 组合边界测试 - 多个边界条件的组合
///|
test "combined_boundary_test" {
  // 测试多个边界条件的组合操作
  
  // 边界值组合1: 最大值运算
  let max_val = 2147483647
  let min_val = -2147483648
  let combo1 = add(
    add(max_val, min_val),
    multiply(1, 1)
  )
  assert_eq(0, combo1)  // (max + min) + 1 = -1 + 1 = 0
  
  // 边界值组合2: 接近溢出的运算
  let near_max = 2147483640
  let near_min = -2147483640
  let combo2 = add(
    add(near_max, 7),
    add(near_min, -8)
  )
  assert_eq(-1, combo2)  // (2147483640 + 7) + (-2147483640 + -8) = 2147483647 + -2147483648 = -1
  
  // 边界值组合3: 乘法和加法混合
  let combo3 = add(
    multiply(46341, 46341),  // 接近溢出
    multiply(-1, -1)         // 简单运算
  )
  assert_eq(2147483647, combo3)  // 2147483647 + 1 = 2147483647 (溢出到最大值)
}

// 测试用例 8: 算法复杂度测试 - 特定算法的性能
///|
test "algorithm_complexity_test" {
  // 测试斐波那契数列计算的模拟
  // F(n) = F(n-1) + F(n-2), F(0)=0, F(1)=1
  // 我们计算前几项
  
  let f0 = 0
  let f1 = 1
  let f2 = add(f1, f0)  // 1 + 0 = 1
  let f3 = add(f2, f1)  // 1 + 1 = 2
  let f4 = add(f3, f2)  // 2 + 1 = 3
  let f5 = add(f4, f3)  // 3 + 2 = 5
  let f6 = add(f5, f4)  // 5 + 3 = 8
  let f7 = add(f6, f5)  // 8 + 5 = 13
  let f8 = add(f7, f6)  // 13 + 8 = 21
  let f9 = add(f8, f7)  // 21 + 13 = 34
  let f10 = add(f9, f8) // 34 + 21 = 55
  
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
  assert_eq(8, f6)
  assert_eq(13, f7)
  assert_eq(21, f8)
  assert_eq(34, f9)
  assert_eq(55, f10)
  
  // 测试阶乘计算的模拟
  // n! = n * (n-1) * ... * 1
  let fact1 = 1  // 1!
  let fact2 = multiply(2, fact1)   // 2 * 1 = 2
  let fact3 = multiply(3, fact2)   // 3 * 2 = 6
  let fact4 = multiply(4, fact3)   // 4 * 6 = 24
  let fact5 = multiply(5, fact4)   // 5 * 24 = 120
  let fact6 = multiply(6, fact5)   // 6 * 120 = 720
  
  assert_eq(2, fact2)
  assert_eq(6, fact3)
  assert_eq(24, fact4)
  assert_eq(120, fact5)
  assert_eq(720, fact6)
}

// 测试用例 9: 数据类型边界测试 - Int类型的极限情况
///|
test "data_type_boundary_test" {
  // 测试Int32的精确边界情况
  
  // 测试平方根边界: sqrt(2147483647) ≈ 46340.95
  let sqrt_max = 46340
  let sqrt_max_plus1 = 46341
  let sqrt_max_squared = multiply(sqrt_max, sqrt_max)
  let sqrt_max_plus1_squared = multiply(sqrt_max_plus1, sqrt_max_plus1)
  
  assert_eq(2147395600, sqrt_max_squared)    // 46340² = 2147395600 (不溢出)
  assert_eq(2147483647, sqrt_max_plus1_squared)  // 46341² = 2147483647 (接近溢出)
  
  // 测试立方根边界: cbrt(2147483647) ≈ 1290.22
  let cbrt_max = 1290
  let cbrt_max_plus1 = 1291
  let cbrt_max_cubed = multiply(
    cbrt_max,
    multiply(cbrt_max, cbrt_max)
  )
  let cbrt_max_plus1_cubed = multiply(
    cbrt_max_plus1,
    multiply(cbrt_max_plus1, cbrt_max_plus1)
  )
  
  assert_eq(2146689000, cbrt_max_cubed)     // 1290³ = 2146689000 (不溢出)
  assert_eq(2147483647, cbrt_max_plus1_cubed)  // 1291³ = 2147483647 (接近溢出)
  
  // 测试除法相关的边界（通过乘法模拟）
  // 测试最大值/2的精确性
  let max_div2_times2 = multiply(
    multiply(2147483647, 1),
    2
  )
  assert_eq(2147483647, max_div2_times2)  // 应该溢出到最大值
}

// 测试用例 10: 实际物理模拟测试 - 真实世界物理计算
///|
test "physics_simulation_test" {
  // 模拟自由落体运动: h = h0 - 0.5 * g * t²
  // 我们通过乘以2来避免小数
  
  let initial_height = 100  // 米
  let gravity = 10          // m/s² (简化为10)
  let time = 4              // 秒
  
  // h * 2 = h0 * 2 - g * t²
  let height_times_2 = add(
    multiply(initial_height, 2),
    multiply(
      multiply(-1, gravity),
      multiply(time, time)
    )
  )
  
  // 验证: 100 * 2 - 10 * 16 = 200 - 160 = 40
  // 所以实际高度 = 40 / 2 = 20米
  assert_eq(40, height_times_2)
  
  // 模拟斜抛运动: 水平距离 = v0 * cos(θ) * t
  // 假设 cos(45°) ≈ 0.707 ≈ 707/1000
  
  let initial_velocity = 20  // m/s
  let angle_cos_numerator = 707
  let angle_cos_denominator = 1000
  let time_flight = 3        // 秒
  
  // 水平距离 * 1000 = v0 * cos(θ分子) * t
  let distance_times_1000 = multiply(
    multiply(initial_velocity, angle_cos_numerator),
    time_flight
  )
  
  // 验证: 20 * 707 * 3 = 42420
  // 实际距离 = 42420 / 1000 = 42.42米
  assert_eq(42420, distance_times_1000)
  
  // 模拟动能计算: KE = 0.5 * m * v²
  // 通过乘以2来避免小数
  
  let mass = 5      // kg
  let velocity = 10 // m/s
  
  // KE * 2 = m * v²
  let kinetic_energy_times_2 = multiply(
    mass,
    multiply(velocity, velocity)
  )
  
  // 验证: 5 * 100 = 500
  // 实际动能 = 500 / 2 = 250焦耳
  assert_eq(500, kinetic_energy_times_2)
}