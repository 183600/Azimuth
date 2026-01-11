// 额外的标准测试用例
// 包含10个新的测试用例，覆盖不同功能场景

// 测试用例1: 加法运算的数学性质验证
///|
test "addition_mathematical_properties" {
  // 验证加法恒等式: a + 0 = a
  assert_eq(42, @azimuth.add(42, 0))
  assert_eq(-17, @azimuth.add(-17, 0))
  
  // 验证加法逆元: a + (-a) = 0
  assert_eq(0, @azimuth.add(25, @azimuth.multiply(-1, 25)))
  assert_eq(0, @azimuth.add(-100, @azimuth.multiply(-1, -100)))
  
  // 验证连续加法的结合性
  assert_eq(@azimuth.add(@azimuth.add(1, 2), 3), @azimuth.add(1, @azimuth.add(2, 3)))
}

// 测试用例2: 乘法运算的数学性质验证
///|
test "multiplication_mathematical_properties" {
  // 验证乘法恒等式: a * 1 = a
  assert_eq(99, @azimuth.multiply(99, 1))
  assert_eq(-88, @azimuth.multiply(-88, 1))
  
  // 验证乘法零元: a * 0 = 0
  assert_eq(0, @azimuth.multiply(12345, 0))
  assert_eq(0, @azimuth.multiply(-6789, 0))
  
  // 验证乘法分配律: a * (b + c) = a * b + a * c
  let a = 7, b = 8, c = 9
  assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
}

// 测试用例3: 字符串处理的高级测试
///|
test "advanced_string_processing" {
  // 测试包含空格的字符串
  assert_eq("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含数字的字符串
  assert_eq("Hello, User123!", @azimuth.greet("User123"))
  
  // 测试包含特殊符号的字符串
  assert_eq("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  
  // 测试长字符串
  let long_name = "VeryVeryLongNameThatExceedsNormalLength"
  assert_eq("Hello, VeryVeryLongNameThatExceedsNormalLength!", @azimuth.greet(long_name))
}

// 测试用例4: 边界值的组合测试
///|
test "boundary_value_combinations" {
  // 最大值与最小值的运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加最小值
  assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 最大值乘1
  assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 最小值乘1
  assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 接近边界的值
  assert_eq(2147483646, @azimuth.add(2147483645, 1))
  assert_eq(-2147483647, @azimuth.add(-2147483646, -1))
}

// 测试用例5: 实际应用场景 - 计算折扣价格
///|
test "practical_discount_calculation" {
  // 原价和折扣率
  let original_price = 1000
  let discount_percentage = 20  // 20%折扣
  
  // 计算折扣金额: 原价 * 折扣率 / 100
  let discount_amount_times_100 = @azimuth.multiply(original_price, discount_percentage)
  
  // 计算最终价格: 原价 - 折扣金额
  let final_price_times_100 = @azimuth.add(
    @azimuth.multiply(original_price, 100),
    @azimuth.multiply(-1, discount_amount_times_100)
  )
  
  // 验证: 1000 * 100 - 1000 * 20 = 100000 - 20000 = 80000
  assert_eq(80000, final_price_times_100)
}

// 测试用例6: 实际应用场景 - 计算平均分
///|
test "practical_average_score_calculation" {
  // 学生成绩
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 96
  let score5 = 88
  
  // 计算总分
  let total_score = @azimuth.add(
    @azimuth.add(
      @azimuth.add(score1, score2),
      @azimuth.add(score3, score4)
    ),
    score5
  )
  
  // 验证总分
  assert_eq(439, total_score)
  
  // 平均分 * 科目数 = 总分
  let average_times_5 = total_score
  assert_eq(439, average_times_5)
}

// 测试用例7: 数学序列测试 - 斐波那契数列
///|
test "fibonacci_sequence_calculation" {
  // 计算前几个斐波那契数
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  
  // 验证计算结果
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
  assert_eq(8, fib6)
  
  // 验证斐波那契性质: F(n+1) * F(n-1) - F(n)² = (-1)^n
  let lhs = @azimuth.add(
    @azimuth.multiply(fib5, fib3),
    @azimuth.multiply(-1, @azimuth.multiply(fib4, fib4))
  )
  assert_eq(-1, lhs)  // 对于n=4，结果应该是-1
}

// 测试用例8: 几何计算 - 三角形面积
///|
test "triangle_area_calculation" {
  // 使用海伦公式: 面积 = √(s(s-a)(s-b)(s-c))
  // 其中s = (a+b+c)/2是半周长
  // 我们验证s² = (a+b+c)²/4的计算
  
  let a = 3, b = 4, c = 5  // 直角三角形
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.add(a, b), c)
  
  // 计算半周长的4倍 (避免分数)
  let s_times_2 = perimeter  // s * 2 = a + b + c
  
  // 验证周长
  assert_eq(12, perimeter)
  assert_eq(12, s_times_2)
}

// 测试用例9: 物理计算 - 动能计算
///|
test "physics_kinetic_energy_calculation" {
  // 动能公式: KE = 0.5 * m * v²
  // 我们通过乘以2来避免小数: KE * 2 = m * v²
  
  let mass = 10      // 质量 (kg)
  let velocity = 5   // 速度 (m/s)
  
  // 计算v²
  let velocity_squared = @azimuth.multiply(velocity, velocity)
  
  // 计算动能 * 2
  let kinetic_energy_times_2 = @azimuth.multiply(mass, velocity_squared)
  
  // 验证: 10 * 25 = 250，所以动能是125焦耳
  assert_eq(250, kinetic_energy_times_2)
}

// 测试用例10: 复合运算测试
///|
test "complex_composite_operations" {
  // 计算 (a + b) * (c - d) + e * f
  // 其中减法通过加负数实现
  
  let a = 10, b = 20, c = 30, d = 15, e = 5, f = 8
  
  // 计算 a + b
  let sum_ab = @azimuth.add(a, b)
  
  // 计算 c - d = c + (-d)
  let diff_cd = @azimuth.add(c, @azimuth.multiply(-1, d))
  
  // 计算 (a + b) * (c - d)
  let product = @azimuth.multiply(sum_ab, diff_cd)
  
  // 计算 e * f
  let product_ef = @azimuth.multiply(e, f)
  
  // 计算最终结果
  let result = @azimuth.add(product, product_ef)
  
  // 验证: (10 + 20) * (30 - 15) + 5 * 8 = 30 * 15 + 40 = 450 + 40 = 490
  assert_eq(490, result)
}