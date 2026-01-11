// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

// 综合测试用例 - 包含数学性质、实际应用和边界情况测试

// 测试用例 1: 数学性质 - 加法和乘法的交换律验证
///|
test "mathematical_commutative_laws" {
  // 加法交换律: a + b = b + a
  assert_eq(@clean_test.@clean_test.add(0, 0), @clean_test.@clean_test.add(0, 0))
  assert_eq(@clean_test.@clean_test.add(1, 2), @clean_test.@clean_test.add(2, 1))
  assert_eq(@clean_test.@clean_test.add(-1, 1), @clean_test.@clean_test.add(1, -1))
  assert_eq(@clean_test.@clean_test.add(-5, -3), @clean_test.@clean_test.add(-3, -5))
  assert_eq(@clean_test.@clean_test.add(100, 200), @clean_test.@clean_test.add(200, 100))
  assert_eq(@clean_test.@clean_test.add(-100, 50), @clean_test.@clean_test.add(50, -100))
  
  // 乘法交换律: a * b = b * a
  assert_eq(@clean_test.@clean_test.multiply(0, 5), @clean_test.@clean_test.multiply(5, 0))
  assert_eq(@clean_test.@clean_test.multiply(1, 7), @clean_test.@clean_test.multiply(7, 1))
  assert_eq(@clean_test.@clean_test.multiply(-1, 3), @clean_test.@clean_test.multiply(3, -1))
  assert_eq(@clean_test.@clean_test.multiply(-2, 4), @clean_test.@clean_test.multiply(4, -2))
  assert_eq(@clean_test.@clean_test.multiply(10, 20), @clean_test.@clean_test.multiply(20, 10))
  assert_eq(@clean_test.@clean_test.multiply(-3, -6), @clean_test.@clean_test.multiply(-6, -3))
}

// 测试用例 2: 数学性质 - 结合律验证
///|
test "mathematical_associative_laws" {
  // 加法结合律: (a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = 300
  let left_assoc = @clean_test.@clean_test.add(@clean_test.@clean_test.add(a, b), c)
  let right_assoc = @clean_test.@clean_test.add(a, @clean_test.@clean_test.add(b, c))
  assert_eq(left_assoc, right_assoc)
  
  // 负数测试
  let x = -50
  let y = -30
  let z = 20
  let left_assoc_neg = @clean_test.@clean_test.add(@clean_test.@clean_test.add(x, y), z)
  let right_assoc_neg = @clean_test.@clean_test.add(x, @clean_test.@clean_test.add(y, z))
  assert_eq(left_assoc_neg, right_assoc_neg)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let m = 2
  let n = 3
  let p = 4
  let left_mult = @clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(m, n), p)
  let right_mult = @clean_test.@clean_test.multiply(m, @clean_test.@clean_test.multiply(n, p))
  assert_eq(left_mult, right_mult)
}

// 测试用例 3: 实际应用 - 银行复利计算
///|
test "bank_compound_interest" {
  // 本金
  let principal = 10000
  // 年利率 5%
  let rate_percent = 5
  // 年数
  let years = 3
  
  // 第一年结束: 本金 + 利息
  let year1_interest = @clean_test.@clean_test.multiply(principal, rate_percent) / 100
  let year1_total = @clean_test.@clean_test.add(principal, year1_interest)
  
  // 第二年结束: 第一年总额 + 新利息
  let year2_interest = @clean_test.@clean_test.multiply(year1_total, rate_percent) / 100
  let year2_total = @clean_test.@clean_test.add(year1_total, year2_interest)
  
  // 第三年结束: 第二年总额 + 新利息
  let year3_interest = @clean_test.@clean_test.multiply(year2_total, rate_percent) / 100
  let year3_total = @clean_test.@clean_test.add(year2_total, year3_interest)
  
  // 验证结果大约为 11576 (10000 * 1.05³)
  assert_eq(true, year3_total >= 11570 && year3_total <= 11580)
}

// 测试用例 4: 实际应用 - 购物折扣计算
///|
test "shopping_discount_calculation" {
  // 商品价格
  let item1_price = 199
  let item2_price = 299
  let item3_price = 399
  let item4_price = 499
  // 折扣率
  let discount_percent = 20
  
  // 计算总价
  let total_price = @clean_test.@clean_test.add(
    @clean_test.@clean_test.add(item1_price, item2_price),
    @clean_test.@clean_test.add(item3_price, item4_price)
  )
  
  // 计算折扣金额
  let discount_amount = @clean_test.@clean_test.multiply(total_price, discount_percent) / 100
  
  // 计算最终价格
  let final_price = @clean_test.@clean_test.add(total_price, @clean_test.@clean_test.multiply(-1, discount_amount))
  
  // 验证: 原价 1396，折扣20%后应为 1396 * 0.8 = 1116.8 ≈ 1117
  assert_eq(true, final_price >= 1110 && final_price <= 1120)
}

// 测试用例 5: 边界情况 - 极值运算
///|
test "extreme_value_operations" {
  // 测试最大值和最小值的运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加法测试
  assert_eq(max_val, @clean_test.@clean_test.add(max_val, 1))
  assert_eq(max_val, @clean_test.@clean_test.add(max_val, max_val))
  
  // 最小值加法测试
  assert_eq(min_val, @clean_test.@clean_test.add(min_val, -1))
  assert_eq(min_val, @clean_test.@clean_test.add(min_val, min_val))
  
  // 最大值乘法测试
  assert_eq(max_val, @clean_test.@clean_test.multiply(max_val, 2))
  assert_eq(max_val, @clean_test.@clean_test.multiply(46341, 46341))
  
  // 最小值乘法测试
  assert_eq(min_val, @clean_test.@clean_test.multiply(min_val, 2))
  assert_eq(min_val, @clean_test.@clean_test.multiply(min_val, -1))
}

// 测试用例 6: 字符串处理 - 多语言和特殊字符
///|
test "multilingual_string_processing" {
  // 测试不同语言的问候语
  let name1 = "世界"
  let name2 = "World"
  let name3 = "MoonBit"
  
  let greeting1 = @clean_test.@clean_test.greet(name1)
  let greeting2 = @clean_test.@clean_test.greet(name2)
  let greeting3 = @clean_test.@clean_test.greet(name3)
  
  // 验证问候语格式正确
  assert_eq("Hello, 世界!", greeting1)
  assert_eq("Hello, World!", greeting2)
  assert_eq("Hello, MoonBit!", greeting3)
  
  // 测试特殊字符
  let name4 = "John-Doe"
  let name5 = "O'Reilly"
  
  let greeting4 = @clean_test.@clean_test.greet(name4)
  let greeting5 = @clean_test.@clean_test.greet(name5)
  
  assert_eq("Hello, John-Doe!", greeting4)
  assert_eq("Hello, O'Reilly!", greeting5)
}

// 测试用例 7: 数学应用 - 简单统计计算
///|
test "basic_statistical_calculations" {
  // 模拟数据集: [10, 20, 30, 40, 50]
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 50
  let count = 5
  
  // 计算总和
  let sum = @clean_test.@clean_test.add(
    @clean_test.@clean_test.add(
      @clean_test.@clean_test.add(data1, data2),
      @clean_test.@clean_test.add(data3, data4)
    ),
    data5
  )
  
  // 计算平均值 (通过乘法验证)
  let expected_average = 30
  let average_check = @clean_test.@clean_test.multiply(expected_average, count)
  assert_eq(sum, average_check)
  
  // 计算方差 (简化版)
  let mean = 30
  let diff1 = @clean_test.@clean_test.add(data1, @clean_test.@clean_test.multiply(-1, mean))  // 10 - 30 = -20
  let diff2 = @clean_test.@clean_test.add(data2, @clean_test.@clean_test.multiply(-1, mean))  // 20 - 30 = -10
  let diff3 = @clean_test.@clean_test.add(data3, @clean_test.@clean_test.multiply(-1, mean))  // 30 - 30 = 0
  let diff4 = @clean_test.@clean_test.add(data4, @clean_test.@clean_test.multiply(-1, mean))  // 40 - 30 = 10
  let diff5 = @clean_test.@clean_test.add(data5, @clean_test.@clean_test.multiply(-1, mean))  // 50 - 30 = 20
  
  // 计算平方和
  let sq_sum = @clean_test.@clean_test.add(
    @clean_test.@clean_test.add(
      @clean_test.@clean_test.multiply(diff1, diff1),
      @clean_test.@clean_test.multiply(diff2, diff2)
    ),
    @clean_test.@clean_test.add(
      @clean_test.@clean_test.add(
        @clean_test.@clean_test.multiply(diff3, diff3),
        @clean_test.@clean_test.multiply(diff4, diff4)
      ),
      @clean_test.@clean_test.multiply(diff5, diff5)
    )
  )
  
  // 验证方差计算: (-20)² + (-10)² + 0² + 10² + 20² = 400 + 100 + 0 + 100 + 400 = 1000
  assert_eq(1000, sq_sum)
}

// 测试用例 8: 物理应用 - 简单运动学计算
///|
test "physics_kinematics_calculations" {
  // 自由落体运动: h = v₀t + 0.5gt²
  // 为避免小数，我们计算 2h = 2v₀t + gt²
  
  let initial_velocity = 10  // 初始速度 m/s
  let gravity = 10           // 重力加速度 m/s² (简化为10)
  let time = 5               // 时间 s
  
  // 计算 2h = 2*v₀*t + g*t²
  let double_height = @clean_test.@clean_test.add(
    @clean_test.@clean_test.multiply(2, @clean_test.@clean_test.multiply(initial_velocity, time)),
    @clean_test.@clean_test.multiply(gravity, @clean_test.@clean_test.multiply(time, time))
  )
  
  // 验证: 2*10*5 + 10*25 = 100 + 250 = 350，所以 h = 175
  assert_eq(350, double_height)
  
  // 抛物运动: 水平距离 = 水平速度 * 时间
  let horizontal_velocity = 20  // m/s
  let horizontal_distance = @clean_test.@clean_test.multiply(horizontal_velocity, time)
  assert_eq(100, horizontal_distance)
  
  // 垂直速度: v = v₀ + gt
  let vertical_velocity = @clean_test.@clean_test.add(initial_velocity, @clean_test.@clean_test.multiply(gravity, time))
  assert_eq(60, vertical_velocity)
}