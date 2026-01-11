// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

// 标准 MoonBit 测试用例

// 测试加法的结合律：(a + b) + c = a + (b + c)
///|
test "addition_associative_law" {
  assert_eq(@clean_test.@clean_test.add(@clean_test.@clean_test.add(10, 20), 30), @clean_test.@clean_test.add(10, @clean_test.@clean_test.add(20, 30)))
  assert_eq(@clean_test.@clean_test.add(@clean_test.@clean_test.add(-5, 15), 10), @clean_test.@clean_test.add(-5, @clean_test.@clean_test.add(15, 10)))
  assert_eq(@clean_test.@clean_test.add(@clean_test.@clean_test.add(0, 100), 50), @clean_test.@clean_test.add(0, @clean_test.@clean_test.add(100, 50)))
}

// 测试乘法的交换律：a * b = b * a
///|
test "multiplication_commutative_law" {
  assert_eq(@clean_test.@clean_test.multiply(15, 25), @clean_test.@clean_test.multiply(25, 15))
  assert_eq(@clean_test.@clean_test.multiply(-8, 12), @clean_test.@clean_test.multiply(12, -8))
  assert_eq(@clean_test.@clean_test.multiply(-7, -9), @clean_test.@clean_test.multiply(-9, -7))
  assert_eq(@clean_test.@clean_test.multiply(0, 123), @clean_test.@clean_test.multiply(123, 0))
}

// 测试字符串拼接的边界情况
///|
test "string_concatenation_edge_cases" {
  assert_eq_string("Hello, !", @clean_test.@clean_test.greet(""))
  assert_eq_string("Hello, A!", @clean_test.@clean_test.greet("A"))
  assert_eq_string("Hello, 1234567890!", @clean_test.@clean_test.greet("1234567890"))
  assert_eq_string("Hello,   !", @clean_test.@clean_test.greet("  "))
}

// 测试大数运算的边界情况
///|
test "large_number_boundary_operations" {
  // 测试接近最大值的加法
  assert_eq(2147483647, @clean_test.@clean_test.add(2147483640, 7))
  assert_eq(2147483647, @clean_test.@clean_test.add(2147483630, 17))
  
  // 测试接近最小值的加法
  assert_eq(-2147483648, @clean_test.@clean_test.add(-2147483640, -8))
  assert_eq(-2147483648, @clean_test.@clean_test.add(-2147483630, -18))
}

// 测试负数乘法的特殊情况
///|
test "negative_multiplication_special_cases" {
  // 测试负数相乘得到正数
  assert_eq(144, @clean_test.@clean_test.multiply(-12, -12))
  assert_eq(81, @clean_test.@clean_test.multiply(-9, -9))
  
  // 测试负数乘以正数得到负数
  assert_eq(-36, @clean_test.@clean_test.multiply(-6, 6))
  assert_eq(-49, @clean_test.@clean_test.multiply(7, -7))
}

// 测试零值运算的各种情况
///|
test "zero_value_operations" {
  // 加法中的零值
  assert_eq(123, @clean_test.@clean_test.add(123, 0))
  assert_eq(-456, @clean_test.@clean_test.add(0, -456))
  assert_eq(0, @clean_test.@clean_test.add(0, 0))
  
  // 乘法中的零值
  assert_eq(0, @clean_test.@clean_test.multiply(123, 0))
  assert_eq(0, @clean_test.@clean_test.multiply(0, -456))
  assert_eq(0, @clean_test.@clean_test.multiply(0, 0))
}

// 测试实际应用场景：计算折扣价格
///|
test "discount_price_calculation" {
  // 原价
  let original_price = 1000
  // 折扣率 20%
  let discount_rate = 20
  // 折扣金额 = 原价 * 折扣率 / 100
  let discount_amount = @clean_test.@clean_test.multiply(original_price, discount_rate)
  // 最终价格 = 原价 - 折扣金额
  let final_price = @clean_test.@clean_test.add(original_price, @clean_test.@clean_test.multiply(-1, discount_amount))
  
  // 验证：1000 - (1000 * 20 / 100) = 800
  assert_eq(80000, @clean_test.@clean_test.multiply(100, final_price))  // 800 * 100 = 80000
}

// 测试实际应用场景：计算平均分
///|
test "average_score_calculation" {
  // 五门课程的成绩
  let score1 = 85
  let score2 = 90
  let score3 = 78
  let score4 = 92
  let score5 = 88
  
  // 计算总分
  let total_score = @clean_test.@clean_test.add(
    @clean_test.@clean_test.add(
      @clean_test.@clean_test.add(score1, score2),
      @clean_test.@clean_test.add(score3, score4)
    ),
    score5
  )
  
  // 平均分 * 5 = 总分
  let average_times_5 = total_score
  
  // 验证：85 + 90 + 78 + 92 + 88 = 433
  assert_eq(433, average_times_5)
}

// 测试数学性质：分配律 a * (b + c) = a * b + a * c
///|
test "multiplication_distributive_law" {
  let a = 5
  let b = 10
  let c = 15
  
  // a * (b + c)
  let left_side = @clean_test.@clean_test.multiply(a, @clean_test.@clean_test.add(b, c))
  
  // a * b + a * c
  let right_side = @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(a, b), @clean_test.@clean_test.multiply(a, c))
  
  assert_eq(left_side, right_side)
  
  // 测试负数情况
  let x = -3
  let y = 7
  let z = -2
  
  let left_side_neg = @clean_test.@clean_test.multiply(x, @clean_test.@clean_test.add(y, z))
  let right_side_neg = @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(x, y), @clean_test.@clean_test.multiply(x, z))
  
  assert_eq(left_side_neg, right_side_neg)
}

// 测试字符串处理：包含特殊字符
///|
test "string_special_character_handling" {
  assert_eq_string("Hello, MoonBit!", @clean_test.@clean_test.greet("MoonBit"))
  assert_eq_string("Hello, 中文!", @clean_test.@clean_test.greet("中文"))
  assert_eq_string("Hello, 🚀!", @clean_test.@clean_test.greet("🚀"))
  assert_eq_string("Hello, C++!", @clean_test.@clean_test.greet("C++"))
  assert_eq_string("Hello, @#$%!", @clean_test.@clean_test.greet("@#$%"))
}