// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

// 测试 1: 加法零值边界情况
///|
test "add_zero_boundary_cases" {
  // 测试零与边界值的组合
  assert_eq(0, @clean_test.@clean_test.add(0, 0))
  assert_eq(2147483647, @clean_test.@clean_test.add(2147483647, 0))
  assert_eq(-2147483648, @clean_test.@clean_test.add(-2147483648, 0))
  assert_eq(2147483647, @clean_test.@clean_test.add(0, 2147483647))
  assert_eq(-2147483648, @clean_test.@clean_test.add(0, -2147483648))
}

// 测试 2: 乘法单位元和零元性质
///|
test "multiply_identity_and_zero" {
  // 测试乘法单位元 (1)
  assert_eq(123, @clean_test.@clean_test.multiply(123, 1))
  assert_eq(-456, @clean_test.@clean_test.multiply(-456, 1))
  assert_eq(0, @clean_test.@clean_test.multiply(0, 1))
  assert_eq(2147483647, @clean_test.@clean_test.multiply(2147483647, 1))
  assert_eq(-2147483648, @clean_test.@clean_test.multiply(-2147483648, 1))
  
  // 测试乘法零元 (0)
  assert_eq(0, @clean_test.@clean_test.multiply(123, 0))
  assert_eq(0, @clean_test.@clean_test.multiply(-456, 0))
  assert_eq(0, @clean_test.@clean_test.multiply(0, 0))
  assert_eq(0, @clean_test.@clean_test.multiply(2147483647, 0))
  assert_eq(0, @clean_test.@clean_test.multiply(-2147483648, 0))
}

// 测试 3: 负数乘法特性
///|
test "negative_multiplication_properties" {
  // 测试负数乘法规则
  assert_eq(-6, @clean_test.@clean_test.multiply(2, -3))
  assert_eq(-6, @clean_test.@clean_test.multiply(-2, 3))
  assert_eq(6, @clean_test.@clean_test.multiply(-2, -3))
  
  // 测试负数与边界值
  assert_eq(-2147483647, @clean_test.@clean_test.multiply(2147483647, -1))
  assert_eq(2147483647, @clean_test.@clean_test.multiply(-2147483647, -1))
  assert_eq(2147483646, @clean_test.@clean_test.multiply(-2147483646, -1))
}

// 测试 4: 字符串边界情况
///|
test "string_boundary_cases" {
  // 测试空字符串
  assert_eq_string("Hello, !", @clean_test.@clean_test.greet(""))
  
  // 测试单字符
  assert_eq_string("Hello, A!", @clean_test.@clean_test.greet("A"))
  assert_eq_string("Hello, 1!", @clean_test.@clean_test.greet("1"))
  assert_eq_string("Hello, !", @clean_test.@clean_test.greet("!"))
  
  // 测试长字符串
  let long_name = "VeryVeryVeryVeryVeryLongName"
  assert_eq_string("Hello, VeryVeryVeryVeryVeryLongName!", @clean_test.@clean_test.greet(long_name))
}

// 测试 5: 加法结合律验证
///|
test "addition_associativity" {
  // 验证 (a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = 300
  
  let left_assoc = @clean_test.@clean_test.add(@clean_test.@clean_test.add(a, b), c)
  let right_assoc = @clean_test.@clean_test.add(a, @clean_test.@clean_test.add(b, c))
  assert_eq(left_assoc, right_assoc)
  
  // 测试负数情况
  let x = -100
  let y = -200
  let z = 300
  
  let left_neg = @clean_test.@clean_test.add(@clean_test.@clean_test.add(x, y), z)
  let right_neg = @clean_test.@clean_test.add(x, @clean_test.@clean_test.add(y, z))
  assert_eq(left_neg, right_neg)
}

// 测试 6: 乘法分配律验证
///|
test "multiplication_distributivity" {
  // 验证 a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  
  let left_dist = @clean_test.@clean_test.multiply(a, @clean_test.@clean_test.add(b, c))
  let right_dist = @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(a, b), @clean_test.@clean_test.multiply(a, c))
  assert_eq(left_dist, right_dist)
  
  // 测试负数情况
  let x = -3
  let y = 7
  let z = -2
  
  let left_neg = @clean_test.@clean_test.multiply(x, @clean_test.@clean_test.add(y, z))
  let right_neg = @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(x, y), @clean_test.@clean_test.multiply(x, z))
  assert_eq(left_neg, right_neg)
}

// 测试 7: 特殊值组合测试
///|
test "special_value_combinations" {
  // 测试最小值与最大值的运算
  assert_eq(-1, @clean_test.@clean_test.add(2147483647, -2147483648))
  assert_eq(-1, @clean_test.@clean_test.add(-2147483648, 2147483647))
  
  // 测试最小值乘法
  assert_eq(-2147483648, @clean_test.@clean_test.multiply(-2147483648, 1))
  assert_eq(-2147483648, @clean_test.@clean_test.multiply(-2147483648, -1))
  
  // 测试最大值乘法
  assert_eq(2147483647, @clean_test.@clean_test.multiply(2147483647, 1))
  assert_eq(-2147483647, @clean_test.@clean_test.multiply(2147483647, -1))
}

// 测试 8: 数值范围边界测试
///|
test "numeric_range_boundaries" {
  // 测试接近边界的正数
  assert_eq(2147483646, @clean_test.@clean_test.add(2147483645, 1))
  assert_eq(2147483647, @clean_test.@clean_test.add(2147483646, 1))
  assert_eq(2147483647, @clean_test.@clean_test.add(2147483647, 1))
  
  // 测试接近边界的负数
  assert_eq(-2147483647, @clean_test.@clean_test.add(-2147483648, 1))
  assert_eq(-2147483646, @clean_test.@clean_test.add(-2147483647, 1))
  assert_eq(-2147483645, @clean_test.@clean_test.add(-2147483646, 1))
  
  // 测试大数乘法
  assert_eq(2147483647, @clean_test.@clean_test.multiply(46341, 46341))
  assert_eq(2147483647, @clean_test.@clean_test.multiply(46342, 46342))
}

// 测试 9: 交换律验证
///|
test "commutative_property" {
  // 加法交换律
  assert_eq(@clean_test.@clean_test.add(123, 456), @clean_test.@clean_test.add(456, 123))
  assert_eq(@clean_test.@clean_test.add(-100, 200), @clean_test.@clean_test.add(200, -100))
  
  // 乘法交换律
  assert_eq(@clean_test.@clean_test.multiply(12, 34), @clean_test.@clean_test.multiply(34, 12))
  assert_eq(@clean_test.@clean_test.multiply(-5, 7), @clean_test.@clean_test.multiply(7, -5))
}

// 测试 10: 实际应用场景 - 计算折扣
///|
test "discount_calculation" {
  // 原价
  let original_price = 1000
  
  // 折扣率 20% 表示为分数
  let discount_rate_numerator = 20
  let discount_rate_denominator = 100
  
  // 计算折扣金额
  let discount_amount = @clean_test.@clean_test.multiply(original_price, discount_rate_numerator)
  
  // 计算最终价格
  let final_price_times_denominator = @clean_test.@clean_test.add(
    @clean_test.@clean_test.multiply(original_price, discount_rate_denominator),
    @clean_test.@clean_test.multiply(-1, discount_amount)
  )
  
  // 验证: 1000 * 100 - 1000 * 20 = 80000
  assert_eq(80000, final_price_times_denominator)
}