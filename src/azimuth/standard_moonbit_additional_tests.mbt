// 标准 MoonBit 额外测试用例
// 测试 azimuth 包的核心功能

test "arithmetics_addition_properties" {
  // 测试加法的数学性质
  let a = 42
  let b = 17
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 加法结合律
  let c = 23
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 零元素性质
  assert_eq(a, add(a, 0))
  assert_eq(b, add(0, b))
}

test "arithmetics_multiplication_properties" {
  // 测试乘法的数学性质
  let a = 6
  let b = 7
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 乘法结合律
  let c = 3
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 单位元素性质
  assert_eq(a, multiply(a, 1))
  assert_eq(b, multiply(1, b))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(0, multiply(0, b))
}

test "arithmetics_distributive_property" {
  // 测试分配律：a × (b + c) = a × b + a × c
  let a = 5
  let b = 8
  let c = 12
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "string_greeting_functionality" {
  // 测试字符串问候功能的各种情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, user@example.com!", greet("user@example.com"))
}

test "arithmetics_negative_numbers" {
  // 测试负数运算
  let positive = 15
  let negative = -8
  
  // 正负数相加
  assert_eq(7, add(positive, negative))
  assert_eq(-7, add(negative, positive))
  
  // 负数相加
  assert_eq(-16, add(negative, negative))
  
  // 正负数相乘
  assert_eq(-120, multiply(positive, negative))
  assert_eq(-120, multiply(negative, positive))
  
  // 负数相乘
  assert_eq(64, multiply(negative, negative))
}

test "arithmetics_boundary_values" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "practical_calculations_finance" {
  // 实际金融计算场景
  let principal = 10000  // 本金
  let rate = 5           // 利率 5%
  let years = 3          // 年数
  
  // 简单利息计算：本金 + (本金 × 利率 × 年数)
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  
  assert_eq(160000, total)  // 10000 + (10000 × 5 × 3) = 160000
}

test "practical_calculations_geometry" {
  // 实际几何计算场景
  let length = 12
  let width = 8
  
  // 矩形面积
  let area = multiply(length, width)
  assert_eq(96, area)
  
  // 矩形周长
  let perimeter = multiply(2, add(length, width))
  assert_eq(40, perimeter)
  
  // 面积与周长的总和
  let total = add(area, perimeter)
  assert_eq(136, total)
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  let base = 10
  let exponent = 3
  
  // 计算 x³ (通过连续相乘)
  let power = multiply(multiply(base, base), base)
  assert_eq(1000, power)
  
  // 计算 (a + b) × (c + d)
  let a = 5
  let b = 3
  let c = 7
  let d = 2
  
  let sum1 = add(a, b)
  let sum2 = add(c, d)
  let result = multiply(sum1, sum2)
  assert_eq(80, result)
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试边界条件不会导致错误
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  
  // 测试正常计算
  assert_true(add(10, 20) == 30)
  assert_true(multiply(6, 7) == 42)
  assert_true(greet("Test") == "Hello, Test!")
}