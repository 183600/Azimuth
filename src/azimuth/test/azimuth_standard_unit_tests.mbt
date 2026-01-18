// Azimuth 项目标准测试用例
// 测试项目的核心功能，使用标准 MoonBit 测试语法

test "add_positive_numbers" {
  // 测试正数相加
  assert_eq(5, add(2, 3))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-5, add(-2, -3))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(-5, 5))
}

test "add_mixed_numbers" {
  // 测试正负数混合相加
  assert_eq(2, add(5, -3))
  assert_eq(-2, add(-5, 3))
  assert_eq(10, add(15, -5))
}

test "multiply_positive_numbers" {
  // 测试正数相乘
  assert_eq(6, multiply(2, 3))
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(0, 5))
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  assert_eq(6, multiply(-2, -3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(-5, 0))
}

test "greet_standard_names" {
  // 测试标准名称问候
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, Alice!", greet("Alice"))
}

test "greet_edge_cases" {
  // 测试边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello,   !", greet("   "))
}

test "complex_calculations" {
  // 测试复合计算
  // (a + b) * c
  let result1 = multiply(add(2, 3), 4)
  assert_eq(20, result1)
  
  // a * b + c * d
  let result2 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result2)
}

test "mathematical_properties" {
  // 测试数学性质
  // 加法交换律
  assert_eq(add(5, 3), add(3, 5))
  
  // 乘法交换律
  assert_eq(multiply(5, 3), multiply(3, 5))
  
  // 分配律
  assert_eq(multiply(2, add(3, 4)), add(multiply(2, 3), multiply(2, 4)))
}