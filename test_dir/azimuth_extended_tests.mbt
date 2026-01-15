// 额外的 MoonBit 测试用例
// 这些测试用例使用标准的 MoonBit 测试语法，为 azimuth 模块提供额外的测试覆盖

// 导入主模块中的函数
use azimuth

test "arithmetics_zero_handling" {
  // 测试零值在算术运算中的处理
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
}

test "arithmetics_negative_numbers" {
  // 测试负数算术运算
  assert_eq(-10, add(-5, -5))
  assert_eq(15, add(20, -5))
  assert_eq(-15, add(-20, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-30, multiply(6, -5))
  assert_eq(30, multiply(-6, -5))
}

test "greet_function_special_cases" {
  // 测试 greet 函数的特殊情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, a_b_c!", greet("a_b_c"))
  assert_eq_string("Hello, line1
line2!", greet("line1
line2"))
}

test "combined_operations_complex" {
  // 测试复杂的组合运算
  let x = 7
  let y = 11
  let z = 13
  
  // (x * y) + (x * z)
  let result1 = add(multiply(x, y), multiply(x, z))
  assert_eq(168, result1)
  
  // (x + y) * (x + z)
  let result2 = multiply(add(x, y), add(x, z))
  assert_eq(324, result2)
  
  // x * y * z
  let result3 = multiply(multiply(x, y), z)
  assert_eq(1001, result3)
}

test "practical_calculations" {
  // 测试实际应用场景的计算
  // 计算三角形面积：底 * 高 / 2
  let base = 12
  let height = 8
  let triangle_area = multiply(base, height) / 2
  assert_eq(48, triangle_area)
  
  // 计算平均数：(a + b + c) / 3
  let a = 15
  let b = 25
  let c = 35
  let sum = add(add(a, b), c)
  let average = sum / 3
  assert_eq(25, average)
}

test "string_boundary_cases" {
  // 测试字符串边界情况
  // 测试单个字符
  assert_eq_string("Hello, x!", greet("x"))
  
  // 测试空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 测试长字符串
  let long_string = "ThisIsAVeryLongStringThatMightCauseIssues"
  assert_eq_string("Hello, ThisIsAVeryLongStringThatMightCauseIssues!", greet(long_string))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let a = 42
  
  // 加法单位元
  assert_eq(a, add(a, 0))
  assert_eq(a, add(0, a))
  
  // 乘法单位元
  assert_eq(a, multiply(a, 1))
  assert_eq(a, multiply(1, a))
  
  // 乘法零元
  assert_eq(0, multiply(a, 0))
  assert_eq(0, multiply(0, a))
}

test "overflow_behavior" {
  // 测试溢出行为
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 确保溢出被正确处理（不崩溃）
  let overflow_add = add(max_int, max_int)
  let overflow_mul = multiply(max_int, 2)
  let underflow_add = add(min_int, min_int)
  let underflow_mul = multiply(min_int, 2)
  
  // 验证结果在合理范围内（不会崩溃）
  assert_true(overflow_add <= max_int)
  assert_true(overflow_mul <= max_int)
  assert_true(underflow_add >= min_int)
  assert_true(underflow_mul >= min_int)
}