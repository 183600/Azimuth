// Azimuth 项目综合测试套件
// 使用标准 MoonBit 测试语法，包含多样化的测试场景

test "integer_arithmetic_edge_cases" {
  // 测试整数算术的边界情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(5, -6))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
}

test "add_function_comprehensive" {
  // 测试 add 函数的全面功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
}

test "multiply_function_comprehensive" {
  // 测试 multiply 函数的全面功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-42, @azimuth.multiply(-1, 42))
}

test "mathematical_expressions" {
  // 测试数学表达式组合
  let a = 5
  let b = 3
  let c = 2
  
  let result1 = @azimuth.add(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(11, result1)  // 5 + (3 * 2) = 11
  
  let result2 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(16, result2)  // (5 + 3) * 2 = 16
}

test "edge_case_operations" {
  // 测试边界情况操作
  let large_num = 1000000
  let small_num = 1
  
  @azimuth.assert_eq(1000001, @azimuth.add(large_num, small_num))
  @azimuth.assert_eq(1000000, @azimuth.multiply(large_num, small_num))
  @azimuth.assert_eq(0, @azimuth.multiply(large_num, 0))
}

test "string_operations_extended" {
  // 测试扩展字符串操作
  let name1 = "Alice"
  let name2 = "Bob"
  let name3 = "张三"
  
  let greeting1 = @azimuth.greet(name1)
  let greeting2 = @azimuth.greet(name2)
  let greeting3 = @azimuth.greet(name3)
  
  @azimuth.assert_eq_string("Hello, Alice!", greeting1)
  @azimuth.assert_eq_string("Hello, Bob!", greeting2)
  @azimuth.assert_eq_string("Hello, 张三!", greeting3)
}

test "boolean_assertions" {
  // 测试布尔断言
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_false(3 > 5)
}

test "complex_calculations" {
  // 测试复杂计算
  let x = 10
  let y = 20
  let z = 5
  
  // 计算表达式: x + y * z
  let result1 = @azimuth.add(x, @azimuth.multiply(y, z))
  @azimuth.assert_eq(110, result1)  // 10 + 20 * 5 = 110
  
  // 计算表达式: (x + y) * z
  let result2 = @azimuth.multiply(@azimuth.add(x, y), z)
  @azimuth.assert_eq(150, result2)  // (10 + 20) * 5 = 150
}

test "performance_patterns" {
  // 测试性能模式
  let base = 42
  
  // 重复加法
  let result1 = @azimuth.add(@azimuth.add(@azimuth.add(base, base), base), base)
  @azimuth.assert_eq(168, result1)  // 42 * 4 = 168
  
  // 混合运算
  let result2 = @azimuth.multiply(@azimuth.add(base, base), 2)
  @azimuth.assert_eq(168, result2)  // (42 + 42) * 2 = 168
}