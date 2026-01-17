// 新的标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  
  // 测试负数情况
  let c = -10
  let d = 20
  assert_eq(add(c, d), add(d, c))
}

test "add_associative_property" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 5
  let b = 10
  let c = 15
  
  let result1 = add(add(a, b), c)
  let result2 = add(a, add(b, c))
  assert_eq(result1, result2)
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 3
  let b = 4
  let c = 5
  
  let result1 = multiply(a, add(b, c))
  let result2 = add(multiply(a, b), multiply(a, c))
  assert_eq(result1, result2)
}

test "multiply_identity_elements" {
  // 测试乘法单位元
  let a = 42
  assert_eq(a, multiply(a, 1))
  assert_eq(a, multiply(1, a))
  assert_eq(0, multiply(a, 0))
  assert_eq(0, multiply(0, a))
}

test "greet_unicode_handling" {
  // 测试 greet 函数的 Unicode 处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
}

test "mixed_operations_complexity" {
  // 测试混合运算的复杂性
  let x = 8
  let y = 6
  let z = 4
  
  // 计算：(x + y) × z - x
  let result = add(multiply(add(x, y), z), -x)
  assert_eq(44, result)
  
  // 计算：x × y + y × z
  let result2 = add(multiply(x, y), multiply(y, z))
  assert_eq(72, result2)
}

test "edge_case_zero_handling" {
  // 测试零值处理的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(5, add(0, 5))
  assert_eq(-3, add(-3, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, multiply(-9876, 0))
}

test "large_number_operations" {
  // 测试大数运算
  let large1 = 1000000
  let large2 = 2000000
  
  assert_eq(3000000, add(large1, large2))
  assert_eq(2000000000, multiply(large1, 1000))
  
  // 测试大数与边界值的组合
  let max_val = 2147483647
  assert_eq(max_val, add(max_val, 1000000))
}

test "negative_number_properties" {
  // 测试负数性质
  let a = -10
  let b = -20
  let c = 30
  
  // 负数加法
  assert_eq(-30, add(a, b))
  assert_eq(20, add(a, c))
  
  // 负数乘法
  assert_eq(200, multiply(a, b))
  assert_eq(-300, multiply(a, c))
  
  // 负数与零的运算
  assert_eq(-10, add(a, 0))
  assert_eq(0, multiply(a, 0))
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, newline\n!", greet("newline\n"))
  assert_eq_string("Hello, tab\t!", greet("tab\t"))
}