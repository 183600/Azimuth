// 新增的 MoonBit 测试用例
// 这些测试用例涵盖了不同的测试场景和边界情况

// 测试加法函数的零值处理
test "add_zero_handling" {
  // 测试各种零值组合
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(123, @azimuth.add(0, 123))
  @azimuth.assert_eq(123, @azimuth.add(123, 0))
  @azimuth.assert_eq(-456, @azimuth.add(0, -456))
  @azimuth.assert_eq(-456, @azimuth.add(-456, 0))
}

// 测试乘法函数的零值和单位元处理
test "multiply_identity_and_zero" {
  // 测试零值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -456))
  @azimuth.assert_eq(0, @azimuth.multiply(-456, 0))
  
  // 测试单位元乘法
  @azimuth.assert_eq(123, @azimuth.multiply(1, 123))
  @azimuth.assert_eq(123, @azimuth.multiply(123, 1))
  @azimuth.assert_eq(-456, @azimuth.multiply(1, -456))
  @azimuth.assert_eq(-456, @azimuth.multiply(-456, 1))
}

// 测试乘法函数的负一处理
test "multiply_negative_one" {
  // 测试负一乘法
  @azimuth.assert_eq(123, @azimuth.multiply(-1, -123))
  @azimuth.assert_eq(-123, @azimuth.multiply(-1, 123))
  @azimuth.assert_eq(-123, @azimuth.multiply(1, -123))
  @azimuth.assert_eq(123, @azimuth.multiply(-1, -123))
  
  // 测试最小值与负一的乘法（特殊情况）
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
}

// 测试加法函数的负数处理
test "add_negative_numbers" {
  // 测试两个负数相加
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(-1000, @azimuth.add(-600, -400))
  
  // 测试正数和负数相加
  @azimuth.assert_eq(0, @azimuth.add(10, -10))
  @azimuth.assert_eq(-5, @azimuth.add(5, -10))
  @azimuth.assert_eq(5, @azimuth.add(10, -5))
}

// 测试乘法函数的负数处理
test "multiply_negative_numbers" {
  // 测试两个负数相乘
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  @azimuth.assert_eq(1000, @azimuth.multiply(-20, -50))
  
  // 测试正数和负数相乘
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
}

// 测试 greet 函数的不同输入
test "greet_various_inputs" {
  // 测试单字符输入
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试长字符串输入
  @azimuth.assert_eq_string("Hello, This is a very long string!", @azimuth.greet("This is a very long string"))
  
  // 测试包含数字和字母的字符串
  @azimuth.assert_eq_string("Hello, Test123!", @azimuth.greet("Test123"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
}

// 测试加法和乘法的组合运算
test "combined_operations" {
  // 测试 (a + b) * c
  let a = 5
  let b = 3
  let c = 2
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(16, result)
  
  // 测试 a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(17, result2)
  
  // 测试 a + b * c
  let product2 = @azimuth.multiply(b, c)
  let result3 = @azimuth.add(a, product2)
  @azimuth.assert_eq(11, result3)
}

// 测试边界值的加法运算
test "add_boundary_edge_cases" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的值
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 2, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 100, 100))
  
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val + 2, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 100, -100))
  
  // 测试边界值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val + 1))
}

// 测试边界值的乘法运算
test "multiply_boundary_edge_cases" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值与2的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -2))
  
  // 测试边界值与0和1的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试边界值与-1的乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}