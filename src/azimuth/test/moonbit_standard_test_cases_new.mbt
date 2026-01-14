// 标准 MoonBit 测试用例
// 这些测试用例覆盖了 azimuth 包的基本功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  let result1 = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result1)
  
  let result2 = @azimuth.add(0, 0)
  @azimuth.assert_eq(0, result2)
  
  let result3 = @azimuth.add(-2, -3)
  @azimuth.assert_eq(-5, result3)
}

test "add_with_zero" {
  // 测试与零相加的情况
  let result1 = @azimuth.add(5, 0)
  @azimuth.assert_eq(5, result1)
  
  let result2 = @azimuth.add(0, 5)
  @azimuth.assert_eq(5, result2)
  
  let result3 = @azimuth.add(0, 0)
  @azimuth.assert_eq(0, result3)
}

test "add_negative_numbers" {
  // 测试负数加法
  let result1 = @azimuth.add(2, -3)
  @azimuth.assert_eq(-1, result1)
  
  let result2 = @azimuth.add(-2, 3)
  @azimuth.assert_eq(1, result2)
  
  let result3 = @azimuth.add(-7, -3)
  @azimuth.assert_eq(-10, result3)
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  let result1 = @azimuth.multiply(2, 3)
  @azimuth.assert_eq(6, result1)
  
  let result2 = @azimuth.multiply(5, 0)
  @azimuth.assert_eq(0, result2)
  
  let result3 = @azimuth.multiply(5, 1)
  @azimuth.assert_eq(5, result3)
  
  let result4 = @azimuth.multiply(2, -3)
  @azimuth.assert_eq(-6, result4)
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  let result1 = @azimuth.multiply(-2, -3)
  @azimuth.assert_eq(6, result1)
  
  let result2 = @azimuth.multiply(2, -3)
  @azimuth.assert_eq(-6, result2)
  
  let result3 = @azimuth.multiply(-2, 3)
  @azimuth.assert_eq(-6, result3)
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  let result1 = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result1)
  
  let result2 = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result2)
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  let result1 = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result1)
  
  let result2 = @azimuth.greet("测试")
  @azimuth.assert_eq_string("Hello, 测试!", result2)
  
  let result3 = @azimuth.greet("@123")
  @azimuth.assert_eq_string("Hello, @123!", result3)
}

test "mathematical_combinations" {
  // 测试数学运算组合
  let a = 10
  let b = 20
  
  // (a + b) * 2
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, 2)
  @azimuth.assert_eq(60, result)
  
  // a * 2 + b * 3
  let result2 = @azimuth.add(@azimuth.multiply(a, 2), @azimuth.multiply(b, 3))
  @azimuth.assert_eq(80, result2)
}

test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值相加
  let result1 = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, result1)
  
  let result2 = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, result2)
  
  // 边界值相乘
  let result3 = @azimuth.multiply(max_val, 0)
  @azimuth.assert_eq(0, result3)
  
  let result4 = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, result4)
}