// Azimuth 标准测试套件
// 使用标准的 MoonBit 测试语法

test "add_zero_cases" {
  // 测试加法中的零值处理
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-3, @azimuth.add(0, -3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
}

test "add_boundary_values" {
  // 测试加法的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 接近边界的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(1, max_val - 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(-1, min_val + 1))
}

test "multiply_by_zero_and_one" {
  // 测试乘以0和1的情况
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(12345, @azimuth.multiply(1, 12345))
  @azimuth.assert_eq(-12345, @azimuth.multiply(-12345, 1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(1, -12345))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
}

test "greet_with_various_inputs" {
  // 测试不同输入的问候功能
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, space separated!", @azimuth.greet("space separated"))
}

test "min_value_special_cases" {
  // 测试最小值的特殊情况
  let min_val = -2147483648
  
  // 最小值的加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, 1))
  
  // 最小值的乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(2, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "combined_operations" {
  // 测试组合运算
  let a = 100
  let b = -50
  let c = 25
  
  // (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(1250, result)
  
  // a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(-4975, result2)
}

test "assert_function_variants" {
  // 测试各种断言函数
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 != 1)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(0 != 0)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq(-100, -100)
  @azimuth.assert_eq_string("hello", "hello")
  @azimuth.assert_eq_string("", "")
}