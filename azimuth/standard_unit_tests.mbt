// 标准 MoonBit 单元测试文件
// 包含 10 个核心测试用例，覆盖 azimuth 包的主要功能

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
}

test "add_overflow_boundary" {
  // 测试加法溢出边界处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(1000000000, 2000000000))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(-1000000000, -2000000000))
}

test "multiply_function_basic" {
  // 测试乘法函数基本功能
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(123, @azimuth.multiply(1, 123))
  @azimuth.assert_eq(123, @azimuth.multiply(123, 1))
  @azimuth.assert_eq(-123, @azimuth.multiply(-1, 123))
  @azimuth.assert_eq(-123, @azimuth.multiply(123, -1))
  @azimuth.assert_eq(144, @azimuth.multiply(12, 12))
}

test "multiply_edge_values" {
  // 测试乘法函数边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(1, max_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
}

test "greet_function_variations" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, space separated!", @azimuth.greet("space separated"))
}

test "greet_unicode_support" {
  // 测试问候函数的 Unicode 支持
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
}

test "divide_with_ceil_positive" {
  // 测试正数的向上取整除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(6, 2))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(7, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(10, @azimuth.divide_with_ceil(19, 2))
}

test "divide_with_ceil_negative" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-6, 2))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-7, 2))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-4, 2))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 2))
  @azimuth.assert_eq(-9, @azimuth.divide_with_ceil(-19, 2))
}

test "complex_combined_operations" {
  // 测试复合运算
  let a = 10
  let b = 5
  let c = -3
  
  // (a + b) * c
  let sum = @azimuth.add(a, b)
  let result1 = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(-45, result1)
  
  // a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(47, result2)
  
  // 复杂表达式验证
  let x = @azimuth.add(@azimuth.multiply(3, 4), @azimuth.multiply(2, 5))
  @azimuth.assert_eq(22, x)
}