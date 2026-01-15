// 新增的 MoonBit 测试用例
// 专注于测试一些新的场景和边界情况

test "divide_with_ceil_basic" {
  // 测试基本的向上取整除法
  azimuth::assert_eq(3, azimuth::divide_with_ceil(10, 4))
  azimuth::assert_eq(5, azimuth::divide_with_ceil(20, 4))
  azimuth::assert_eq(4, azimuth::divide_with_ceil(15, 4))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 5))
}

test "divide_with_ceil_simple" {
  // 测试简单的向上取整除法
  azimuth::assert_eq(2, azimuth::divide_with_ceil(5, 3))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(3, 3))
  azimuth::assert_eq(2, azimuth::divide_with_ceil(4, 2))
}

test "divide_with_ceil_negative_simple" {
  // 测试负数的向上取整除法（简化版）
  azimuth::assert_eq(-2, azimuth::divide_with_ceil(-10, 5))
  azimuth::assert_eq(-1, azimuth::divide_with_ceil(-5, 5))
}

test "fibonacci_simple" {
  // 简单的斐波那契数列测试
  let f0 = 0
  let f1 = 1
  let f2 = azimuth::add(f0, f1)
  let f3 = azimuth::add(f1, f2)
  
  azimuth::assert_eq(1, f2)
  azimuth::assert_eq(2, f3)
}

test "string_greet_variations" {
  // 测试不同类型的字符串问候
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, 123!", azimuth::greet("123"))
}

test "math_operations_sequence" {
  // 测试数学运算序列
  let a = 10
  let b = 20
  let sum = azimuth::add(a, b)
  let product = azimuth::multiply(a, b)
  
  azimuth::assert_eq(30, sum)
  azimuth::assert_eq(200, product)
}

test "edge_case_zero_operations" {
  // 测试零的边界情况
  azimuth::assert_eq(0, azimuth::multiply(0, 12345))
  azimuth::assert_eq(12345, azimuth::add(0, 12345))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 5))
}

test "negative_operations" {
  // 测试负数运算
  azimuth::assert_eq(-10, azimuth::add(-5, -5))
  azimuth::assert_eq(15, azimuth::multiply(-3, -5))
  azimuth::assert_eq(-15, azimuth::multiply(3, -5))
}

test "large_number_operations" {
  // 测试大数运算
  let large1 = 1000000
  let large2 = 2000000
  let sum = azimuth::add(large1, large2)
  let product = azimuth::multiply(1000, 1000)
  
  azimuth::assert_eq(3000000, sum)
  azimuth::assert_eq(1000000, product)
}

test "complex_expression" {
  // 测试复杂表达式
  let result = azimuth::add(azimuth::multiply(2, 3), azimuth::multiply(4, 5))
  azimuth::assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}