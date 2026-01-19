// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

// 测试加法函数的基本功能
test "add_basic_functionality" {
  // 基本加法测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-1, -2))
  @azimuth.assert_eq(1, @azimuth.add(-2, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

// 测试乘法函数的基本功能
test "multiply_basic_functionality" {
  // 基本乘法测试
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

// 测试字符串问候函数
test "greet_functionality" {
  // 基本问候测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

// 测试向上取整除法函数
test "divide_with_ceil_functionality" {
  // 基本除法测试
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(10, 5))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))  // 10/4 = 2.5 -> 3
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(11, 4))  // 11/4 = 2.75 -> 3
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-10, 5))  // -10/5 = -2 -> -2
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-9, 5))  // -9/5 = -1.8 -> -1 -> 需要确认实际行为
}

// 测试加法函数的溢出保护
test "add_overflow_protection" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 负数溢出测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 边界值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

// 测试乘法函数的溢出保护
test "multiply_overflow_protection" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大数相乘溢出测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))  // sqrt(max_val) * sqrt(max_val)
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值特殊处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值测试
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

// 测试断言函数
test "assertion_functions" {
  // 测试 assert_true 和 assert_false
  @azimuth.assert_true(true)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 < 1)
  
  @azimuth.assert_false(false)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(0 > 1)
}

// 测试复合运算
test "compound_operations" {
  // 复合运算测试：(a + b) * c
  let a = 10
  let b = 20
  let c = 3
  
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(90, result)  // (10 + 20) * 3 = 90
  
  // 另一个复合运算：a * b + c * d
  let d = 5
  let product1 = @azimuth.multiply(a, b)  // 200
  let product2 = @azimuth.multiply(c, d)  // 15
  let total = @azimuth.add(product1, product2)
  @azimuth.assert_eq(215, total)  // 200 + 15 = 215
}

// 测试边界值和特殊情况
test "edge_cases_and_special_values" {
  // 测试 0 和 1 的特殊情况
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(-1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  
  // 测试负数运算
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2, @azimuth.add(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
}