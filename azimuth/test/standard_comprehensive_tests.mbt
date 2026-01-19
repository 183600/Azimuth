// 标准综合测试套件
// 测试 azimuth 包中的核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(0, 100))
  @azimuth.assert_eq(-50, @azimuth.add(-50, 0))
}

test "addition_overflow_handling" {
  // 测试加法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 负数溢出测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 边界值测试
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(10, 0))
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(-7, @azimuth.multiply(7, -1))
}

test "multiplication_overflow_handling" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  
  // 负数溢出测试
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  
  // 最小值特殊处理测试
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_function_functionality" {
  // 测试问候函数功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-10, -3))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 12
  let b = 8
  let c = -5
  
  // 计算 (a + b) * c
  let sum = @azimuth.add(a, b)
  let result1 = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(-100, result1)
  
  // 计算 a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(91, result2)
  
  // 计算 (a - b) * (b + c)
  let diff = @azimuth.add(a, -b)
  let sum2 = @azimuth.add(b, c)
  let result3 = @azimuth.multiply(diff, sum2)
  @azimuth.assert_eq(12, result3)
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -1))
  
  // 边界值乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(46340, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46340, 46341))
  
  // 边界值与零的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  // 负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-7, @azimuth.add(-10, 3))
  @azimuth.assert_eq(3, @azimuth.add(-7, 10))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  
  // 负数乘法
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-21, @azimuth.multiply(-3, 7))
  @azimuth.assert_eq(-21, @azimuth.multiply(3, -7))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
  
  // 负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
}

test "special_character_handling" {
  // 测试特殊字符处理
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, emoji😊!", @azimuth.greet("emoji😊"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  @azimuth.assert_eq_string("Hello, \\path\\!", @azimuth.greet("\\path\\"))
  @azimuth.assert_eq_string("Hello, \t\n\r!", @azimuth.greet("\t\n\r"))
}