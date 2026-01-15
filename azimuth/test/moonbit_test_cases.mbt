// MoonBit 标准测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
}

test "add_function_overflow_handling" {
  // 测试加法函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "multiply_function_basic_functionality" {
  // 测试乘法函数的基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(15, @azimuth.multiply(5, 1))
}

test "multiply_function_overflow_handling" {
  // 测试乘法函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "greet_function_basic_functionality" {
  // 测试问候函数的基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(10, 20), @azimuth.add(20, 10))
  @azimuth.assert_eq(@azimuth.add(-5, 15), @azimuth.add(15, -5))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(6, 7), @azimuth.multiply(7, 6))
  @azimuth.assert_eq(@azimuth.multiply(-4, 8), @azimuth.multiply(8, -4))
}

test "mathematical_properties_identity" {
  // 测试数学单位元
  // 加法单位元: a + 0 = a
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  
  // 乘法单位元: a * 1 = a
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
  
  // 乘法零性质: a * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-17, 0))
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  let a = 10
  let b = 20
  let c = -5
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(-150, result1)  // (10 + 20) * (-5) = 30 * (-5) = -150
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(195, result2)  // 10 * 20 + (-5) = 200 - 5 = 195
  
  // 分配律: a * (b + c) = a * b + a * c
  let distributive_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distributive_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distributive_left, distributive_right)
}

test "assertion_functions" {
  // 测试断言函数
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_true(-10 < 0)
  
  @azimuth.assert_false(5 < 3)
  @azimuth.assert_false(0 != 0)
  @azimuth.assert_false(-10 > 0)
  
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(-17, -17)
  @azimuth.assert_eq(0, 0)
  
  @azimuth.assert_eq_string("Hello", "Hello")
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("MoonBit", "MoonBit")
}