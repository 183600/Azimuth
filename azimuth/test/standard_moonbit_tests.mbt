// 标准 MoonBit 测试用例
// 测试 azimuth 包中的核心功能

// 测试用例1: 测试 add 函数的基本功能
test "add_basic_functionality" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

// 测试用例2: 测试 add 函数的混合符号处理
test "add_mixed_signs" {
  @azimuth.assert_eq(1, @azimuth.add(5, -4))
  @azimuth.assert_eq(-1, @azimuth.add(-5, 4))
  @azimuth.assert_eq(10, @azimuth.add(15, -5))
}

// 测试用例3: 测试 add 函数的边界值处理
test "add_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 测试零值边界
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
}

// 测试用例4: 测试 multiply 函数的基本功能
test "multiply_basic_operations" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(-7, @azimuth.multiply(7, -1))
}

// 测试用例5: 测试 multiply 函数的负数处理
test "multiply_negative_numbers" {
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
}

// 测试用例6: 测试 multiply 函数的边界值处理
test "multiply_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试最小值的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

// 测试用例7: 测试 greet 函数的基本功能
test "greet_basic_functionality" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

// 测试用例8: 测试 greet 函数的特殊字符处理
test "greet_special_characters" {
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

// 测试用例9: 测试 assert_true 和 assert_false 函数
test "assertion_functions" {
  // 测试 assert_true
  @azimuth.assert_true(true)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true("hello" == "hello")
  
  // 测试 assert_false
  @azimuth.assert_false(false)
  @azimuth.assert_false(1 == 2)
  @azimuth.assert_false("hello" == "world")
}

// 测试用例10: 测试复合运算
test "compound_operations" {
  // 测试多个 add 操作的组合
  let result1 = @azimuth.add(@azimuth.add(2, 3), @azimuth.add(4, 5))
  @azimuth.assert_eq(14, result1)
  
  // 测试多个 multiply 操作的组合
  let result2 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result2)
  
  // 测试 add 和 multiply 的混合运算
  let result3 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result3)
}