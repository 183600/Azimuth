// 额外的 MoonBit 测试用例
// 测试 azimuth 包中的高级功能和边界情况

// 测试用例1: 测试 add 函数的零值特性
test "add_zero_properties" {
  // 加法零元特性：a + 0 = a
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(15, 27), @azimuth.add(27, 15))
  @azimuth.assert_eq(@azimuth.add(-10, 20), @azimuth.add(20, -10))
}

// 测试用例2: 测试 add 函数的大数处理
test "add_large_numbers" {
  // 测试接近边界的大数
  let near_max = 2147483640
  let near_min = -2147483640
  
  @azimuth.assert_eq(2147483647, @azimuth.add(near_max, 7))
  @azimuth.assert_eq(-2147483648, @azimuth.add(near_min, -8))
  
  // 测试大数相加
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483640, 7))
}

// 测试用例3: 测试 multiply 函数的幂等特性
test "multiply_idempotent_properties" {
  // 乘法单位元：a * 1 = a
  @azimuth.assert_eq(37, @azimuth.multiply(37, 1))
  @azimuth.assert_eq(-42, @azimuth.multiply(-42, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 1))
  
  // 零元特性：a * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(999999, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-999999, 0))
  
  // 交换律：a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(12, 15), @azimuth.multiply(15, 12))
  @azimuth.assert_eq(@azimuth.multiply(-8, 7), @azimuth.multiply(7, -8))
}

// 测试用例4: 测试 multiply 函数的负数平方
test "multiply_negative_squares" {
  // 负数平方应该为正
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(144, @azimuth.multiply(-12, -12))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
  
  // 零的平方
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

// 测试用例5: 测试 greet 函数的边界情况
test "greet_edge_cases" {
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, Moon Bit!", @azimuth.greet("Moon Bit"))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
}

// 测试用例6: 测试复合运算的分配律
test "distributive_properties" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 3
  let c = 7
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
  
  // 另一个例子
  let x = 8
  let y = 2
  let z = 4
  
  let left = @azimuth.multiply(x, @azimuth.add(y, z))
  let right = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  
  @azimuth.assert_eq(left, right)
}

// 测试用例7: 测试极端边界值的组合运算
test "extreme_boundary_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的加法
  let result = @azimuth.add(max_val, min_val)
  @azimuth.assert_eq(-1, result)
  
  // 最大值与-1的乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 最小值与1的乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

// 测试用例8: 测试链式运算
test "chained_operations" {
  // 链式加法
  let add_chain = @azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4)
  @azimuth.assert_eq(10, add_chain)
  
  // 链式乘法
  let mul_chain = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 3), 4), 5)
  @azimuth.assert_eq(120, mul_chain)
  
  // 混合链式运算
  let mixed_chain = @azimuth.multiply(@azimuth.add(10, 5), @azimuth.add(3, 2))
  @azimuth.assert_eq(75, mixed_chain)
}

// 测试用例9: 测试断言函数的逻辑组合
test "assertion_logic_combinations" {
  // 测试复合布尔表达式
  @azimuth.assert_true(true && true)
  @azimuth.assert_true(true || false)
  @azimuth.assert_false(false && true)
  @azimuth.assert_false(false || false)
  
  // 测试数值比较
  @azimuth.assert_true(10 > 5)
  @azimuth.assert_true(5 >= 5)
  @azimuth.assert_true(3 < 8)
  @azimuth.assert_true(8 <= 8)
  
  @azimuth.assert_false(10 < 5)
  @azimuth.assert_false(5 > 8)
}

// 测试用例10: 测试字符串处理的边界情况
test "string_processing_edge_cases" {
  // 测试长字符串
  let long_name = "This is a very long name that should still work properly with the greet function"
  let expected_long = "Hello, " + long_name + "!"
  @azimuth.assert_eq_string(expected_long, @azimuth.greet(long_name))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, \\\"!", @azimuth.greet("\\\""))
  
  // 测试数字字符串
  @azimuth.assert_eq_string("Hello, 123456789!", @azimuth.greet("123456789"))
}