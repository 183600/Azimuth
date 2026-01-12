test "add_large_numbers_near_max" {
  // 测试接近最大值的大数相加
  @clean_test.assert_eq(2147483647, @clean_test.add(2147483640, 10)) // 应该溢出
  @clean_test.assert_eq(2147483647, @clean_test.add(2147483630, 20)) // 应该溢出
  @clean_test.assert_eq(2147483647, @clean_test.add(2000000000, 147483648)) // 应该溢出
  @clean_test.assert_eq(2147483646, @clean_test.add(2147483630, 16)) // 不应该溢出
  @clean_test.assert_eq(2147483640, @clean_test.add(2147483630, 10)) // 不应该溢出
}

///|
test "add_alternating_signs" {
  // 测试交替符号的加法
  @clean_test.assert_eq(0, @clean_test.add(100, -100))
  @clean_test.assert_eq(50, @clean_test.add(100, -50))
  @clean_test.assert_eq(-50, @clean_test.add(-100, 50))
  @clean_test.assert_eq(1, @clean_test.add(2147483647, -2147483646))
  @clean_test.assert_eq(-1, @clean_test.add(-2147483647, 2147483646))
}

///|
test "multiply_fractions_edge" {
  // 测试大数乘法中的分数边界
  @clean_test.assert_eq(2147483647, @clean_test.multiply(46341, 46341)) // sqrt(INT_MAX) 的平方
  @clean_test.assert_eq(2147483647, @clean_test.multiply(46340, 46342)) // 交叉边界
  @clean_test.assert_eq(2147398401, @clean_test.multiply(46339, 46339)) // 不应该溢出
  @clean_test.assert_eq(2147483646, @clean_test.multiply(46340, 46341)) // 不应该溢出
}

///|
test "multiply_negative_patterns" {
  // 测试各种负数乘法模式
  @clean_test.assert_eq(2147483647, @clean_test.multiply(-46341, -46341)) // 负数乘负数应该正溢出
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(-46341, 46341)) // 负数乘正数应该负溢出
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(46341, -46341)) // 正数乘负数应该负溢出
  @clean_test.assert_eq(1000000, @clean_test.multiply(-1000, -1000)) // 负数乘负数，不溢出
  @clean_test.assert_eq(-1000000, @clean_test.multiply(-1000, 1000)) // 负数乘正数，不溢出
}

///|
test "greet_special_characters" {
  // 测试包含特殊字符的字符串
  @clean_test.assert_eq_string("Hello, @#$%^&*()!", @clean_test.greet("@#$%^&*()"))
  @clean_test.assert_eq_string("Hello, 中文测试!", @clean_test.greet("中文测试"))
  @clean_test.assert_eq_string("Hello,   !", @clean_test.greet("   ")) // 空格
  @clean_test.assert_eq_string("Hello, \n\t!", @clean_test.greet("\n\t")) // 换行和制表符
  @clean_test.assert_eq_string("Hello, 🌙!", @clean_test.greet("🌙")) // emoji
}

///|
test "add_zero_combinations" {
  // 测试各种与0相关的加法组合
  @clean_test.assert_eq(0, @clean_test.add(0, 0))
  @clean_test.assert_eq(2147483647, @clean_test.add(2147483647, 0))
  @clean_test.assert_eq(-2147483648, @clean_test.add(-2147483648, 0))
  @clean_test.assert_eq(123, @clean_test.add(0, 123))
  @clean_test.assert_eq(-456, @clean_test.add(0, -456))
  @clean_test.assert_eq(2147483647, @clean_test.add(0, 2147483647))
  @clean_test.assert_eq(-2147483648, @clean_test.add(0, -2147483648))
}

///|
test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  @clean_test.assert_eq(2, @clean_test.multiply(1, 2))
  @clean_test.assert_eq(4, @clean_test.multiply(2, 2))
  @clean_test.assert_eq(8, @clean_test.multiply(2, 4))
  @clean_test.assert_eq(16, @clean_test.multiply(4, 4))
  @clean_test.assert_eq(65536, @clean_test.multiply(256, 256))
  @clean_test.assert_eq(1048576, @clean_test.multiply(1024, 1024))
  @clean_test.assert_eq(2147483647, @clean_test.multiply(32768, 65536)) // 应该溢出
  @clean_test.assert_eq(2147483647, @clean_test.multiply(65536, 65536)) // 应该溢出
}

///|
test "add_consecutive_operations" {
  // 测试连续加法运算的一致性
  let a = 100
  let b = 200
  let c = 300
  // 测试 (a + b) + c == a + (b + c)
  @clean_test.assert_eq(@clean_test.add(@clean_test.add(a, b), c), @clean_test.add(a, @clean_test.add(b, c)))
  
  let x = -100
  let y = -200
  let z = -300
  // 测试负数情况
  @clean_test.assert_eq(@clean_test.add(@clean_test.add(x, y), z), @clean_test.add(x, @clean_test.add(y, z)))
  
  // 测试混合符号
  @clean_test.assert_eq(@clean_test.add(@clean_test.add(a, x), y), @clean_test.add(a, @clean_test.add(x, y)))
}

///|
test "multiply_distributive_property" {
  // 测试乘法分配律 (a * (b + c) == a * b + a * c)
  let a = 10
  let b = 20
  let c = 30
  @clean_test.assert_eq(@clean_test.multiply(a, @clean_test.add(b, c)), @clean_test.add(@clean_test.multiply(a, b), @clean_test.multiply(a, c)))
  
  // 测试负数情况
  let x = -10
  let y = 20
  let z = -30
  @clean_test.assert_eq(@clean_test.multiply(x, @clean_test.add(y, z)), @clean_test.add(@clean_test.multiply(x, y), @clean_test.multiply(x, z)))
}

///|
test "extreme_boundary_combinations" {
  // 测试极值边界组合
  let max = 2147483647
  let min = -2147483648
  
  // 最大值与接近最大值的运算
  @clean_test.assert_eq(max, @clean_test.add(max, 1))
  @clean_test.assert_eq(max, @clean_test.add(max - 1, 2))
  @clean_test.assert_eq(max, @clean_test.add(max - 100, 101))
  
  // 最小值与接近最小值的运算
  @clean_test.assert_eq(min, @clean_test.add(min, -1))
  @clean_test.assert_eq(min, @clean_test.add(min + 1, -2))
  @clean_test.assert_eq(min, @clean_test.add(min + 100, -101))
  
  // 最大值与最小值的乘法
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(max, min))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(min, max))
}
