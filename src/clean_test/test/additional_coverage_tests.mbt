// 额外的测试用例，提供更全面的覆盖率

///|
test "add_large_numbers_near_max" {
  // 测试接近最大值的大数相加
  assert_eq(2147483647, @azimuth.add(2147483640, 10)) // 应该溢出
  assert_eq(2147483647, @azimuth.add(2147483630, 20)) // 应该溢出
  assert_eq(2147483647, @azimuth.add(2000000000, 147483648)) // 应该溢出
  assert_eq(2147483646, @azimuth.add(2147483630, 16)) // 不应该溢出
  assert_eq(2147483640, @azimuth.add(2147483630, 10)) // 不应该溢出
}

///|
test "add_alternating_signs" {
  // 测试交替符号的加法
  assert_eq(0, @azimuth.add(100, -100))
  assert_eq(50, @azimuth.add(100, -50))
  assert_eq(-50, @azimuth.add(-100, 50))
  assert_eq(1, @azimuth.add(2147483647, -2147483646))
  assert_eq(-1, @azimuth.add(-2147483647, 2147483646))
}

///|
test "multiply_fractions_edge" {
  // 测试大数乘法中的分数边界
  assert_eq(2147483647, @azimuth.multiply(46341, 46341)) // sqrt(INT_MAX) 的平方
  assert_eq(2147483647, @azimuth.multiply(46340, 46342)) // 交叉边界
  assert_eq(2147398401, @azimuth.multiply(46339, 46339)) // 不应该溢出
  assert_eq(2147483646, @azimuth.multiply(46340, 46341)) // 不应该溢出
}

///|
test "multiply_negative_patterns" {
  // 测试各种负数乘法模式
  assert_eq(2147483647, @azimuth.multiply(-46341, -46341)) // 负数乘负数应该正溢出
  assert_eq(-2147483648, @azimuth.multiply(-46341, 46341)) // 负数乘正数应该负溢出
  assert_eq(-2147483648, @azimuth.multiply(46341, -46341)) // 正数乘负数应该负溢出
  assert_eq(1000000, @azimuth.multiply(-1000, -1000)) // 负数乘负数，不溢出
  assert_eq(-1000000, @azimuth.multiply(-1000, 1000)) // 负数乘正数，不溢出
}

///|
test "greet_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  assert_eq("Hello, 中文测试!", @azimuth.greet("中文测试"))
  assert_eq("Hello,   !", @azimuth.greet("   ")) // 空格
  assert_eq("Hello, \n\t!", @azimuth.greet("\n\t")) // 换行和制表符
  assert_eq("Hello, 🌙!", @azimuth.greet("🌙")) // emoji
}

///|
test "add_zero_combinations" {
  // 测试各种与0相关的加法组合
  assert_eq(0, @azimuth.add(0, 0))
  assert_eq(2147483647, @azimuth.add(2147483647, 0))
  assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
  assert_eq(123, @azimuth.add(0, 123))
  assert_eq(-456, @azimuth.add(0, -456))
  assert_eq(2147483647, @azimuth.add(0, 2147483647))
  assert_eq(-2147483648, @azimuth.add(0, -2147483648))
}

///|
test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  assert_eq(2, @azimuth.multiply(1, 2))
  assert_eq(4, @azimuth.multiply(2, 2))
  assert_eq(8, @azimuth.multiply(2, 4))
  assert_eq(16, @azimuth.multiply(4, 4))
  assert_eq(65536, @azimuth.multiply(256, 256))
  assert_eq(1048576, @azimuth.multiply(1024, 1024))
  assert_eq(2147483647, @azimuth.multiply(32768, 65536)) // 应该溢出
  assert_eq(2147483647, @azimuth.multiply(65536, 65536)) // 应该溢出
}

///|
test "add_consecutive_operations" {
  // 测试连续加法运算的一致性
  let a = 100
  let b = 200
  let c = 300
  // 测试 (a + b) + c == a + (b + c)
  assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  let x = -100
  let y = -200
  let z = -300
  // 测试负数情况
  assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))
  
  // 测试混合符号
  assert_eq(@azimuth.add(@azimuth.add(a, x), y), @azimuth.add(a, @azimuth.add(x, y)))
}

///|
test "multiply_distributive_property" {
  // 测试乘法分配律 (a * (b + c) == a * b + a * c)
  let a = 10
  let b = 20
  let c = 30
  assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
  
  // 测试负数情况
  let x = -10
  let y = 20
  let z = -30
  assert_eq(@azimuth.multiply(x, @azimuth.add(y, z)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z)))
}

///|
test "extreme_boundary_combinations" {
  // 测试极值边界组合
  let max = 2147483647
  let min = -2147483648
  
  // 最大值与接近最大值的运算
  assert_eq(max, @azimuth.add(max, 1))
  assert_eq(max, @azimuth.add(max - 1, 2))
  assert_eq(max, @azimuth.add(max - 100, 101))
  
  // 最小值与接近最小值的运算
  assert_eq(min, @azimuth.add(min, -1))
  assert_eq(min, @azimuth.add(min + 1, -2))
  assert_eq(min, @azimuth.add(min + 100, -101))
  
  // 最大值与最小值的乘法
  assert_eq(-2147483648, @azimuth.multiply(max, min))
  assert_eq(-2147483648, @azimuth.multiply(min, max))
}