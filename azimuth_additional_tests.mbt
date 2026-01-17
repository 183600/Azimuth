// Azimuth 项目额外测试用例
// 使用标准 MoonBit 测试语法，专注于边界条件和特殊场景

test "add_large_numbers" {
  // 测试大数加法
  assert_eq(azimuth::add(1000000, 2000000), 3000000)
  assert_eq(azimuth::add(999999999, 1), 1000000000)
  assert_eq(azimuth::add(-1000000, 1000000), 0)
}

test "add_boundary_values" {
  // 测试边界值加法
  assert_eq(azimuth::add(2147483647, 0), 2147483647)  // 接近32位整数最大值
  assert_eq(azimuth::add(-2147483648, 0), -2147483648) // 32位整数最小值
  assert_eq(azimuth::add(2147483647, -2147483647), 0)  // 最大值与负最大值相加
}

test "multiply_large_numbers" {
  // 测试大数乘法
  assert_eq(azimuth::multiply(10000, 10000), 100000000)
  assert_eq(azimuth::multiply(12345, 6789), 83810205)
  assert_eq(azimuth::multiply(-5000, 2000), -10000000)
}

test "multiply_boundary_values" {
  // 测试边界值乘法
  assert_eq(azimuth::multiply(46340, 46340), 2147395600)  // 接近32位整数平方根
  assert_eq(azimuth::multiply(-32768, 32768), -1073741824) // 16位边界值测试
  assert_eq(azimuth::multiply(65536, 32768), 2147483648)  // 2^16 * 2^15
}

test "greet_empty_string" {
  // 测试空字符串问候
  assert_eq(azimuth::greet(""), "Hello, !")
}

test "greet_unicode_characters" {
  // 测试Unicode字符
  assert_eq(azimuth::greet("张三"), "Hello, 张三!")
  assert_eq(azimuth::greet("🚀"), "Hello, 🚀!")
  assert_eq(azimuth::greet("café"), "Hello, café!")
}

test "greet_long_names" {
  // 测试长名称
  let long_name = "VeryLongNameThatExceedsNormalLengthExpectations"
  assert_eq(azimuth::greet(long_name), "Hello, VeryLongNameThatExceedsNormalLengthExpectations!")
}

test "mathematical_properties" {
  // 测试数学性质
  // 交换律
  assert_eq(azimuth::add(17, 29), azimuth::add(29, 17))
  assert_eq(azimuth::multiply(12, 15), azimuth::multiply(15, 12))
  
  // 结合律
  assert_eq(azimuth::add(azimuth::add(5, 10), 15), azimuth::add(5, azimuth::add(10, 15)))
  assert_eq(azimuth::multiply(azimuth::multiply(2, 3), 4), azimuth::multiply(2, azimuth::multiply(3, 4)))
  
  // 分配律
  assert_eq(azimuth::multiply(5, azimuth::add(3, 4)), azimuth::add(azimuth::multiply(5, 3), azimuth::multiply(5, 4)))
}

test "zero_operations" {
  // 测试零操作
  // 加法零元
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::add(12345, 0), 12345)
  assert_eq(azimuth::add(0, -6789), -6789)
  
  // 乘法零元
  assert_eq(azimuth::multiply(0, 0), 0)
  assert_eq(azimuth::multiply(0, 12345), 0)
  assert_eq(azimuth::multiply(-6789, 0), 0)
}