// 基础测试文件

///|
test "simple_add" {
  assert_eq(3, @azimuth.add(1, 2))
}

///|
test "add_functions" {
  // 基础功能测试
  assert_eq(3, @azimuth.add(1, 2))
  assert_eq(0, @azimuth.add(-1, 1))
  assert_eq(-5, @azimuth.add(-2, -3))
  assert_eq(0, @azimuth.add(0, 0))

  // 正数溢出测试
  assert_eq(2147483647, @azimuth.add(2147483646, 1)) // 接近Int最大值
  assert_eq(2147483647, @azimuth.add(2147483647, 0)) // 最大值加0
  assert_eq(2147483647, @azimuth.add(2147483647, 1)) // 溢出情况
  assert_eq(2147483647, @azimuth.add(1000000000, 2000000000)) // 溢出情况
  assert_eq(2147483647, @azimuth.add(2147483645, 3)) // 2147483645 + 3 = 2147483648，应该溢出

  // 负数溢出测试
  assert_eq(-2147483648, @azimuth.add(-2147483648, 0)) // 最小值加0
  assert_eq(-2147483648, @azimuth.add(-2147483648, -1)) // 溢出情况
  assert_eq(-2147483648, @azimuth.add(-2147483648, -1000000000)) // 溢出情况
  assert_eq(-2147483648, @azimuth.add(-2147483647, -1)) // 边界负数相加
  assert_eq(-2147483648, @azimuth.add(-2147483647, -2)) // 边界负数相加
  assert_eq(-2147483648, @azimuth.add(-2000000000, -2000000000)) // 大负数相加
  assert_eq(-2147483648, @azimuth.add(-1073741824, -1073741824)) // 应该溢出
  assert_eq(-2147483648, @azimuth.add(-1610612736, -536870912)) // 应该溢出
  assert_eq(-2147483648, @azimuth.add(-2000000000, -147483648)) // 应该溢出

  // 不溢出的负数相加
  assert_eq(-2147483647, @azimuth.add(-1073741824, -1073741823)) // 不应该溢出
  assert_eq(-2, @azimuth.add(-1, -1)) // 简单负数相加
  assert_eq(-2147483646, @azimuth.add(-2147483647, 1)) // 负数加正数
  assert_eq(-2147483647, @azimuth.add(-2147483646, -1)) // 不溢出
  assert_eq(-100, @azimuth.add(-50, -50)) // 普通负数相加

  // 正负数混合相加测试
  assert_eq(-1, @azimuth.add(2147483647, -2147483648)) // 最大值 + 最小值
  assert_eq(-1, @azimuth.add(-2147483648, 2147483647)) // 最小值 + 最大值
  assert_eq(-2147483647, @azimuth.add(1, -2147483648)) // 1 + 最小值
  assert_eq(-2147483647, @azimuth.add(-2147483648, 1)) // 最小值 + 1
  assert_eq(2147483646, @azimuth.add(2147483647, -1)) // 最大值 - 1
  assert_eq(100, @azimuth.add(200, -100)) // 普通正负数相加
  assert_eq(-100, @azimuth.add(-200, 100)) // 普通负正数相加
  assert_eq(0, @azimuth.add(2147483647, -2147483647)) // 最大值 + -最大值 = 0

  // 综合边界测试
  assert_eq(2147483647, @azimuth.add(1073741824, 1073741824))
  assert_eq(-2147483648, @azimuth.add(-2147483646, -3))
}

///|
test "multiply_functions" {
  // 基础功能测试
  assert_eq(6, @azimuth.multiply(2, 3))
  assert_eq(0, @azimuth.multiply(5, 0))
  assert_eq(-4, @azimuth.multiply(-2, 2))
  assert_eq(1, @azimuth.multiply(1, 1))
  assert_eq(0, @azimuth.multiply(0, 0))
  assert_eq(1, @azimuth.multiply(-1, -1))

  // 最小值相关测试
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1)) // 最小值乘1
  assert_eq(-2147483648, @azimuth.multiply(1, -2147483648)) // 1乘最小值
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1)) // 最小值乘-1（溢出）
  assert_eq(-2147483648, @azimuth.multiply(-1, -2147483648)) // -1乘最小值（溢出）
  assert_eq(0, @azimuth.multiply(-2147483648, 0)) // 最小值乘0
  assert_eq(0, @azimuth.multiply(0, -2147483648)) // 0乘最小值

  // 最小值乘以大于1的数
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, 2)) // 最小值乘2
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, -2)) // 最小值乘-2
  assert_eq(-2147483648, @azimuth.multiply(2, -2147483648)) // 正数乘以最小值
  assert_eq(-2147483648, @azimuth.multiply(-2, -2147483648)) // 负数乘以最小值

  // 边界值测试
  assert_eq(2147483647, @azimuth.multiply(2147483647, 1)) // 最大值乘1
  assert_eq(-2147483647, @azimuth.multiply(2147483647, -1)) // 最大值乘-1
  assert_eq(-2147483647, @azimuth.multiply(-2147483647, 1)) // 接近最小值乘1
  assert_eq(2147483647, @azimuth.multiply(-2147483647, -1)) // 接近最小值乘-1

  // 溢出测试
  assert_eq(2147483647, @azimuth.multiply(46341, 46341)) // 接近溢出的平方根
  assert_eq(2147483647, @azimuth.multiply(46342, 46342)) // 正溢出
  assert_eq(2147483647, @azimuth.multiply(50000, 50000)) // 正溢出
  assert_eq(2147483647, @azimuth.multiply(100000, 30000)) // 溢出情况
  assert_eq(2147483647, @azimuth.multiply(65536, 65536)) // 65536*65536=4294967296，应该溢出
  assert_eq(2147441940, @azimuth.multiply(46340, 46341)) // 接近溢出
  assert_eq(2147441940, @azimuth.multiply(46341, 46340)) // 接近溢出

  // 负溢出测试
  assert_eq(-2147483648, @azimuth.multiply(-100000, 30000)) // 负溢出情况
  assert_eq(-2147483648, @azimuth.multiply(-46341, 46341)) // 负溢出
  assert_eq(-2147483648, @azimuth.multiply(46341, -46341))
} // 负溢出

///|
test "greet_function" {
  assert_eq("Hello, World!", @azimuth.greet("World"))
  assert_eq("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  // 边界情况测试
  assert_eq("Hello, !", @azimuth.greet("")) // 空字符串
  assert_eq("Hello, A!", @azimuth.greet("A")) // 单字符
  assert_eq("Hello, 123!", @azimuth.greet("123"))
} // 数字字符串

///|
test "min_value_positive_add" {
  // 测试最小值加正数的情况（之前会被错误地处理为溢出）
  assert_eq(-2147483647, @azimuth.add(-2147483648, 1)) // 最小值 + 1
  assert_eq(-2147483646, @azimuth.add(-2147483648, 2)) // 最小值 + 2
  assert_eq(-2147483640, @azimuth.add(-2147483648, 8)) // 最小值 + 8
  assert_eq(-2147483648, @azimuth.add(-2147483648, 0)) // 最小值 + 0
  assert_eq(-1, @azimuth.add(-2147483648, 2147483647)) // 最小值 + 最大正数

  // 反过来测试
  assert_eq(-2147483647, @azimuth.add(1, -2147483648)) // 1 + 最小值
  assert_eq(-2147483646, @azimuth.add(2, -2147483648)) // 2 + 最小值
  assert_eq(-2147483640, @azimuth.add(8, -2147483648)) // 8 + 最小值
  assert_eq(-2147483648, @azimuth.add(0, -2147483648)) // 0 + 最小值

  // 测试最小值加负数（应该溢出）
  assert_eq(-2147483648, @azimuth.add(-2147483648, -1)) // 最小值 + -1
  assert_eq(-2147483648, @azimuth.add(-2147483648, -2)) // 最小值 + -2
  assert_eq(-2147483648, @azimuth.add(-1, -2147483648)) // -1 + 最小值
  assert_eq(-2147483648, @azimuth.add(-2, -2147483648))
} // -2 + 最小值

///|
test "multiply_edge_cases" {
  // 测试最小值乘以各种值
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1)) // 最小值 * 1
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1)) // 最小值 * -1 (溢出)
  assert_eq(0, @azimuth.multiply(-2147483648, 0)) // 最小值 * 0
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, 2)) // 最小值 * 2 (溢出)
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, -2)) // 最小值 * -2 (溢出)

  // 反过来测试
  assert_eq(-2147483648, @azimuth.multiply(1, -2147483648)) // 1 * 最小值
  assert_eq(-2147483648, @azimuth.multiply(-1, -2147483648)) // -1 * 最小值 (溢出)
  assert_eq(0, @azimuth.multiply(0, -2147483648)) // 0 * 最小值
  assert_eq(-2147483648, @azimuth.multiply(2, -2147483648)) // 2 * 最小值 (溢出)
  assert_eq(-2147483648, @azimuth.multiply(-2, -2147483648)) // -2 * 最小值 (溢出)

  // 测试大数相乘的溢出
  assert_eq(2147483647, @azimuth.multiply(46341, 46341)) // 接近溢出边界
  assert_eq(2147483647, @azimuth.multiply(46342, 46342)) // 溢出
  assert_eq(2147483647, @azimuth.multiply(65536, 65536)) // 大数溢出

  // 测试负溢出
  assert_eq(-2147483648, @azimuth.multiply(-46341, 46341)) // 负溢出
  assert_eq(-2147483648, @azimuth.multiply(46341, -46341))
} // 负溢出

///|
test "min_value_edge_cases" {
  // 测试第26行：a == -2147483648 且 b >= 0 的情况
  assert_eq(-2147483648, @azimuth.add(-2147483648, 0)) // 最小值 + 0
  assert_eq(-2147483647, @azimuth.add(-2147483648, 1)) // 最小值 + 1
  assert_eq(-2147483646, @azimuth.add(-2147483648, 2)) // 最小值 + 2
  assert_eq(-2147483548, @azimuth.add(-2147483648, 100)) // 最小值 + 100
  assert_eq(-2147482648, @azimuth.add(-2147483648, 1000)) // 最小值 + 1000
  assert_eq(-1, @azimuth.add(-2147483648, 2147483647)) // 最小值 + 最大值

  // 测试第36行：b == -2147483648 且 a >= 0 的情况
  assert_eq(-2147483648, @azimuth.add(0, -2147483648)) // 0 + 最小值
  assert_eq(-2147483647, @azimuth.add(1, -2147483648)) // 1 + 最小值
  assert_eq(-2147483646, @azimuth.add(2, -2147483648)) // 2 + 最小值
  assert_eq(-2147483548, @azimuth.add(100, -2147483648)) // 100 + 最小值
  assert_eq(-2147482648, @azimuth.add(1000, -2147483648)) // 1000 + 最小值
  assert_eq(-1, @azimuth.add(2147483647, -2147483648))
} // 最大值 + 最小值

///|
test "extra_test" {
  assert_eq(1, 1)
}

///|
test "add_boundary_test" {
  assert_eq(2147483647, @azimuth.add(2147483647, 1))
}

///|
test "multiply_boundary_test" {
  assert_eq(2147483647, @azimuth.multiply(46342, 46342))
}

///|
test "test_negative_overflow_edge_case" {
  // 测试 -1073741824 + (-1073741824) = -2147483648
  // 这是精确的边界情况，应该返回最小值
  assert_eq(-2147483648, @azimuth.add(-1073741824, -1073741824))

  // 测试 -1073741823 + (-1073741823) = -2147483646
  // 这不应该溢出
  assert_eq(-2147483646, @azimuth.add(-1073741823, -1073741823))
}

///|
test "debug_add" {
  // 测试负数相加的边界情况
  assert_eq(-2147483648, @azimuth.add(-1073741824, -1073741824)) // 应该溢出

  // 测试特殊情况
  assert_eq(-2147483647, @azimuth.add(-2147483648, 1)) // 最小值 + 1
  assert_eq(-2147483647, @azimuth.add(1, -2147483648)) // 1 + 最小值

  // 测试边界值
  assert_eq(2147483647, @azimuth.add(2147483646, 1)) // 接近最大值
  assert_eq(2147483647, @azimuth.add(2147483647, 1)) // 溢出

  // 测试0的情况
  assert_eq(0, @azimuth.add(0, 0))
  assert_eq(5, @azimuth.add(0, 5))
  assert_eq(-5, @azimuth.add(-5, 0))

  // 测试正负数混合
  assert_eq(-1, @azimuth.add(2147483647, -2147483648)) // 最大值 + 最小值
  assert_eq(0, @azimuth.add(2147483647, -2147483647)) // 最大值 + -最大值
  assert_eq(100, @azimuth.add(200, -100))
} // 普通正负数相加

///|
test "debug_multiply" {
  // 测试最小值的特殊情况
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1)) // 最小值乘1
  assert_eq(-2147483648, @azimuth.multiply(1, -2147483648)) // 1乘最小值
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1)) // 最小值乘-1（溢出）
  assert_eq(-2147483648, @azimuth.multiply(-1, -2147483648)) // -1乘最小值（溢出）

  // 测试边界溢出
  assert_eq(2147483647, @azimuth.multiply(46341, 46341)) // 应该溢出
  assert_eq(2147483647, @azimuth.multiply(46342, 46342)) // 应该溢出
  assert_eq(2147483647, @azimuth.multiply(65536, 65536)) // 65536*65536=4294967296，应该溢出

  // 测试负溢出
  assert_eq(-2147483648, @azimuth.multiply(-46341, 46341)) // 应该负溢出
  assert_eq(-2147483648, @azimuth.multiply(46341, -46341))
} // 应该负溢出

///|
test "specific_add_test" {
  assert_eq(-4, @azimuth.add(-2, -2))
}

///|
test "multiply_zero_test" {
  assert_eq(0, @azimuth.multiply(0, 0))
}

///|
test "add_edge_case" {
  assert_eq(-2147483648, @azimuth.add(-1073741824, -1073741824))
}

///|
test "add_min_value_positive" {
  assert_eq(-2147483647, @azimuth.add(-2147483648, 1)) // 最小值 + 1
  assert_eq(-2147483647, @azimuth.add(1, -2147483648))
} // 1 + 最小值

///|
test "final_add_test" {
  // 测试精确的边界情况
  assert_eq(-2147483648, @azimuth.add(-1073741824, -1073741824)) // -1073741824 * 2 = -2147483648
  assert_eq(-2147483647, @azimuth.add(-1073741824, -1073741823)) // -1073741824 * 2 + 1 = -2147483647
  assert_eq(-2147483648, @azimuth.add(-1610612736, -536870912)) // -1610612736 + -536870912 = -2147483648
  assert_eq(-2147483648, @azimuth.add(-2000000000, -147483648)) // -2000000000 + -147483648 = -2147483648

  // 测试不溢出的情况
  assert_eq(-2, @azimuth.add(-1, -1))
  assert_eq(-100, @azimuth.add(-50, -50))
  assert_eq(-2147483646, @azimuth.add(-2147483647, 1))
}

///|
test "final_multiply_test" {
  // 测试最小值的特殊情况
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
  assert_eq(-2147483648, @azimuth.multiply(1, -2147483648))
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1))
  assert_eq(-2147483648, @azimuth.multiply(-1, -2147483648))
  assert_eq(0, @azimuth.multiply(-2147483648, 0))
  assert_eq(0, @azimuth.multiply(0, -2147483648))

  // 测试边界溢出
  assert_eq(2147483647, @azimuth.multiply(46341, 46341))
  assert_eq(2147483647, @azimuth.multiply(46342, 46342))
  assert_eq(-2147483648, @azimuth.multiply(-46341, 46341))
  assert_eq(-2147483648, @azimuth.multiply(46341, -46341))
}
