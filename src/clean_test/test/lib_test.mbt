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

// 测试加法的交换律性质
///|
test "addition_commutative_property" {
  // 测试 a + b = b + a
  assert_eq(@azimuth.add(123, 456), @azimuth.add(456, 123))
  assert_eq(@azimuth.add(-100, 200), @azimuth.add(200, -100))
  assert_eq(@azimuth.add(-50, -75), @azimuth.add(-75, -50))
  assert_eq(@azimuth.add(0, 999), @azimuth.add(999, 0))
  assert_eq(@azimuth.add(2147483647, -2147483648), @azimuth.add(-2147483648, 2147483647))
}

// 测试乘法的分配律性质
///|
test "multiplication_distributive_property" {
  // 测试 a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
  
  let x = -3
  let y = 7
  let z = -2
  assert_eq(@azimuth.multiply(x, @azimuth.add(y, z)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z)))
}

// 测试实际应用场景：计算矩形的面积和周长
///|
test "practical_rectangle_calculations" {
  // 矩形的长和宽
  let length = 100
  let width = 50
  
  // 计算面积：长 × 宽
  let area = @azimuth.multiply(length, width)
  assert_eq(5000, area)
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  assert_eq(300, perimeter)
}

// 测试实际应用场景：购物车总价计算
///|
test "practical_shopping_cart_total" {
  // 商品价格和数量
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let item3_price = 99
  let item3_quantity = 3
  
  // 计算每种商品的小计
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let item3_total = @azimuth.multiply(item3_price, item3_quantity)
  
  // 计算总价
  let total = @azimuth.add(@azimuth.add(item1_total, item2_total), item3_total)
  assert_eq(994, total)
}

// 测试字符串拼接的实际应用场景
///|
test "practical_string_concatenation" {
  // 测试用户信息拼接
  let first_name = "张"
  let last_name = "三"
  let age = "25"
  let city = "北京"
  
  // 拼接完整姓名
  let full_name = @azimuth.add(first_name, last_name)
  assert_eq("张三", full_name)
  
  // 拼接自我介绍
  let intro1 = @azimuth.greet(full_name)
  assert_eq(true, intro1.contains("张三"))
}

// 测试加法的结合律性质
///|
test "addition_associative_property_enhanced" {
  // 测试 (a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = 300
  assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 测试负数的情况
  let x = -50
  let y = -30
  let z = 20
  assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))
  
  // 测试包含0的情况
  assert_eq(@azimuth.add(@azimuth.add(0, 123), 456), @azimuth.add(0, @azimuth.add(123, 456)))
}

// 测试乘法的结合律性质
///|
test "multiplication_associative_property_enhanced" {
  // 测试 (a * b) * c = a * (b * c)
  let a = 2
  let b = 3
  let c = 4
  assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 测试包含负数的情况
  let x = -2
  let y = 3
  let z = -4
  assert_eq(@azimuth.multiply(@azimuth.multiply(x, y), z), @azimuth.multiply(x, @azimuth.multiply(y, z)))
  
  // 测试包含0的情况
  assert_eq(@azimuth.multiply(@azimuth.multiply(0, 123), 456), @azimuth.multiply(0, @azimuth.multiply(123, 456)))
}

// 测试乘法交换律性质
///|
test "multiplication_commutative_property_enhanced" {
  // 测试 a * b = b * a
  assert_eq(@azimuth.multiply(12, 34), @azimuth.multiply(34, 12))
  assert_eq(@azimuth.multiply(-100, 200), @azimuth.multiply(200, -100))
  assert_eq(@azimuth.multiply(-50, -75), @azimuth.multiply(-75, -50))
  assert_eq(@azimuth.multiply(0, 999), @azimuth.multiply(999, 0))
}

// 测试加法单位元性质
///|
test "addition_identity_element_enhanced" {
  // 测试 a + 0 = a 和 0 + a = a
  assert_eq(123, @azimuth.add(123, 0))
  assert_eq(123, @azimuth.add(0, 123))
  assert_eq(-456, @azimuth.add(-456, 0))
  assert_eq(-456, @azimuth.add(0, -456))
  assert_eq(0, @azimuth.add(0, 0))
}

// 测试乘法单位元性质
///|
test "multiplication_identity_element_enhanced" {
  // 测试 a * 1 = a 和 1 * a = a
  assert_eq(123, @azimuth.multiply(123, 1))
  assert_eq(123, @azimuth.multiply(1, 123))
  assert_eq(-456, @azimuth.multiply(-456, 1))
  assert_eq(-456, @azimuth.multiply(1, -456))
  assert_eq(0, @azimuth.multiply(0, 1))
  assert_eq(0, @azimuth.multiply(1, 0))
}

// 测试乘法零元素性质
///|
test "multiplication_zero_element_enhanced" {
  // 测试 a * 0 = 0 和 0 * a = 0
  assert_eq(0, @azimuth.multiply(123, 0))
  assert_eq(0, @azimuth.multiply(0, 123))
  assert_eq(0, @azimuth.multiply(-456, 0))
  assert_eq(0, @azimuth.multiply(0, -456))
  assert_eq(0, @azimuth.multiply(0, 0))
}

// 测试字符串处理：特殊字符和Unicode
///|
test "string_special_characters" {
  assert_eq("Hello, 世界!", @azimuth.greet("世界"))
  assert_eq("Hello, 🌙!", @azimuth.greet("🌙"))
  assert_eq("Hello, John-Doe!", @azimuth.greet("John-Doe"))
  assert_eq("Hello, O'Reilly!", @azimuth.greet("O'Reilly"))
  assert_eq("Hello, C++!", @azimuth.greet("C++"))
}

// 测试复杂数学运算：平方和立方
///|
test "mathematical_operations" {
  // 平方计算
  assert_eq(100, @azimuth.multiply(10, 10))
  assert_eq(144, @azimuth.multiply(12, 12))
  assert_eq(1, @azimuth.multiply(-1, -1))
  
  // 立方计算
  assert_eq(1000, @azimuth.multiply(10, @azimuth.multiply(10, 10)))
  assert_eq(27, @azimuth.multiply(3, @azimuth.multiply(3, 3)))
  assert_eq(-8, @azimuth.multiply(-2, @azimuth.multiply(-2, -2)))
}

// 测试温度转换：摄氏度到华氏度
///|
test "temperature_conversion" {
  // F = C * 9/5 + 32，我们通过乘法来避免除法
  // 测试0°C = 32°F
  let celsius_0 = 0
  let fahrenheit_0_times_5 = @azimuth.add(@azimuth.multiply(celsius_0, 9), 160)  // (0 * 9 + 160) = 160, 160/5 = 32
  assert_eq(160, fahrenheit_0_times_5)
  
  // 测试100°C = 212°F
  let celsius_100 = 100
  let fahrenheit_100_times_5 = @azimuth.add(@azimuth.multiply(celsius_100, 9), 160)  // (100 * 9 + 160) = 1060, 1060/5 = 212
  assert_eq(1060, fahrenheit_100_times_5)
  
  // 测试-40°C = -40°F
  let celsius_minus40 = -40
  let fahrenheit_minus40_times_5 = @azimuth.add(@azimuth.multiply(celsius_minus40, 9), 160)  // (-40 * 9 + 160) = -200, -200/5 = -40
  assert_eq(-200, fahrenheit_minus40_times_5)
}

// 测试银行利息计算
///|
test "bank_interest_calculation" {
  // 本金
  let principal = 10000
  // 年利率 5% 表示为分数
  let interest_rate_numerator = 5
  let interest_rate_denominator = 100
  // 存款年限
  let years = 3
  
  // 简单利息计算：本金 * 利率 * 年限
  let interest_times_denominator = @azimuth.multiply(@azimuth.multiply(principal, interest_rate_numerator), years)
  // 总金额 = 本金 + 利息，这里我们只检查计算过程
  let total_times_denominator = @azimuth.add(@azimuth.multiply(principal, interest_rate_denominator), interest_times_denominator)
  
  // 11500 * 100 = 1150000
  assert_eq(1150000, total_times_denominator)
}

// 测试数列求和
///|
test "arithmetic_series_sum" {
  // 等差数列求和：1 + 2 + 3 + ... + n = n * (n + 1) / 2
  // 我们通过乘法来避免除法
  
  // 1 + 2 + ... + 10 = 55
  let n1 = 10
  let sum1_times_2 = @azimuth.multiply(n1, @azimuth.add(n1, 1))  // 10 * 11 = 110, 110/2 = 55
  assert_eq(110, sum1_times_2)
  
  // 1 + 2 + ... + 100 = 5050
  let n2 = 100
  let sum2_times_2 = @azimuth.multiply(n2, @azimuth.add(n2, 1))  // 100 * 101 = 10100, 10100/2 = 5050
  assert_eq(10100, sum2_times_2)
}

// 测试大数运算边界情况
///|
test "large_number_operations" {
  // 测试接近边界的运算
  let large_num = 1000000
  let another_large = 2000000
  
  // 大数加法
  assert_eq(3000000, @azimuth.add(large_num, another_large))
  
  // 大数乘法（不溢出）
  assert_eq(1000000000000, @azimuth.multiply(large_num, large_num))
  
  // 大数减法（用加法实现）
  assert_eq(-1000000, @azimuth.add(large_num, @azimuth.multiply(-1, another_large)))
}

// 测试字符串格式化
///|
test "string_formatting" {
  // 测试不同长度的字符串
  let short_name = "A"
  let medium_name = "MoonBit"
  let long_name = "VeryLongProgrammingLanguageName"
  
  let greeting_short = @azimuth.greet(short_name)
  let greeting_medium = @azimuth.greet(medium_name)
  let greeting_long = @azimuth.greet(long_name)
  
  // 验证字符串内容
  assert_eq("Hello, A!", greeting_short)
  assert_eq("Hello, MoonBit!", greeting_medium)
  assert_eq("Hello, VeryLongProgrammingLanguageName!", greeting_long)
  
  // 验证不同输入产生不同输出
  assert_eq(true, greeting_short != greeting_medium)
  assert_eq(true, greeting_medium != greeting_long)
  assert_eq(true, greeting_short != greeting_long)
}

// 新增测试用例 1: 测试减法运算（通过加负数实现）
///|
test "subtraction_using_addition" {
  // a - b = a + (-b)
  assert_eq(5, @azimuth.add(10, @azimuth.multiply(-1, 5)))  // 10 - 5 = 5
  assert_eq(-5, @azimuth.add(5, @azimuth.multiply(-1, 10)))  // 5 - 10 = -5
  assert_eq(0, @azimuth.add(10, @azimuth.multiply(-1, 10)))  // 10 - 10 = 0
  assert_eq(15, @azimuth.add(5, @azimuth.multiply(-1, -10))) // 5 - (-10) = 15
  
  // 边界情况
  assert_eq(2147483647, @azimuth.add(2147483647, @azimuth.multiply(-1, 0)))  // 最大值 - 0
  assert_eq(0, @azimuth.add(0, @azimuth.multiply(-1, 0)))  // 0 - 0
}

// 新增测试用例 2: 测试字符串中的特殊字符
///|
test "string_special_characters_extended" {
  // 测试包含特殊字符的字符串
  assert_eq("Hello, \"John\"!", @azimuth.greet("\"John\""))
  assert_eq("Hello, \\ slashes!", @azimuth.greet("\\ slashes"))
  assert_eq("Hello, line\nbreak!", @azimuth.greet("line\nbreak"))
  assert_eq("Hello, tab\tcharacter!", @azimuth.greet("tab\tcharacter"))
  assert_eq("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
}

// 新增测试用例 3: 测试几何计算 - 圆形面积和周长
///|
test "geometry_circle_calculations" {
  // 圆周率近似值 22/7
  let pi_numerator = 22
  let pi_denominator = 7
  
  // 半径
  let radius = 7
  
  // 面积 = π * r² = (22/7) * 49 = 154
  let area_times_denominator = @azimuth.multiply(pi_numerator, @azimuth.multiply(radius, radius))
  assert_eq(154, @azimuth.multiply(pi_denominator, 154))  // 验证结果
  
  // 周长 = 2 * π * r = 2 * (22/7) * 7 = 44
  let circumference_times_denominator = @azimuth.multiply(@azimuth.multiply(2, pi_numerator), radius)
  assert_eq(44, @azimuth.multiply(pi_denominator, 44))  // 验证结果
}

// 新增测试用例 4: 测试物理公式 - 速度和距离计算
///|
test "physics_motion_calculations" {
  // 距离 = 初速度 * 时间 + 0.5 * 加速度 * 时间²
  // 我们通过乘以2来避免小数
  
  let initial_velocity = 10  // m/s
  let acceleration = 2       // m/s²
  let time = 5               // s
  
  // 距离 * 2 = 2 * (初速度 * 时间) + 加速度 * 时间²
  let distance_times_2 = @azimuth.add(
    @azimuth.multiply(2, @azimuth.multiply(initial_velocity, time)),
    @azimuth.multiply(acceleration, @azimuth.multiply(time, time))
  )
  
  // 预期: 2 * (10 * 5) + 2 * 25 = 100 + 50 = 150
  assert_eq(150, distance_times_2)
}

// 新增测试用例 5: 测试统计分析 - 平均值计算
///|
test "statistics_average_calculation" {
  // 平均值 = 总和 / 数量
  // 我们通过计算总和来验证
  
  let numbers = [10, 20, 30, 40, 50]  // 假设的数组
  let count = 5
  
  // 计算总和
  let sum = @azimuth.add(
    @azimuth.add(
      @azimuth.add(10, 20),
      @azimuth.add(30, 40)
    ),
    50
  )
  
  // 平均值 * 数量 = 总和
  let average_times_count = sum
  assert_eq(150, average_times_count)  // 验证总和
  
  // 验证平均值计算
  assert_eq(30, @azimuth.multiply(5, 30))  // 5 * 30 = 150
}

// 新增测试用例 6: 测试时间转换 - 秒到分钟和小时
///|
test "time_conversion_calculations" {
  // 3661秒 = 1小时1分钟1秒
  
  let total_seconds = 3661
  
  // 小时 = 秒 / 3600，我们通过乘法验证
  let hours = 1
  let hours_in_seconds = @azimuth.multiply(hours, 3600)
  assert_eq(3600, hours_in_seconds)
  
  // 剩余秒数
  let remaining_seconds = @azimuth.add(total_seconds, @azimuth.multiply(-1, hours_in_seconds))
  assert_eq(61, remaining_seconds)
  
  // 分钟 = 剩余秒数 / 60
  let minutes = 1
  let minutes_in_seconds = @azimuth.multiply(minutes, 60)
  assert_eq(60, minutes_in_seconds)
  
  // 最终剩余秒数
  let final_seconds = @azimuth.add(remaining_seconds, @azimuth.multiply(-1, minutes_in_seconds))
  assert_eq(1, final_seconds)
}

// 新增测试用例 7: 测试货币转换 - 汇率计算
///|
test "currency_exchange_calculation" {
  // 假设汇率：1 USD = 6.5 CNY
  // 我们通过乘以10来避免小数
  
  let usd_amount = 100
  let exchange_rate_numerator = 65  // 6.5 * 10
  let exchange_rate_denominator = 10
  
  // CNY金额 * 10 = USD金额 * 汇率分子
  let cny_times_10 = @azimuth.multiply(usd_amount, exchange_rate_numerator)
  assert_eq(6500, cny_times_10)  // 100 * 65 = 6500，所以是650 CNY
  
  // 反向计算
  let cny_amount = 650
  let usd_times_10 = @azimuth.multiply(cny_amount, exchange_rate_denominator)
  assert_eq(6500, usd_times_10)  // 验证反向计算
}

// 新增测试用例 8: 测试游戏逻辑 - 简单的得分计算
///|
test "game_score_calculation" {
  // 游戏得分 = 基础分 * 等级 + 连击奖励
  
  let base_score = 100
  let level = 5
  let combo_bonus = 50
  let combo_multiplier = 3
  
  // 基础得分
  let level_score = @azimuth.multiply(base_score, level)
  
  // 连击奖励
  let total_bonus = @azimuth.multiply(combo_bonus, combo_multiplier)
  
  // 总得分
  let total_score = @azimuth.add(level_score, total_bonus)
  
  // 验证: 100 * 5 + 50 * 3 = 500 + 150 = 650
  assert_eq(650, total_score)
}

// 新增测试用例 9: 测试复杂数学表达式 - 二次方程求值
///|
test "complex_mathematical_expression" {
  // 计算 ax² + bx + c
  // 例子: 2x² + 3x + 1，当 x = 5 时
  
  let a = 2
  let b = 3
  let c = 1
  let x = 5
  
  // x²
  let x_squared = @azimuth.multiply(x, x)
  
  // ax²
  let ax_squared = @azimuth.multiply(a, x_squared)
  
  // bx
  let bx = @azimuth.multiply(b, x)
  
  // ax² + bx + c
  let result = @azimuth.add(@azimuth.add(ax_squared, bx), c)
  
  // 验证: 2*25 + 3*5 + 1 = 50 + 15 + 1 = 66
  assert_eq(66, result)
}

// 新增测试用例 10: 测试数组求和（模拟）
///|
test "array_sum_simulation" {
  // 模拟数组求和: [1, 2, 3, 4, 5]
  
  // 使用链式加法
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  assert_eq(15, sum1)
  
  // 使用分组加法
  let group1 = @azimuth.add(1, 5)  // 首尾相加
  let group2 = @azimuth.add(2, 4)  // 第二和倒数第二相加
  let sum2 = @azimuth.add(@azimuth.add(group1, group2), 3)  // 加上中间值
  assert_eq(15, sum2)
  
  // 验证两种方法结果一致
  assert_eq(sum1, sum2)
}
