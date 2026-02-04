// 用户自定义的 azimuth 库测试用例
// 包含 8 个高质量的标准 MoonBit 测试用例

test "add_function_mathematical_properties" {
  // 测试加法函数的数学性质
  // 交换律：a + b = b + a
  let a = 42
  let b = 17
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 结合律：(a + b) + c = a + (b + c)
  let c = 23
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 单位元：a + 0 = a
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(b, @azimuth.add(0, b))
  
  // 相反数：a + (-a) = 0
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(0, @azimuth.add(-10, 10))
}

test "multiply_function_comprehensive_cases" {
  // 测试乘法函数的全面情况
  // 交换律：a * b = b * a
  let a = 7
  let b = 13
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 零元素：a * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, b))
  
  // 单位元：a * 1 = a
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(b, @azimuth.multiply(1, b))
  
  // 负数乘法
  @azimuth.assert_eq(-91, @azimuth.multiply(-7, 13))
  @azimuth.assert_eq(91, @azimuth.multiply(-7, -13))
  
  // 大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  // 基本功能测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))  // 3.333... -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))   // 3 -> 3
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 0.01 -> 1
  
  // 负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -3.333... -> -3
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))  // 3.333... -> 4
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))  // -3.333... -> -3
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))     // 0/5 = 0 -> 0
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))     // 除数为0，返回0
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))     // 0/0，返回0
  
  // 极端情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))     // 1/1 = 1 -> 1
  @azimuth.assert_eq(1000, @azimuth.divide_with_ceil(999999, 1000))  // 999.999 -> 1000
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  // 基本英文
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 中文支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 心流!", @azimuth.greet("心流"))
  
  // Emoji 支持
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, 👋!", @azimuth.greet("👋"))
  
  // 特殊符号
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  
  // 空字符串和空格
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  
  // 混合字符
  @azimuth.assert_eq_string("Hello, Hello世界🚀!", @azimuth.greet("Hello世界🚀"))
}