// 用户自定义的 azimuth 库测试用例
// 包含 8 个高质量的标准 MoonBit 测试用例

test "add_function_mathematical_properties" {
  // 测试加法函数的数学性质
  // 交换律：a + b = b + a
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  
  // 结合律：(a + b) + c = a + (b + c)
  let c = 23
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 单位元：a + 0 = a
  assert_eq(a, add(a, 0))
  assert_eq(b, add(0, b))
  
  // 相反数：a + (-a) = 0
  assert_eq(0, add(5, -5))
  assert_eq(0, add(-10, 10))
}

test "multiply_function_comprehensive_cases" {
  // 测试乘法函数的全面情况
  // 交换律：a * b = b * a
  let a = 7
  let b = 13
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素：a * 0 = 0
  assert_eq(0, multiply(a, 0))
  assert_eq(0, multiply(0, b))
  
  // 单位元：a * 1 = a
  assert_eq(a, multiply(a, 1))
  assert_eq(b, multiply(1, b))
  
  // 负数乘法
  assert_eq(-91, multiply(-7, 13))
  assert_eq(91, multiply(-7, -13))
  
  // 大数乘法
  assert_eq(1000000, multiply(1000, 1000))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  // 基本功能测试
  assert_eq(4, divide_with_ceil(10, 3))  // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // 3.333... -> 4
  assert_eq(-4, divide_with_ceil(10, -3))  // -3.333... -> -3
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))     // 0/5 = 0 -> 0
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为0，返回0
  assert_eq(0, divide_with_ceil(0, 0))     // 0/0，返回0
  
  // 极端情况
  assert_eq(1, divide_with_ceil(1, 1))     // 1/1 = 1 -> 1
  assert_eq(1000, divide_with_ceil(999999, 1000))  // 999.999 -> 1000
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  // 基本英文
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 中文支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 心流!", greet("心流"))
  
  // Emoji 支持
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 👋!", greet("👋"))
  
  // 特殊符号
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 123!", greet("123"))
  
  // 空字符串和空格
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  
  // 混合字符
  assert_eq_string("Hello, Hello世界🚀!", greet("Hello世界🚀"))
}