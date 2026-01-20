// 新增的标准 MoonBit 测试用例
// 测试 azimuth 包的各种核心功能和边界情况

test "add_comprehensive_edge_cases" {
  // 全面测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值加法
  @azimuth.assert_eq(100, @azimuth.add(100, 0))
  @azimuth.assert_eq(-100, @azimuth.add(-100, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 测试大数加法
  @azimuth.assert_eq(max_val, @azimuth.add(2147483640, 7))
  @azimuth.assert_eq(max_val, @azimuth.add(1073741824, 1073741823))
  
  // 测试负数加法
  @azimuth.assert_eq(min_val, @azimuth.add(-2147483640, -8))
  @azimuth.assert_eq(-100, @azimuth.add(-150, 50))
}

test "multiply_comprehensive_scenarios" {
  // 全面测试乘法的各种场景
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(46340, 46341))
  @azimuth.assert_eq(max_val, @azimuth.multiply(65536, 32768))
  
  // 测试负数乘法的各种组合
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  @azimuth.assert_eq(-100, @azimuth.multiply(-10, 10))
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
  
  // 测试最小值的特殊处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "greet_function_comprehensive" {
  // 全面测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 王小明!", @azimuth.greet("王小明"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, empty!", @azimuth.greet("empty"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, space bar!", @azimuth.greet("space bar"))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  // 测试能整除的情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(15, 3))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-15, 3))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(15, -3))
  
  // 测试有余数的情况
  @azimuth.assert_eq(6, @azimuth.divide_with_ceil(16, 3))
  @azimuth.assert_eq(6, @azimuth.divide_with_ceil(17, 3))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-14, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-13, 3))
  
  // 测试1和-1的情况
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(100, 1))
  @azimuth.assert_eq(-100, @azimuth.divide_with_ceil(-100, 1))
  @azimuth.assert_eq(-100, @azimuth.divide_with_ceil(100, -1))
}

test "complex_mathematical_expressions" {
  // 测试复杂的数学表达式
  let a = 15
  let b = 4
  let c = 3
  
  // 测试复合运算
  let result1 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(63, result1)
  
  let result2 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(57, result2)
  
  let result3 = @azimuth.divide_with_ceil(@azimuth.add(a, b), c)
  @azimuth.assert_eq(7, result3)
  
  // 测试嵌套运算
  let result4 = @azimuth.multiply(@azimuth.add(@azimuth.multiply(2, 3), 4), 5)
  @azimuth.assert_eq(50, result4)
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let x = 10
  let y = 20
  let z = 30
  
  // 加法交换律（在安全范围内）
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 加法结合律（在安全范围内）
  let left_assoc = @azimuth.add(@azimuth.add(x, y), z)
  let right_assoc = @azimuth.add(x, @azimuth.add(y, z))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 乘法交换律（在安全范围内）
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律（在安全范围内）
  let distributive1 = @azimuth.multiply(x, @azimuth.add(y, z))
  let distributive2 = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(distributive1, distributive2)
}

test "boundary_and_overflow_comprehensive" {
  // 综合测试边界值和溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近最大值的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 100, 101))
  @azimuth.assert_eq(max_val, @azimuth.add(1073741823, 1073741824))
  
  // 测试接近最小值的加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 100, -101))
  @azimuth.assert_eq(min_val, @azimuth.add(-1073741824, -1073741824))
  
  // 测试最大值乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(46340, 46341))
  @azimuth.assert_eq(max_val, @azimuth.multiply(32768, 65536))
  
  // 测试最小值乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_and_unicode_handling" {
  // 测试字符串和Unicode处理
  // 基本字符串测试
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, ABC!", @azimuth.greet("ABC"))
  @azimuth.assert_eq_string("Hello, abc!", @azimuth.greet("abc"))
  
  // Unicode字符测试
  @azimuth.assert_eq_string("Hello, 中文字符!", @azimuth.greet("中文字符"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, mix混合123!", @azimuth.greet("mix混合123"))
  
  // 特殊字符和符号
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced  "))
}