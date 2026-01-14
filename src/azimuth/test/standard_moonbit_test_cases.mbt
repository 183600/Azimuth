// 标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_basic_functionality" {
  // 测试 add 函数的基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-1, -2))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_overflow_handling" {
  // 测试 add 函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试混合运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试 multiply 函数的基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

test "multiply_overflow_handling" {
  // 测试 multiply 函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数相乘溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  
  // 测试最小值的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "greet_basic_functionality" {
  // 测试 greet 函数的基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_edge_cases" {
  // 测试 greet 函数的边界情况
  // 测试长字符串
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
  
  // 测试特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  
  // 测试 Unicode 字符
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "assertion_functions" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "mathematical_properties" {
  // 测试数学运算的性质
  let a = 7
  let b = 11
  let c = 13
  
  // 测试加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "practical_application" {
  // 测试实际应用场景
  // 计算矩形面积和周长
  let length = 10
  let width = 5
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  @azimuth.assert_eq(50, area)
  @azimuth.assert_eq(30, perimeter)
}

test "error_handling" {
  // 测试错误处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
}