// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
}

test "add_boundary_values" {
  // 测试加法的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))  // 溢出保护
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))  // 溢出保护
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val))
}

test "multiply_boundary_values" {
  // 测试乘法的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))  // 特殊情况处理
}

test "greet_function_basic" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "greet_function_special_characters" {
  // 测试包含特殊字符的问候功能
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 10
  let b = 20
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 5)), 
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 5))
  )
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算表达式：(a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)  // 8
  let product = @azimuth.multiply(sum_ab, c)  // 32
  let result = @azimuth.add(product, -d)  // 22
  
  @azimuth.assert_eq(22, result)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算矩形面积和周长
  let length = 8
  let width = 5
  
  let area = @azimuth.multiply(length, width)  // 40
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))  // 26
  
  @azimuth.assert_eq(40, area)
  @azimuth.assert_eq(26, perimeter)
}

test "error_handling" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -100) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  
  // 测试正常情况
  @azimuth.assert_true(@azimuth.add(5, 3) == 8)
  @azimuth.assert_true(@azimuth.multiply(4, 6) == 24)
  @azimuth.assert_true(@azimuth.greet("Test") == "Hello, Test!")
}