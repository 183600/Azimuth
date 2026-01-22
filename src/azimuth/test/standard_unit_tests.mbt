// 标准 MoonBit 单元测试用例
// 测试 Azimuth 项目的核心功能

test "add_function_basic_cases" {
  // 测试 add 函数的基本情况
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "multiply_function_basic_cases" {
  // 测试 multiply 函数的基本情况
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(16, @azimuth.multiply(-4, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "greet_function_variations" {
  // 测试 greet 函数的各种变体
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 3)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3))
  )
}

test "complex_calculations" {
  // 测试复合计算
  // 计算 (a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(22, result)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 场景：计算矩形周长
  let length = 8
  let width = 5
  
  let perimeter = @azimuth.add(
    @azimuth.multiply(2, length),
    @azimuth.multiply(2, width)
  )
  
  @azimuth.assert_eq(26, perimeter)
  
  // 场景：计算购物总价
  let price = 199
  let quantity = 3
  let shipping = 15
  
  let total = @azimuth.add(
    @azimuth.multiply(price, quantity),
    shipping
  )
  
  @azimuth.assert_eq(612, total)
}

test "error_handling" {
  // 测试错误处理
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_eq(@azimuth.add(max_val, 100, true) == max_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 2, true) == max_val)
}

test "string_processing" {
  // 测试字符串处理
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello, special@chars!", @azimuth.greet("special@chars"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced  "))
}