// 标准MoonBit测试用例 - 用户自定义
// 测试 azimuth 模块的核心功能

test "add_function_basic_tests" {
  // 测试 add 函数的基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_basic_tests" {
  // 测试 multiply 函数的基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "greet_function_basic_tests" {
  // 测试 greet 函数的基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "arithmetic_property_tests" {
  // 测试算术运算的性质
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

test "boundary_value_tests" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 最小值测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "real_world_calculation_tests" {
  // 测试实际应用场景的计算
  // 计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(597, subtotal)
  @azimuth.assert_eq(617, total)
}

test "string_processing_tests" {
  // 测试字符串处理
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, A B!", @azimuth.greet("A B"))
}

test "complex_calculation_tests" {
  // 测试复杂计算
  // 计算 (a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(75, result)
}

test "mathematical_series_tests" {
  // 测试数学级数计算
  // 计算等差数列前5项和：2 + 4 + 6 + 8 + 10
  let sum = @azimuth.add(
    @azimuth.add(
      @azimuth.add(@azimuth.add(2, 4), 6),
      8
    ),
    10
  )
  
  @azimuth.assert_eq(30, sum)
  
  // 使用公式验证：n/2 * (首项 + 末项)
  let formula_sum = @azimuth.multiply(5, @azimuth.add(2, 10)) / 2
  @azimuth.assert_eq(30, formula_sum)
}

test "error_handling_tests" {
  // 测试错误处理
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("hello", "hello")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}