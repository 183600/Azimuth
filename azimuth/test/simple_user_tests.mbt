// 用户请求的标准 MoonBit 测试用例（简化版）
// 这些测试用例专注于验证核心功能

test "basic_addition_test" {
  // 测试基本加法
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

test "basic_multiplication_test" {
  // 测试基本乘法
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
}

test "string_greeting_test" {
  // 测试字符串问候
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "assert_functions_test" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 3
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_values_test" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "real_world_scenario" {
  // 测试实际应用场景：计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 计算最终总价（含运费）
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(612, total)  // 199*3 + 15 = 597 + 15 = 612
}

test "complex_calculation" {
  // 测试复杂计算：计算矩形的周长
  let length = 10
  let width = 5
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  
  @azimuth.assert_eq(30, perimeter)
}