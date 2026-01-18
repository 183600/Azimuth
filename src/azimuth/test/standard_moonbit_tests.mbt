// 标准 MoonBit 测试用例
// 测试基本功能、边界条件和实际应用场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "identity_properties" {
  // 测试恒等性质
  let x = 42
  
  // 加法恒等
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
}

test "commutative_properties" {
  // 测试交换律
  let a = 15
  let b = 27
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "associative_properties" {
  // 测试结合律
  let a = 10
  let b = 20
  let c = 30
  
  // 加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
}

test "distributive_property" {
  // 测试分配律
  let a = 5
  let b = 7
  let c = 3
  
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
}

test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "string_operations" {
  // 测试字符串操作
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "negative_number_operations" {
  // 测试负数运算
  let a = -10
  let b = -20
  let c = 15
  
  // 负数相加
  @azimuth.assert_eq(-30, @azimuth.add(a, b))
  
  // 正负数相加
  @azimuth.assert_eq(5, @azimuth.add(a, c))
  
  // 负数相乘
  @azimuth.assert_eq(200, @azimuth.multiply(a, b))
  
  // 正负数相乘
  @azimuth.assert_eq(-150, @azimuth.multiply(a, c))
}

test "complex_calculation" {
  // 测试复杂计算
  let x = 5
  let y = 3
  let z = 2
  
  // 计算 (x + y) * z - x
  let result = @azimuth.add(@azimuth.multiply(@azimuth.add(x, y), z), -x)
  @azimuth.assert_eq(11, result)  // (5 + 3) * 2 - 5 = 16 - 5 = 11
  
  // 计算 x * y + x * z
  let result2 = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(25, result2)  // 5 * 3 + 5 * 2 = 15 + 10 = 25
}

test "real_world_scenario" {
  // 测试实际应用场景：计算订单总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let discount = 50
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)
  
  @azimuth.assert_eq(602, total)  // 199 * 3 - 50 + 15 = 597 - 50 + 15 = 562
}