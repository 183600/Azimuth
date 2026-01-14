// 标准 MoonBit 测试套件
// 包含基本功能测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
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
    @azimuth.multiply(a, @azimuth.add(b, 5)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 5))
  )
}

test "boundary_value_tests" {
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

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算矩形面积和周长
  let length = 10
  let width = 5
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(30, perimeter)
}

test "real_world_application" {
  // 测试实际应用场景：简单购物车计算
  let item_price = 199
  let quantity = 3
  let shipping_fee = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 计算最终总价
  let total = @azimuth.add(subtotal, shipping_fee)
  
  @azimuth.assert_eq(597, subtotal)
  @azimuth.assert_eq(612, total)
}

test "sequential_operations" {
  // 测试连续操作
  // 计算 (a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(22, result)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  let special_chars = "test@example.com"
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet(special_chars))
  
  // 测试包含数字的字符串
  let numeric_string = "User123"
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet(numeric_string))
  
  // 测试包含空格的字符串
  let spaced_string = "John Doe"
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet(spaced_string))
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 计算等差数列：1 + 3 + 5 + 7 + 9
  let sum1 = @azimuth.add(1, 3)
  let sum2 = @azimuth.add(sum1, 5)
  let sum3 = @azimuth.add(sum2, 7)
  let sum4 = @azimuth.add(sum3, 9)
  
  @azimuth.assert_eq(25, sum4)
  
  // 计算等比数列：1 + 2 + 4 + 8 + 16
  let geo1 = @azimuth.add(1, 2)
  let geo2 = @azimuth.add(geo1, @azimuth.multiply(2, 2))
  let geo3 = @azimuth.add(geo2, @azimuth.multiply(@azimuth.multiply(2, 2), 2))
  let geo4 = @azimuth.add(geo3, @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), 2), 2))
  
  @azimuth.assert_eq(31, geo4)
}