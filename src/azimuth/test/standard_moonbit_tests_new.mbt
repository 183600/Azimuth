// 标准 MoonBit 测试用例
// 覆盖 azimuth 项目的核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  let result1 = @azimuth.add(10, 20)
  @azimuth.assert_eq(30, result1)
  
  let result2 = @azimuth.add(-5, 15)
  @azimuth.assert_eq(10, result2)
  
  let result3 = @azimuth.add(0, 100)
  @azimuth.assert_eq(100, result3)
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  let result1 = @azimuth.multiply(6, 7)
  @azimuth.assert_eq(42, result1)
  
  let result2 = @azimuth.multiply(-4, 5)
  @azimuth.assert_eq(-20, result2)
  
  let result3 = @azimuth.multiply(8, 0)
  @azimuth.assert_eq(0, result3)
}

test "greet_functionality" {
  // 测试问候功能
  let result1 = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result1)
  
  let result2 = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result2)
}

test "overflow_protection_addition" {
  // 测试加法溢出保护
  let max_val = 2147483647
  
  // 测试正数溢出
  let result1 = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, result1)
  
  // 测试负数溢出
  let min_val = -2147483648
  let result2 = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, result2)
}

test "overflow_protection_multiplication" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  let result1 = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, result1)
  
  // 测试最小值特殊处理
  let result2 = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, result2)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(b, a)
  @azimuth.assert_eq(sum1, sum2)
  
  // 乘法交换律
  let product1 = @azimuth.multiply(a, b)
  let product2 = @azimuth.multiply(b, a)
  @azimuth.assert_eq(product1, product2)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形面积和周长
  let length = 15
  let width = 8
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(120, area)
  
  // 计算周长：2 * (length + width)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(46, perimeter)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  let special_name = "测试用户@123"
  let result = @azimuth.greet(special_name)
  @azimuth.assert_eq_string("Hello, 测试用户@123!", result)
  
  let unicode_name = "🌙"
  let result2 = @azimuth.greet(unicode_name)
  @azimuth.assert_eq_string("Hello, 🌙!", result2)
}

test "real_world_application" {
  // 测试实际应用场景：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping_fee = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 计算最终总价
  let total = @azimuth.add(subtotal, shipping_fee)
  
  // 验证结果：199*3 + 15 = 597 + 15 = 612
  @azimuth.assert_eq(612, total)
}

test "assertion_functions" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
}