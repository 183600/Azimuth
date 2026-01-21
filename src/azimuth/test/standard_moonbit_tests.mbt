// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 15
  let b = 23
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 最小值边界测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形面积和周长
  let length = 8
  let width = 6
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(28, perimeter)
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(5, @azimuth.add(-8, 13))
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
}

test "identity_element_properties" {
  // 测试单位元性质
  let number = 42
  
  // 加法单位元：0
  @azimuth.assert_eq(number, @azimuth.add(number, 0))
  @azimuth.assert_eq(number, @azimuth.add(0, number))
  
  // 乘法单位元：1
  @azimuth.assert_eq(number, @azimuth.multiply(number, 1))
  @azimuth.assert_eq(number, @azimuth.multiply(1, number))
  
  // 乘法零元：0
  @azimuth.assert_eq(0, @azimuth.multiply(number, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, number))
}

test "string_processing_with_unicode" {
  // 测试Unicode字符串处理
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, Café!", @azimuth.greet("Café"))
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  let a = 5
  let b = 3
  let c = 2
  
  // a × (b + c) = a × b + a × c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "real_world_application_tax_calculation" {
  // 测试实际应用：税费计算
  let price = 100
  let tax_rate = 8  // 8% 税率
  
  // 计算税费
  let tax_amount = @azimuth.multiply(price, tax_rate) / 100
  @azimuth.assert_eq(8, tax_amount)
  
  // 计算总价
  let total_price = @azimuth.add(price, tax_amount)
  @azimuth.assert_eq(108, total_price)
}