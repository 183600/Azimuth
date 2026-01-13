// 额外的 MoonBit 标准测试用例
// 测试 azimuth 包的核心功能

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(-10, azimuth::add(-5, -5))
  assert_eq(0, azimuth::add(-5, 5))
  assert_eq(-15, azimuth::add(-20, 5))
  assert_eq(5, azimuth::add(-10, 15))
}

test "multiply_negative_combinations" {
  // 测试负数乘法组合
  assert_eq(25, azimuth::multiply(-5, -5))
  assert_eq(-25, azimuth::multiply(-5, 5))
  assert_eq(-25, azimuth::multiply(5, -5))
  assert_eq(0, azimuth::multiply(-5, 0))
}

test "greet_multilingual" {
  // 测试多语言问候
  assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  assert_eq_string("Hello, Москва!", azimuth::greet("Москва"))
  assert_eq_string("Hello, Paris!", azimuth::greet("Paris"))
  assert_eq_string("Hello, São Paulo!", azimuth::greet("São Paulo"))
}

test "boundary_value_addition" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth::add(max_val, 0))
  assert_eq(min_val, azimuth::add(min_val, 0))
  assert_eq(max_val, azimuth::add(max_val, -min_val))
  assert_eq(0, azimuth::add(max_val, min_val))
}

test "mathematical_sequences" {
  // 测试数学序列计算
  // 等比数列：2 + 4 + 8 + 16 = 30
  let geo_sum = azimuth::add(azimuth::add(2, 4), azimuth::add(8, 16))
  assert_eq(30, geo_sum)
  
  // 平方数序列：1² + 2² + 3² + 4² = 30
  let square_sum = azimuth::add(azimuth::multiply(1, 1), azimuth::add(azimuth::multiply(2, 2), azimuth::add(azimuth::multiply(3, 3), azimuth::multiply(4, 4))))
  assert_eq(30, square_sum)
}

test "string_special_characters" {
  // 测试特殊字符字符串
  assert_eq_string("Hello, @#$%^&*()!", azimuth::greet("@#$%^&*()"))
  assert_eq_string("Hello, \"quoted\"!", azimuth::greet("\"quoted\""))
  assert_eq_string("Hello, 'single'!", azimuth::greet("'single'"))
  assert_eq_string("Hello, [brackets]!", azimuth::greet("[brackets]"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算
  // 订单计算：商品价格 × 数量 + 运费 - 折扣
  let product_price = 199
  let quantity = 3
  let shipping = 20
  let discount = 50
  
  let subtotal = azimuth::multiply(product_price, quantity)
  let total = azimuth::add(azimuth::add(subtotal, shipping), -discount)
  
  assert_eq(587, total)  // 199×3 + 20 - 50 = 597 + 20 - 50 = 567
}

test "assert_function_variations" {
  // 测试断言函数的各种用法
  let test_value = 42
  let test_string = "MoonBit"
  let test_condition = true
  let test_false_condition = false
  
  assert_eq(test_value, azimuth::add(20, 22))
  assert_eq_string(test_string, "MoonBit")
  assert_true(test_condition)
  assert_false(test_false_condition)
}

test "edge_case_calculations" {
  // 测试边缘情况计算
  // 接近边界的值
  let near_max = 2147483640
  let near_min = -2147483640
  
  assert_eq(2147483647, azimuth::add(near_max, 7))
  assert_eq(-2147483648, azimuth::add(near_min, -8))
  
  // 大数相乘（接近边界但不溢出）
  assert_eq(1000000, azimuth::multiply(1000, 1000))
  assert_eq(-1000000, azimuth::multiply(-1000, 1000))
}

test "real_world_measurements" {
  // 测试实际测量计算
  // 长度单位转换：米转厘米（1米 = 100厘米）
  let meters = 5
  let centimeters = azimuth::multiply(meters, 100)
  assert_eq(500, centimeters)
  
  // 面积计算：矩形面积 = 长 × 宽
  let length = 12
  let width = 8
  let area = azimuth::multiply(length, width)
  assert_eq(96, area)
  
  // 周长计算：矩形周长 = 2 × (长 + 宽)
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  assert_eq(40, perimeter)
}