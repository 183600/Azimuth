// MoonBit 测试用例示例
// 包含10个标准测试用例，覆盖不同的功能和场景

test "addition_with_large_numbers" {
  // 测试大数相加
  let large_num1 = 1000000000
  let large_num2 = 1500000000
  let result = azimuth::add(large_num1, large_num2)
  assert_eq(2147483647, result)  // 应该达到最大值
}

test "multiplication_with_negative_numbers" {
  // 测试负数乘法
  let result1 = azimuth::multiply(-5, 8)
  assert_eq(-40, result1)
  
  let result2 = azimuth::multiply(-7, -9)
  assert_eq(63, result2)
  
  let result3 = azimuth::multiply(0, -100)
  assert_eq(0, result3)
}

test "greet_with_various_names" {
  // 测试不同类型的名字
  assert_eq_string("Hello, Alice!", azimuth::greet("Alice"))
  assert_eq_string("Hello, Bob123!", azimuth::greet("Bob123"))
  assert_eq_string("Hello, 张三!", azimuth::greet("张三"))
  assert_eq_string("Hello, !", azimuth::greet(""))
}

test "boundary_value_addition" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加正数应该溢出
  assert_eq(max_val, azimuth::add(max_val, 1))
  // 最小值加负数应该溢出
  assert_eq(min_val, azimuth::add(min_val, -1))
  // 最大值加最小值
  assert_eq(-1, azimuth::add(max_val, min_val))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算圆的周长（近似值）
  // 周长 = 2 × π × r，使用 π ≈ 22/7
  let radius = 7
  let pi_numerator = 22
  let pi_denominator = 7
  
  // 计算 2 × r
  let diameter = azimuth::multiply(2, radius)
  // 计算 (2 × r) × 22
  let circumference_times_7 = azimuth::multiply(diameter, pi_numerator)
  // 除以 7 得到周长
  let circumference = circumference_times_7 / pi_denominator
  
  assert_eq(44, circumference)  // 2 × 7 × 22 / 7 = 44
}

test "sequential_operations" {
  // 测试连续操作
  let initial = 10
  
  // 连续加法
  let result1 = azimuth::add(azimuth::add(initial, 5), azimuth::add(3, 2))
  assert_eq(20, result1)  // 10 + 5 + 3 + 2 = 20
  
  // 连续乘法
  let result2 = azimuth::multiply(azimuth::multiply(initial, 2), azimuth::multiply(3, 1))
  assert_eq(60, result2)  // 10 × 2 × 3 × 1 = 60
  
  // 混合操作
  let result3 = azimuth::add(azimuth::multiply(initial, 2), azimuth::multiply(5, 3))
  assert_eq(35, result3)  // 10 × 2 + 5 × 3 = 35
}

test "string_with_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, John@Doe!", azimuth::greet("John@Doe"))
  assert_eq_string("Hello, Test#123!", azimuth::greet("Test#123"))
  assert_eq_string("Hello, User$Name!", azimuth::greet("User$Name"))
  assert_eq_string("Hello, Line1\nLine2!", azimuth::greet("Line1\nLine2"))
}

test "mathematical_properties" {
  // 测试数学性质
  
  // 加法交换律
  let a = 15
  let b = 25
  assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律
  let c = 8
  let d = 12
  assert_eq(azimuth::multiply(c, d), azimuth::multiply(d, c))
  
  // 分配律 (a × (b + c) = a × b + a × c)
  let x = 5
  let y = 3
  let z = 2
  let left_side = azimuth::multiply(x, azimuth::add(y, z))
  let right_side = azimuth::add(azimuth::multiply(x, y), azimuth::multiply(x, z))
  assert_eq(left_side, right_side)
}

test "real_world_calculation" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let shipping_fee = 20
  
  // 计算每种商品的总价
  let item1_total = azimuth::multiply(item1_price, item1_quantity)
  let item2_total = azimuth::multiply(item2_price, item2_quantity)
  
  // 计算商品总价
  let subtotal = azimuth::add(item1_total, item2_total)
  
  // 计算最终总价（包含运费）
  let total = azimuth::add(subtotal, shipping_fee)
  
  assert_eq(1294, total)  // 299×2 + 199×3 + 20 = 598 + 597 + 20 = 1215
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  
  // 测试断言函数
  azimuth::assert_true(true)
  azimuth::assert_false(false)
  azimuth::assert_eq(42, 42)
  azimuth::assert_eq_string("test", "test")
  
  // 测试极值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  azimuth::assert_true(azimuth::add(max_val, 1) == max_val)
  azimuth::assert_true(azimuth::add(min_val, -1) == min_val)
  azimuth::assert_true(azimuth::multiply(max_val, 2) == max_val)
  azimuth::assert_true(azimuth::multiply(min_val, -1) == min_val)
}