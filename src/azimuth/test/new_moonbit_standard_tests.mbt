// 新增 MoonBit 测试用例
// 覆盖核心功能和实际应用场景

test "addition_with_large_numbers" {
  // 测试大数相加
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
  @azimuth.assert_eq(0, @azimuth.add(1500000000, -1500000000))
}

test "multiplication_with_negative_numbers" {
  // 测试负数乘法的各种情况
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-30, @azimuth.multiply(6, -5))
  @azimuth.assert_eq(-42, @azimuth.multiply(-7, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(-8, 0))
}

test "greet_function_with_special_characters" {
  // 测试包含特殊字符的问候语
  @azimuth.assert_eq_string("Hello, John_Doe!", @azimuth.greet("John_Doe"))
  @azimuth.assert_eq_string("Hello, user@123!", @azimuth.greet("user@123"))
  @azimuth.assert_eq_string("Hello, test#1!", @azimuth.greet("test#1"))
  @azimuth.assert_eq_string("Hello, $name!", @azimuth.greet("$name"))
}

test "complex_calculation_volume_of_box" {
  // 测试盒子体积计算
  let length = 5
  let width = 4
  let height = 3
  let expected_volume = 60
  
  // 体积 = 长 × 宽 × 高
  let base_area = @azimuth.multiply(length, width)
  let actual_volume = @azimuth.multiply(base_area, height)
  @azimuth.assert_eq(expected_volume, actual_volume)
}

test "mathematical_distributive_property" {
  // 测试数学分配律: a × (b + c) = a × b + a × c
  let a = 7
  let b = 3
  let c = 5
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "shopping_cart_total_calculation" {
  // 测试购物车总价计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let shipping_fee = 20
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 计算最终总价
  let total = @azimuth.add(subtotal, shipping_fee)
  @azimuth.assert_eq(817, total)
}

test "boundary_value_addition_overflow" {
  // 测试边界值加法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试极值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "sequential_operations_test" {
  // 测试连续运算
  let initial_value = 10
  
  // 连续加法: (((10 + 5) + 3) + 2) = 20
  let step1 = @azimuth.add(initial_value, 5)
  let step2 = @azimuth.add(step1, 3)
  let final_add = @azimuth.add(step2, 2)
  
  // 连续乘法: ((20 * 2) * 3) = 120
  let step3 = @azimuth.multiply(final_add, 2)
  let final_result = @azimuth.multiply(step3, 3)
  
  @azimuth.assert_eq(120, final_result)
}

test "string_processing_with_numbers" {
  // 测试包含数字的字符串处理
  @azimuth.assert_eq_string("Hello, 2023!", @azimuth.greet("2023"))
  @azimuth.assert_eq_string("Hello, version1.0!", @azimuth.greet("version1.0"))
  @azimuth.assert_eq_string("Hello, test123!", @azimuth.greet("test123"))
  @azimuth.assert_eq_string("Hello, 007!", @azimuth.greet("007"))
}

test "real_world_scenario_distance_calculation" {
  // 测试实际应用场景：距离计算
  let speed_kmh = 60  // 公里/小时
  let time_hours = 2
  let time_minutes = 30
  
  // 将时间转换为总分钟数
  let total_minutes = @azimuth.add(@azimuth.multiply(time_hours, 60), time_minutes)
  
  // 计算总时间（小时）= 150分钟 = 2.5小时，用整数表示为25/10
  let time_tenths = @azimuth.multiply(total_minutes, 10) / 60  // 2.5小时表示为25/10
  
  // 计算距离 = 速度 × 时间 = 60 × 2.5 = 150公里
  let distance = @azimuth.multiply(speed_kmh, time_tenths) / 10
  
  @azimuth.assert_eq(150, distance)
}