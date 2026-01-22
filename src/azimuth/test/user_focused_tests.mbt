// 用户专注测试用例 - 高质量 MoonBit 测试
// 覆盖 azimuth 库的核心功能和边界情况

test "add_function_core_functionality" {
  // 测试加法函数的核心功能
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-5, 2))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
}

test "multiply_function_core_functionality" {
  // 测试乘法函数的核心功能
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(-18, @azimuth.multiply(3, -6))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的各种情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
}

test "real_world_business_calculation" {
  // 实际业务计算场景
  let unit_price = 25
  let quantity = 8
  let discount = 20
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let final_total = @azimuth.add(subtotal, -discount)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(180, final_total)
}

test "packaging_and_distribution_calculation" {
  // 包装和分发计算
  let total_products = 47
  let products_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_products, products_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)
  
  // 计算总箱数（包括额外的备用箱）
  let total_boxes = @azimuth.add(boxes_needed, 1)
  @azimuth.assert_eq(5, total_boxes)
}

test "complex_nested_operations" {
  // 复杂嵌套操作测试
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 7
  
  // 计算 (base * multiplier) / divisor + addition
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let final_result = @azimuth.add(step2, addition)
  
  @azimuth.assert_eq(30, step1)
  @azimuth.assert_eq(8, step2)
  @azimuth.assert_eq(15, final_result)
}