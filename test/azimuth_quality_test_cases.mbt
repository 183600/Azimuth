// 高质量 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 使用标准 MoonBit 测试语法编写

test "mathematical_operations_precision" {
  // 测试数学运算的精度
  assert_eq(100, add(47, 53))
  assert_eq(144, multiply(12, 12))
  assert_eq(7, divide_with_ceil(20, 3))
  assert_eq(5, divide_with_ceil(25, 5))
}

test "negative_numbers_comprehensive" {
  // 全面测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "boundary_values_testing" {
  // 边界值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "complex_business_calculations" {
  // 复杂业务计算测试
  let product_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(348, final_total)
}

test "real_world_packaging_scenario" {
  // 实际包装场景测试
  let total_items = 47
  let items_per_box = 12
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
}

test "mathematical_properties_verification" {
  // 数学性质验证
  let a = 7
  let b = 13
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "large_scale_operations" {
  // 大规模运算测试
  let base_value = 10000
  let multiplier = 100
  
  let large_product = multiply(base_value, multiplier)
  let large_sum = add(large_product, 50000)
  
  assert_eq(1000000, large_product)
  assert_eq(1050000, large_sum)
}

test "fractional_division_accuracy" {
  // 分数除法精度测试
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(7, divide_with_ceil(13, 2))
  assert_eq(12, divide_with_ceil(100, 9))
}

test "combined_functional_operations" {
  // 组合功能操作测试
  let x = 10
  let y = 15
  let z = 20
  
  // 测试复合表达式
  let result1 = add(multiply(x, y), z)
  let result2 = multiply(add(x, y), z)
  
  assert_eq(170, result1)  // 10*15 + 20 = 150 + 20 = 170
  assert_eq(500, result2)  // (10 + 15) * 20 = 25 * 20 = 500
}