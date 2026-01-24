// 新的标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含基本功能、边界情况和实际应用场景的测试

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(20, multiply(4, 5))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "mathematical_properties" {
  // 数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律 a * (b + c) = a * b + a * c
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "boundary_values_test" {
  // 边界值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq_string("Hello, !", greet(""))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "real_world_shopping_cart" {
  // 实际购物车计算场景
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 15
  let item2_quantity = 2
  
  let subtotal1 = multiply(item1_price, item1_quantity)
  let subtotal2 = multiply(item2_price, item2_quantity)
  let total = add(subtotal1, subtotal2)
  
  assert_eq(105, total)  // 25*3 + 15*2 = 75 + 30 = 105
}

test "packaging_optimization" {
  // 包装优化计算
  let total_products = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(total_products, capacity_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
}

test "string_greeting_variations" {
  // 字符串问候变体测试
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "complex_business_calculation" {
  // 复杂业务计算
  let base_price = 100
  let quantity = 5
  let tax_rate = 8
  let discount = 20
  
  let subtotal = multiply(base_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(subtotal, tax)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(520, final_total)  // 100*5 + ceil(500*8/100) - 20 = 500 + 40 - 20 = 520
}

test "large_number_handling" {
  // 大数处理测试
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  assert_eq(33334, divide_with_ceil(1000000, 30))
}

test "edge_case_divisions" {
  // 除法边界情况
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(101, 100))
  assert_eq(100, divide_with_ceil(100, 1))
  assert_eq(-1, divide_with_ceil(-1, 100))
  assert_eq(0, divide_with_ceil(0, 100))
}