// 简化的 Azimuth 测试用例
// 使用标准 MoonBit 测试语法

test "simple_addition_test" {
  // 简单加法测试
  assert_eq(10, add(3, 7))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "simple_multiplication_test" {
  // 简单乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(100, multiply(10, 10))
  assert_eq(0, multiply(0, 5))
}

test "simple_divide_with_ceil_test" {
  // 简单向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "simple_greet_test" {
  // 简单问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "simple_combined_operations" {
  // 简单组合操作测试
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
}

test "simple_business_scenario" {
  // 简单业务场景测试
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(200, subtotal)
  assert_eq(215, total)
}

test "simple_packaging_test" {
  // 简单包装测试
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "simple_unicode_test" {
  // 简单Unicode测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "simple_mathematical_properties" {
  // 简单数学性质测试
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "simple_practical_calculation" {
  // 简单实用计算测试
  let base_price = 100
  let tax_rate = 8
  let quantity = 5
  
  let subtotal = multiply(base_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax_amount)
  
  assert_eq(500, subtotal)
  assert_eq(40, tax_amount)
  assert_eq(540, total)
}