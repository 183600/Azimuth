// Simple Quality Test Cases for Azimuth Library
// 简单质量测试用例 - 针对 Azimuth 库的基本测试

test "simple_addition_test" {
  // 简单加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "simple_multiplication_test" {
  // 简单乘法测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-4, multiply(-2, 2))
}

test "simple_greet_test" {
  // 简单问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "simple_divide_ceil_test" {
  // 简单向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "simple_combined_operations" {
  // 简单组合运算测试
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "simple_business_calculation" {
  // 简单业务计算测试
  let unit_price = 25
  let quantity = 4
  let shipping = 10
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(100, subtotal)
  assert_eq(110, total)
}

test "simple_packaging_calculation" {
  // 简单包装计算测试
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "simple_unicode_test" {
  // 简单 Unicode 测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "simple_negative_operations" {
  // 简单负数运算测试
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "simple_zero_operations" {
  // 简单零运算测试
  assert_eq(0, multiply(5, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}