// 标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的高质量测试

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(8, add(3, 5))
  assert_eq(12, multiply(3, 4))
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
}

test "greeting_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
}

test "ceiling_division_positive" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "ceiling_division_negative" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-2, divide_with_ceil(-4, 2))
}

test "mathematical_commutative_laws" {
  // 测试交换律
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "combined_operations" {
  // 测试组合运算
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "practical_packaging_scenario" {
  // 实际包装场景测试
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "business_calculation" {
  // 业务计算测试
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  assert_eq(215, total)
}

test "unicode_string_support" {
  // Unicode 字符串支持测试
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, María!", greet("María"))
}

test "edge_case_handling" {
  // 边界情况处理测试
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq(100, divide_with_ceil(100, 1))
  assert_eq(0, multiply(0, -1000))
  assert_eq(-1000, add(0, -1000))
}