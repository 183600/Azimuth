// 标准 MoonBit 测试用例 - 新增
// 测试 azimuth 库的核心功能

test "add_function_core_functionality" {
  // 测试加法函数的核心功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_core_functionality" {
  // 测试乘法函数的核心功能
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(0, 100))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(25, multiply(5, 5))
}

test "greet_function_core_functionality" {
  // 测试问候函数的核心功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_core_functionality" {
  // 测试向上取整除法函数的核心功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 8
  let b = 12
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  let a = 3
  let b = 4
  let c = 5
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-9, -3))
}

test "practical_business_calculation" {
  // 测试实际业务计算场景
  // 计算订单总价
  let product_price = 45
  let quantity = 6
  let shipping = 15
  let subtotal = multiply(product_price, quantity)
  let total = add(subtotal, shipping)
  assert_eq(285, total)
}

test "packaging_optimization" {
  // 测试包装优化场景
  let total_items = 58
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let unused_capacity = multiply(boxes_needed, items_per_box) - total_items
  assert_eq(5, boxes_needed)
  assert_eq(2, unused_capacity)
}

test "multilingual_greeting_support" {
  // 测试多语言问候支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, 123!", greet("123"))
}