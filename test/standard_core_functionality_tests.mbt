// 标准 MoonBit 测试用例 - 核心功能测试
// 包含 10 个测试用例，覆盖 azimuth 库的主要功能

test "addition_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(7, 8))
  assert_eq(15, add(8, 7))
  assert_eq(0, add(5, -5))
  assert_eq(100, add(50, 50))
}

test "multiplication_properties" {
  // 测试乘法性质
  assert_eq(0, multiply(5, 0))
  assert_eq(12, multiply(3, 4))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_variations" {
  // 测试问候函数的各种情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "large_number_calculations" {
  // 测试大数计算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(100001, add(100000, 1))
}

test "business_calculation_scenario" {
  // 业务计算场景：订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  assert_eq(200, subtotal)
  assert_eq(215, total)
}

test "packaging_calculation" {
  // 包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "mathematical_identity_properties" {
  // 测试数学单位元性质
  assert_eq(7, add(7, 0))  // 加法单位元
  assert_eq(7, multiply(7, 1))  // 乘法单位元
  assert_eq(0, multiply(7, 0))  // 乘法零元
  assert_eq(7, divide_with_ceil(7, 1))  // 除法单位元
}