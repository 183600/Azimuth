// 标准 MoonBit 测试用例 - 针对 azimuth 库核心功能
// 包含 10 个高质量的测试用例，覆盖主要功能

test "add_function_standard" {
  // 测试加法函数的标准功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_standard" {
  // 测试乘法函数的标准功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(25, multiply(5, 5))
}

test "greet_function_standard" {
  // 测试问候函数的标准功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_standard" {
  // 测试向上取整除法的标准功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties" {
  // 测试数学性质
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

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = add(divide_with_ceil(10, 3), multiply(2, 2))
  assert_eq(8, result2)  // ceil(10/3) + 2*2 = 4 + 4 = 8
}

test "practical_packaging_calculation" {
  // 实际应用场景：包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "greeting_special_characters" {
  // 测试特殊字符的问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "complex_business_scenario" {
  // 复杂业务场景：计算订单总价
  let product_price = 25
  let quantity = 8
  let shipping_cost = 15
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(215, final_total)  // 25*8 + 15 - 20 = 200 + 15 - 20 = 195
}