// 标准 MoonBit 测试用例 - azimuth 库单元测试
// 测试核心功能：add, multiply, divide_with_ceil, greet

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(5, -5))
  assert_eq(100, add(50, 50))
}

test "multiply_function_properties" {
  // 测试乘法性质
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 10))
  assert_eq(25, multiply(5, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))  // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  assert_eq(0, divide_with_ceil(5, 0))   // 除零安全处理
  assert_eq(-3, divide_with_ceil(-10, 3)) // -3.333... -> -3
  assert_eq(4, divide_with_ceil(-10, -3)) // 3.333... -> 4
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let x = 10
  let y = 15
  let z = 3
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(75, result1)  // (10 + 15) * 3 = 25 * 3 = 75
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(153, result2)  // 10 * 15 + 3 = 150 + 3 = 153
}

test "real_world_business_calculation" {
  // 实际业务计算：订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_calculation_scenario" {
  // 包装计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
}

test "mathematical_distributive_property" {
  // 测试数学分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 3 * (4 + 5) = 3 * 4 + 3 * 5 = 27
}