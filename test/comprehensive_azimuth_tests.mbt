// 综合测试用例 - 针对 azimuth 库的标准测试
// 测试覆盖核心功能：add, multiply, greet, divide_with_ceil

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-3, -7))
}

test "multiply_function_zero_property" {
  // 测试乘法零元素性质
  assert_eq(0, multiply(0, 10))
  assert_eq(0, multiply(10, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(25, multiply(5, 5))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-2, divide_with_ceil(-4, 2))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let result1 = add(multiply(3, 4), multiply(2, 5))
  assert_eq(22, result1)  // 3*4 + 2*5 = 12 + 10 = 22
  
  let result2 = multiply(add(7, 3), 2)
  assert_eq(20, result2)  // (7 + 3) * 2 = 10 * 2 = 20
}

test "business_scenario_order_calculation" {
  // 业务场景：订单总价计算
  let unit_price = 45
  let quantity = 6
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(282, total_before_discount)
  assert_eq(262, final_total)
}

test "packaging_scenario_calculation" {
  // 包装场景：计算需要的包装箱数量
  let total_items = 35
  let items_per_box = 8
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)  // ceil(35/8) = 5
}

test "mathematical_distributive_property" {
  // 测试数学分配律：a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 8
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 4 * (6 + 8) = 4 * 6 + 4 * 8 = 56
}

test "edge_cases_large_numbers" {
  // 边界情况：大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(333334, divide_with_ceil(1000000, 3))
  assert_eq_string("Hello, This is a very long name for testing!", greet("This is a very long name for testing"))
}