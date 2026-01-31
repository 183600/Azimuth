// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行简洁而全面的测试

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(7, 0))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(25, multiply(-5, -5))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "greet_function_comprehensive" {
  // 全面测试问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "mathematical_properties" {
  // 测试数学基本性质
  let a = 6
  let b = 9
  
  // 交换律
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 单位元
  assert_eq(a, add(a, 0))
  assert_eq(a, multiply(a, 1))
}

test "combined_operations" {
  // 测试组合运算
  let result1 = add(multiply(2, 4), multiply(3, 5))
  assert_eq(23, result1)  // 2*4 + 3*5 = 8 + 15 = 23
  
  let result2 = multiply(add(3, 7), 2)
  assert_eq(20, result2)  // (3 + 7) * 2 = 10 * 2 = 20
}

test "practical_business_calculation" {
  // 实际业务计算：订单总价
  let unit_price = 45
  let quantity = 6
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(270, subtotal)
  assert_eq(285, total)
}

test "packaging_calculation" {
  // 包装计算场景
  let total_items = 38
  let items_per_box = 10
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(38/10) = 4
}

test "edge_case_comprehensive" {
  // 边界情况综合测试
  assert_eq(0, multiply(0, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(1, divide_with_ceil(10, 10))
  assert_eq(-5, add(-10, 5))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_workflow" {
  // 复杂工作流程测试
  let base_price = 80
  let quantity = 4
  let discount = 30
  let tax_rate = 10
  
  let subtotal = multiply(base_price, quantity)
  let after_discount = add(subtotal, -discount)
  let tax = divide_with_ceil(multiply(after_discount, tax_rate), 100)
  let final_total = add(after_discount, tax)
  
  assert_eq(320, subtotal)
  assert_eq(290, after_discount)
  assert_eq(29, tax)
  assert_eq(319, final_total)
}