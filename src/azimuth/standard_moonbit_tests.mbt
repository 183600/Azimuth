// 标准 MoonBit 测试用例 - 针对 azimuth 库
// 使用标准的 MoonBit 测试语法，简洁而全面

test "add_function_basic_operations" {
  // 测试加法的基本运算
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_properties" {
  // 测试乘法的性质
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(0, 100))
  assert_eq(-24, multiply(-8, 3))
  assert_eq(25, multiply(5, 5))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))  // 10/3 = 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(0, divide_with_ceil(5, 0))   // 除数为0，返回0
  assert_eq(-3, divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "mathematical_commutative_laws" {
  // 验证加法和乘法交换律
  let a = 12
  let b = 8
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let product_price = 25
  let quantity = 4
  let shipping = 10
  
  let subtotal = multiply(product_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(100, subtotal)
  assert_eq(110, total)
}

test "packaging_calculation_scenario" {
  // 包装计算场景
  let total_items = 23
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(23/6) = 4
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "boundary_value_testing" {
  // 边界值测试
  assert_eq(1, add(0, 1))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq_string("Hello, A!", greet("A"))
}

test "real_world_financial_calculation" {
  // 实际金融计算：小费计算
  let bill_amount = 85
  let tip_percentage = 15
  
  let tip_amount = divide_with_ceil(multiply(bill_amount, tip_percentage), 100)
  let total = add(bill_amount, tip_amount)
  
  assert_eq(13, tip_amount)  // ceil(85*15/100) = ceil(12.75) = 13
  assert_eq(98, total)       // 85 + 13 = 98
}