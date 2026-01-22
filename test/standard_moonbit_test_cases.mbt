// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_with_positive_numbers" {
  // 测试正数加法
  assert_eq(10, add(3, 7))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "add_function_with_negative_numbers" {
  // 测试负数加法
  assert_eq(-10, add(-3, -7))
  assert_eq(0, add(-5, 5))
  assert_eq(-2, add(-8, 6))
}

test "multiply_function_comprehensive" {
  // 测试乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
}

test "greet_function_various_inputs" {
  // 测试问候函数的不同输入
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法的基本功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  let unit_price = 25
  let quantity = 8
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let final_total = add(subtotal, -discount)
  
  assert_eq(180, final_total)
}

test "packaging_optimization" {
  // 包装优化测试
  let total_items = 47
  let items_per_box = 12
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
}

test "mathematical_properties" {
  // 数学性质测试
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

test "real_world_shopping_cart" {
  // 真实世界购物车计算
  let item1_price = 29
  let item1_qty = 2
  let item2_price = 15
  let item2_qty = 3
  
  let total = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  
  assert_eq(103, total)
}