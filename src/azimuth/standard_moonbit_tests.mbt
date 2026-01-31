// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(-3, 13))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
}

test "greet_function_basic_tests" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "zero_element_properties" {
  // 测试零元素性质
  let x = 42
  
  // 加法零元素
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法零元素
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算订单总价：商品价格 × 数量 + 运费
  let unit_price = 25
  let quantity = 8
  let shipping_cost = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping_cost)
  
  assert_eq(200, subtotal)
  assert_eq(215, total)
}

test "packaging_calculation_scenario" {
  // 测试包装计算场景
  let total_items = 47
  let items_per_box = 12
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let total_capacity = multiply(boxes_needed, items_per_box)
  let remaining_space = add(total_capacity, -total_items)
  
  assert_eq(4, boxes_needed)
  assert_eq(48, total_capacity)
  assert_eq(1, remaining_space)
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 123!", greet("123"))
}