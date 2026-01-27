// 标准 MoonBit 测试用例 - azimuth 库核心功能测试
// 包含基本功能、边界情况和实际应用场景的测试

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(10, add(7, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
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
  let total_items = 28
  let items_per_box = 8
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "combined_operations" {
  // 组合运算测试
  let result = add(multiply(3, 4), multiply(5, 2))
  assert_eq(22, result)  // 3*4 + 5*2 = 12 + 10 = 22
}

test "edge_cases" {
  // 边界情况测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(5, 10))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq_string("Hello, A!", greet("A"))
}