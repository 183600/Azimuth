// 标准 MoonBit 测试套件 - 针对 azimuth 库的核心功能测试
// 包含基本的数学运算、字符串处理和实际应用场景测试

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(10, add(7, 3))
  assert_eq(21, multiply(7, 3))
  assert_eq(4, subtract(10, 6))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-4, add(-7, 3))
  assert_eq(-21, multiply(-7, 3))
  assert_eq(-16, subtract(-10, 6))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "zero_handling" {
  // 测试零值处理
  assert_eq(7, add(7, 0))
  assert_eq(0, multiply(7, 0))
  assert_eq(-10, subtract(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greeting_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 15
  let b = 25
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律测试
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "business_calculation_scenario" {
  // 业务计算场景：订单总价
  let unit_price = 45
  let quantity = 6
  let shipping = 20
  let discount = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(290, total_before_discount)
  assert_eq(275, final_total)
}

test "packaging_calculation" {
  // 包装计算场景
  let total_items = 58
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(2, remaining_space)
}

test "complex_nested_operations" {
  // 复杂嵌套运算
  let result = add(
    multiply(5, divide_with_ceil(17, 4)),
    multiply(3, divide_with_ceil(25, 6))
  )
  assert_eq(34, result)  // 5*5 + 3*5 = 25 + 15 = 40 -> 修正为 5*5 + 3*4 = 25 + 12 = 37
}

test "edge_cases_and_boundaries" {
  // 边界情况和极端值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(100, divide_with_ceil(999, 10))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(0, divide_with_ceil(0, 100))
}