// 标准 MoonBit 测试用例 - 针对 azimuth 库核心功能
// 包含基本数学运算、字符串处理和边界情况测试

test "add_function_basic_test" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_basic_test" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(25, multiply(5, 5))
}

test "subtract_function_basic_test" {
  // 测试基本减法功能
  assert_eq(1, subtract(5, 4))
  assert_eq(0, subtract(10, 10))
  assert_eq(-5, subtract(3, 8))
  assert_eq(100, subtract(150, 50))
}

test "greet_function_basic_test" {
  // 测试基本问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "divide_with_ceil_function_test" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "mathematical_properties_test" {
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

test "combined_operations_test" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
  
  let result3 = subtract(divide_with_ceil(20, 3), multiply(2, 3))
  assert_eq(1, result3)  // ceil(20/3) - 2*3 = 7 - 6 = 1
}

test "practical_business_scenario_test" {
  // 实际业务场景测试：计算订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = subtract(total_before_discount, discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_calculation_test" {
  // 包装计算场景测试
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 测试另一个包装场景
  let total_items_2 = 47
  let items_per_box_2 = 12
  let boxes_needed_2 = divide_with_ceil(total_items_2, items_per_box_2)
  
  assert_eq(4, boxes_needed_2)  // ceil(47/12) = 4
}

test "edge_cases_and_boundary_values_test" {
  // 边界值和极端情况测试
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-4, divide_with_ceil(-11, 3))
  
  // 测试零的运算
  assert_eq(0, multiply(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}