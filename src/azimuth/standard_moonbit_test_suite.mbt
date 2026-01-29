// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_basic_functionality" {
  // 测试乘法函数的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_basic_functionality" {
  // 测试问候函数的基本功能
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
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-2, divide_with_ceil(-4, 2))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "mathematical_properties_identity" {
  // 测试单位元性质
  let x = 15
  
  // 加法单位元
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法单位元
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
}

test "combined_operations_complex" {
  // 测试复杂的组合运算
  let x = 10
  let y = 15
  let z = 20
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(500, result1)  // (10 + 15) * 20 = 25 * 20 = 500
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(170, result2)  // 10 * 15 + 20 = 150 + 20 = 170
}

test "practical_business_scenario" {
  // 实际业务场景：计算订单总价
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
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 测试更大的数字
  let large_items = 100
  let large_boxes = divide_with_ceil(large_items, items_per_box)
  assert_eq(17, large_boxes)  // ceil(100/6) = 17
}