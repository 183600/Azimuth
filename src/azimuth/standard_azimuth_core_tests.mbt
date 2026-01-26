// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含 8 个高质量的测试用例，覆盖主要功能和边界情况

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
  assert_eq(1000, add(500, 500))
  assert_eq(-1000, add(-500, -500))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(10000, multiply(100, 100))
}

test "greet_function_variations" {
  // 测试问候函数的各种情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法的功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "combined_operations_test" {
  // 测试组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = multiply(add(5, 3), 2)
  assert_eq(16, result2)  // (5 + 3) * 2 = 8 * 2 = 16
  
  let result3 = divide_with_ceil(add(10, 5), 4)
  assert_eq(4, result3)  // ceil(15/4) = 4
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(
    multiply(a, add(b, 2)),
    add(multiply(a, b), multiply(a, 2))
  )
}

test "practical_business_scenarios" {
  // 实际业务场景测试
  // 计算订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(200, subtotal)
  assert_eq(215, total)
  
  // 计算包装需求
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 零值操作
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, -10))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 大数操作
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 边界除法
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq(100, divide_with_ceil(100, 1))
}