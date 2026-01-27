// 标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的标准测试，包含10个测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "negative_number_calculations" {
  // 测试负数计算
  assert_eq(-8, add(-3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_standard_cases" {
  // 测试问候函数标准情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "combined_operations_complexity" {
  // 测试组合运算复杂性
  let result1 = add(multiply(2, 4), multiply(3, 5))
  assert_eq(23, result1)  // 2*4 + 3*5 = 8 + 15 = 23
  
  let result2 = multiply(add(5, 7), 3)
  assert_eq(36, result2)  // (5 + 7) * 3 = 12 * 3 = 36
}

test "edge_boundary_values" {
  // 测试边界值
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
}

test "practical_application_scenarios" {
  // 测试实际应用场景
  // 包装计算
  let total_items = 17
  let items_per_box = 5
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)  // ceil(17/5) = 4
  
  // 价格计算
  let unit_price = 12
  let quantity = 6
  let total_cost = multiply(unit_price, quantity)
  assert_eq(72, total_cost)
}

test "unicode_and_special_characters" {
  // 测试Unicode和特殊字符
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}

test "error_handling_and_robustness" {
  // 测试错误处理和健壮性
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数运算
  assert_eq(20000, add(10000, 10000))
  assert_eq(1000000, multiply(1000, 1000))
}