// 标准 Azimuth 库完整测试用例
// 使用标准 MoonBit 测试语法，覆盖所有核心功能

test "complete_arithmetic_operations" {
  // 完整的算术运算测试
  assert_eq(15, add(8, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  
  assert_eq(24, multiply(6, 4))
  assert_eq(0, multiply(0, 99))
  assert_eq(-18, multiply(-3, 6))
  
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 交换律验证
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 单位元性质
  assert_eq(a, add(a, 0))
  assert_eq(a, multiply(a, 1))
  assert_eq(0, multiply(a, 0))
}

test "string_greeting_comprehensive" {
  // 全面的字符串问候测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "negative_number_operations_complete" {
  // 负数运算完整测试
  assert_eq(-5, add(-2, -3))
  assert_eq(3, add(-5, 8))
  assert_eq(0, add(-7, 7))
  
  assert_eq(15, multiply(-3, -5))
  assert_eq(-20, multiply(4, -5))
  
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-9, -3))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "business_calculation_scenario" {
  // 业务计算场景测试
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping = 12
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax)
  assert_eq(304, total)
}

test "boundary_and_extreme_values" {
  // 边界和极值测试
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq(100, divide_with_ceil(9999, 100))
  
  assert_eq(1000000, add(500000, 500000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "complex_expression_evaluation" {
  // 复杂表达式求值测试
  let x = 10
  let y = 15
  let z = 20
  
  // (x + y) * z - x / y
  let result1 = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(499, result1)  // (10 + 15) * 20 - ceil(10/15) = 25*20 - 1 = 500 - 1 = 499
  
  // 分配律验证：a * (b + c) = a * b + a * c
  let distributive_left = multiply(x, add(y, z))
  let distributive_right = add(multiply(x, y), multiply(x, z))
  assert_eq(distributive_left, distributive_right)
}

test "real_world_packaging_calculation" {
  // 真实世界包装计算测试
  let items_per_box = 24
  let total_products = 85
  
  let boxes_needed = divide_with_ceil(total_products, items_per_box)
  let remaining_space = multiply(boxes_needed, items_per_box) - total_products
  
  assert_eq(4, boxes_needed)  // ceil(85/24) = 4
  assert_eq(11, remaining_space)  // 4*24 - 85 = 96 - 85 = 11
}

test "assertion_functions_test" {
  // 断言函数测试
  // 这些测试验证断言函数本身能够正常工作
  assert_eq(42, add(20, 22))
  assert_eq_string("Hello, Test!", greet("Test"))
  
  // 测试相等性断言的边界情况
  assert_eq(0, add(5, -5))
  assert_eq(-100, multiply(-10, 10))
}

test "comprehensive_function_integration" {
  // 综合功能集成测试
  let base_value = 100
  let multiplier = 3
  let divisor = 7
  let name = "MoonBit"
  
  // 计算增强值
  let enhanced = multiply(base_value, multiplier)
  // 计算分组数量
  let groups = divide_with_ceil(enhanced, divisor)
  // 计算最终结果
  let final_result = add(enhanced, groups)
  
  // 生成问候消息
  let greeting = greet(name)
  
  assert_eq(300, enhanced)
  assert_eq(43, groups)  // ceil(300/7) = 43
  assert_eq(343, final_result)  // 300 + 43 = 343
  assert_eq_string("Hello, MoonBit!", greeting)
}