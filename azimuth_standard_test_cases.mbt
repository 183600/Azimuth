// 标准 MoonBit 测试用例 - 针对 azimuth 库
// 使用标准的 MoonBit 测试语法编写

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合功能
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边界情况
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(10, subtract(0, -10))
}

test "divide_with_ceil_various_scenarios" {
  // 测试向上取整除法的各种场景
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_calculation_scenario" {
  // 业务计算场景：订单总价计算
  let unit_price = 45
  let quantity = 6
  let shipping_cost = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(285, total_before_discount)
  assert_eq(265, final_total)
}

test "packaging_optimization_calculation" {
  // 包装优化计算
  let total_items = 58
  let items_per_box = 12
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let total_capacity = multiply(boxes_needed, items_per_box)
  let unused_space = subtract(total_capacity, total_items)
  
  assert_eq(5, boxes_needed)
  assert_eq(60, total_capacity)
  assert_eq(2, unused_space)
}

test "complex_nested_operations" {
  // 复杂嵌套运算测试
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let offset = 5
  
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let final_result = add(step2, offset)
  
  assert_eq(30, step1)
  assert_eq(8, step2)
  assert_eq(13, final_result)
}

test "error_handling_boundary_conditions" {
  // 错误处理和边界条件测试
  // 测试零值操作
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(123, 0))
  assert_eq(0, divide_with_ceil(0, 123))
  assert_eq(0, divide_with_ceil(999, 0))
  
  // 测试极值
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, add(0, 0))
}