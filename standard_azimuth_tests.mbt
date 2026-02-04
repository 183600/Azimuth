// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含基本功能测试、边界情况测试和实际应用场景

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(5, add(-2, 7))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的全面功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(1, multiply(1, 100))
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边界情况
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_commutative" {
  // 测试数学交换律性质
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

test "packaging_optimization_test" {
  // 包装优化测试
  let total_items = 58
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 计算总容量和剩余空间
  let total_capacity = multiply(boxes_needed, items_per_box)
  let wasted_space = subtract(total_capacity, total_items)
  
  assert_eq(5, boxes_needed)
  assert_eq(60, total_capacity)
  assert_eq(2, wasted_space)
}

test "complex_arithmetic_expression" {
  // 复杂算术表达式测试
  let x = 15
  let y = 8
  let z = 3
  
  // 测试表达式：(x + y) * z - x / y
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(68, result)  // (15 + 8) * 3 - ceil(15/8) = 23*3 - 2 = 69 - 2 = 67
  
  // 测试嵌套表达式
  let nested = divide_with_ceil(multiply(add(x, multiply(y, z)), 2), 5)
  assert_eq(20, nested)  // ceil((15 + 8*3) * 2 / 5) = ceil((15 + 24) * 2 / 5) = ceil(39*2/5) = ceil(78/5) = 16
}