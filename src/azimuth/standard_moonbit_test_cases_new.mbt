// 新增的 MoonBit 标准测试用例
// 测试 azimuth 库的核心功能

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  let a = 3
  let b = 4
  let c = 5
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
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

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 5
  let common_diff = 3
  let terms = 6
  
  // 使用公式 S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(terms, add(multiply(2, first_term), multiply(terms - 1, common_diff))),
    2
  )
  
  assert_eq(78, sum)
}

test "negative_number_operations_comprehensive" {
  // 负数运算综合测试
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-5, 7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "real_world_packaging_optimization" {
  // 实际包装优化问题
  let items_a = 23
  let items_b = 17
  let items_c = 31
  let capacity_per_box = 12
  
  let boxes_a = divide_with_ceil(items_a, capacity_per_box)
  let boxes_b = divide_with_ceil(items_b, capacity_per_box)
  let boxes_c = divide_with_ceil(items_c, capacity_per_box)
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  
  assert_eq(2, boxes_a)
  assert_eq(2, boxes_b)
  assert_eq(3, boxes_c)
  assert_eq(7, total_boxes)
}

test "string_greeting_edge_cases" {
  // 字符串问候边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
}

test "algorithm_efficiency_simulation" {
  // 算法效率模拟
  let base_operations = 1000
  let optimization_factor = 4
  let overhead_percentage = 20
  
  let optimized_ops = multiply(base_operations, optimization_factor)
  let overhead = divide_with_ceil(multiply(optimized_ops, overhead_percentage), 100)
  let effective_ops = add(optimized_ops, -overhead)
  
  assert_eq(4000, optimized_ops)
  assert_eq(800, overhead)
  assert_eq(3200, effective_ops)
}