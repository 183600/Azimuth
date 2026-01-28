// 新的综合测试用例 - 标准 MoonBit 测试语法
// 针对 azimuth 库的核心功能进行全面测试

test "addition_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  assert_eq(59, add(a, b))
}

test "multiplication_identity_element" {
  // 测试乘法单位元：a * 1 = a
  assert_eq(0, multiply(0, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(5, multiply(5, 1))
  assert_eq(10, multiply(10, 1))
  assert_eq(100, multiply(100, 1))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  assert_eq(0, divide_with_ceil(0, 5))   // 0/5 = 0 -> 0
  assert_eq(0, divide_with_ceil(10, 0))  // 除数为0的安全处理
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "complex_arithmetic_expression" {
  // 测试复杂算术表达式
  // 计算：(a + b) * c - d / e
  let a = 10
  let b = 15
  let c = 4
  let d = 20
  let e = 3
  
  let result = add(multiply(add(a, b), c), -divide_with_ceil(d, e))
  assert_eq(93, result)  // (10 + 15) * 4 - ceil(20/3) = 25 * 4 - 7 = 100 - 7 = 93
}

test "financial_calculation_scenario" {
  // 测试金融计算场景
  let principal = 1000
  let interest_rate = 5  // 5%
  
  // 简化的复利计算
  let year1 = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, interest_rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, interest_rate), 100))
  
  assert_eq(1050, year1)  // 1000 + 50
  assert_eq(1103, year2)  // 1050 + 53
  assert_eq(1158, year3)  // 1103 + 55
}

test "inventory_management_calculation" {
  // 测试库存管理计算
  let items_per_box = 24
  let total_items = 100
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let remaining_space = add(multiply(boxes_needed, items_per_box), -total_items)
  
  assert_eq(5, boxes_needed)      // ceil(100/24) = 5
  assert_eq(20, remaining_space) // 5*24 - 100 = 120 - 100 = 20
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 1234567890!", greet("1234567890"))
}

test "negative_number_division_behavior" {
  // 测试负数除法行为
  assert_eq(-3, divide_with_ceil(-10, 3))   // -10/3 = -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))   // -11/3 = -3.666... -> -4
  assert_eq(4, divide_with_ceil(-10, -3))   // -10/-3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(-9, -3))    // -9/-3 = 3 -> 3
}

test "performance_optimization_calculation" {
  // 测试性能优化计算
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 15
  
  let optimized_ops = multiply(base_operations, optimization_factor)
  let overhead = divide_with_ceil(multiply(optimized_ops, overhead_percentage), 100)
  let actual_performance = add(optimized_ops, -overhead)
  
  assert_eq(3000, optimized_ops)
  assert_eq(450, overhead)
  assert_eq(2550, actual_performance)
}