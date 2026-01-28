// 高质量 MoonBit 测试用例
// 针对 azimuth 库核心功能的全面测试

test "arithmetic_commutative_property" {
  // 验证加法和乘法交换律
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))    // 10/3 = 3.33 -> 4
  assert_eq(3, divide_with_ceil(9, 3))     // 9/3 = 3 -> 3
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.33 -> -3
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为0的安全处理
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, !", greet(""))   // 空字符串处理
}

test "complex_business_calculation" {
  // 复杂业务场景计算：订单总价
  let unit_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(368, total_before_discount)
  assert_eq(348, final_total)
}

test "mathematical_distributive_property" {
  // 验证分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "packaging_optimization_scenario" {
  // 包装优化场景：计算最少需要的包装箱数量
  let items_per_box = 24
  let total_items = 115
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)  // ceil(115/24) = 5
}

test "financial_compound_interest" {
  // 金融复利计算简化版
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  let amount_year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let amount_year2 = add(amount_year1, divide_with_ceil(multiply(amount_year1, rate_percent), 100))
  let amount_year3 = add(amount_year2, divide_with_ceil(multiply(amount_year2, rate_percent), 100))
  
  assert_eq(1050, amount_year1)
  assert_eq(1103, amount_year2)
  assert_eq(1158, amount_year3)
}

test "edge_cases_and_boundary_values" {
  // 边界值和极端情况测试
  assert_eq(0, multiply(0, 2147483647))     // 零乘以最大值
  assert_eq(0, add(2147483647, -2147483647)) // 最大值加最小值+1
  assert_eq(1, divide_with_ceil(1, 2147483647)) // 1除以最大值
  assert_eq_string("Hello, A!", greet("A"))   // 单字符输入
}