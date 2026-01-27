// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含基本数学运算、字符串处理和实际应用场景的测试

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(0, add(5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))  // 10/3 = 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(-3, divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为0的安全处理
  assert_eq(1, divide_with_ceil(1, 100))  // 小数向上取整
}

test "greet_function_international_support" {
  // 测试问候函数的国际化和特殊字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 45
  let quantity = 6
  let shipping_cost = 18
  let discount = 25
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(288, total_before_discount)
  assert_eq(263, final_total)
}

test "packaging_and_shipping_calculation" {
  // 包装和运输计算
  let total_items = 58
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  let shipping_per_box = 8
  let total_shipping = multiply(boxes_needed, shipping_per_box)
  
  assert_eq(5, boxes_needed)  // ceil(58/12) = 5
  assert_eq(40, total_shipping)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 3
  let common_diff = 4
  let num_terms = 7
  
  // 使用公式计算等差数列和：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(105, sum)  // 7/2 * (6 + 24) = 3.5 * 30 = 105
}

test "financial_compound_interest" {
  // 金融复利计算
  let principal = 1000
  let interest_rate = 8
  
  // 简化的三年复利计算
  let year1 = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, interest_rate), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, interest_rate), 100))
  
  assert_eq(1080, year1)  // 1000 + 80 = 1080
  assert_eq(1167, year2)  // 1080 + 87 = 1167
  assert_eq(1261, year3)  // 1167 + 94 = 1261
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况
  assert_eq(0, divide_with_ceil(0, 0))      // 0除以0
  assert_eq(0, divide_with_ceil(100, 0))    // 正数除以0
  assert_eq(0, divide_with_ceil(-100, 0))   // 负数除以0
  assert_eq(0, multiply(0, -50))            // 0乘以任何数
  assert_eq(1000000, multiply(1000, 1000))  // 大数乘法
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合
  let prefix = "Dr."
  let name = "Alice"
  let suffix = "PhD"
  let full_name = prefix + " " + name + " " + suffix
  let formal_greeting = greet(full_name)
  
  assert_eq_string("Hello, Dr. Alice PhD!", formal_greeting)
  assert_eq_string("Hello, 123!", greet("123"))
}