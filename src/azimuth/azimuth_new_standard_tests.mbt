// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "addition_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(-5, 10), add(10, -5))
  assert_eq(add(0, 100), add(100, 0))
}

test "multiplication_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 4
  let b = 7
  let c = 9
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))  // 完全相等
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(100, divide_with_ceil(9999, 100))  // 99.99 -> 100
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(0, divide_with_ceil(0, 100))  // 0除以任何数
  assert_eq(0, divide_with_ceil(50, 0))  // 除数为0的情况
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  let product_price = 89
  let quantity = 4
  let tax_rate = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(add(subtotal, tax_amount), shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)
  assert_eq(400, total_before_discount)
  assert_eq(380, final_total)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(5, add(-8, 13))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(4, divide_with_ceil(-11, -3))
}

test "large_number_operations" {
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(333334, divide_with_ceil(1000000, 3))
  assert_eq(10000, divide_with_ceil(100000000, 10000))
}

test "zero_handling_comprehensive" {
  // 全面测试零的处理
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, -10))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq_string("Hello, !", greet(""))
}

test "time_and_resource_calculation" {
  // 测试时间和资源计算
  let tasks_per_day = 8
  let total_tasks = 45
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(days_needed, 8)
  
  assert_eq(6, days_needed)  // ceil(45/8) = 6
  assert_eq(48, total_hours)  // 6 * 8 = 48
  
  // 测试包装计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let remaining_space = subtract(multiply(boxes_needed, items_per_box), total_items)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  assert_eq(1, remaining_space)  // 4*12 - 47 = 1
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算
  // 计算等差数列的和: 1 + 3 + 5 + 7 + 9 = 25
  let first_term = 1
  let common_difference = 2
  let number_of_terms = 5
  
  // 使用公式: S = n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(25, sum_formula)
  
  // 测试等比数列: 2, 4, 8, 16, 32
  let geometric_sum = add(add(add(add(2, 4), 8), 16), 32)
  assert_eq(62, geometric_sum)
}