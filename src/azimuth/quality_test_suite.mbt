// Quality Test Suite for Azimuth Project
// 高质量测试套件，覆盖核心功能

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(0, add(0, 0))
  assert_eq(-8, add(-3, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  assert_eq(100, left_side)
}

test "divide_with_ceil_comprehensive_behavior" {
  // 全面测试向上取整除法的行为
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  assert_eq(4, divide_with_ceil(-11, -3)) // -11/-3 = 3.666... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为0的安全处理
  assert_eq(0, divide_with_ceil(0, 5))    // 0除以任何数
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, !", greet(""))  // 空字符串处理
  assert_eq_string("Hello, 123!", greet("123"))  // 数字字符串
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping = 12
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = add(subtotal, tax)
  let final_total = add(add(total_before_discount, shipping), -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax)
  assert_eq(397, total_before_discount)
  assert_eq(389, final_total)
}

test "mathematical_series_summation" {
  // 数学级数求和：等差数列求和
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(120, sum_formula)
}

test "inventory_packaging_optimization" {
  // 库存包装优化计算
  let items_per_box = 24
  let total_items_small = 100
  let total_items_medium = 250
  let total_items_large = 1000
  
  let boxes_small = divide_with_ceil(total_items_small, items_per_box)
  let boxes_medium = divide_with_ceil(total_items_medium, items_per_box)
  let boxes_large = divide_with_ceil(total_items_large, items_per_box)
  let total_boxes = add(add(boxes_small, boxes_medium), boxes_large)
  
  assert_eq(5, boxes_small)
  assert_eq(11, boxes_medium)
  assert_eq(42, boxes_large)
  assert_eq(58, total_boxes)
}

test "time_project_management_calculation" {
  // 项目时间管理计算
  let tasks_per_day = 8
  let total_tasks = 45
  let working_days = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(working_days, 8)
  
  assert_eq(6, working_days)  // ceil(45/8) = 6
  assert_eq(48, total_hours)  // 6 * 8 = 48
}

test "negative_number_operations_comprehensive" {
  // 负数运算的综合测试
  assert_eq(-10, add(-3, -7))
  assert_eq(2, add(-5, 7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
}

test "extreme_boundary_value_testing" {
  // 极端边界值测试
  let large_positive = 1000000
  let large_negative = -1000000
  
  assert_eq(2000000, add(large_positive, large_positive))
  assert_eq(-2000000, add(large_negative, large_negative))
  assert_eq(0, add(large_positive, large_negative))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(large_positive, 1))
}