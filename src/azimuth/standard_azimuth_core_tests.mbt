// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含 10 个高质量的测试用例，使用标准 MoonBit 测试语法

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  assert_eq(100, left_side)  // 5 × (8 + 12) = 5 × 20 = 100
}

test "divide_with_ceil_fractional_numbers" {
  // 测试向上取整除法处理小数的情况
  assert_eq(4, divide_with_ceil(10, 3))   // 3.333... → 4
  assert_eq(3, divide_with_ceil(9, 3))    // 3 → 3
  assert_eq(2, divide_with_ceil(5, 3))    // 1.666... → 2
  assert_eq(1, divide_with_ceil(1, 100))  // 0.01 → 1
  assert_eq(0, divide_with_ceil(0, 5))    // 0 → 0
  assert_eq(0, divide_with_ceil(10, 0))   // 除零保护 → 0
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, México!", greet("México"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, !", greet(""))  // 空字符串测试
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 89
  let quantity = 4
  let shipping_cost = 15
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算总价（含运费）
  let total_before_discount = add(subtotal, shipping_cost)
  // 应用折扣
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)           // 89 × 4 = 356
  assert_eq(371, total_before_discount)  // 356 + 15 = 371
  assert_eq(351, final_total)        // 371 - 20 = 351
}

test "mathematical_series_summation" {
  // 数学级数求和：等差数列求和
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式：S = n/2 × (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(120, sum)  // 10/2 × (6 + 18) = 5 × 24 = 120
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))     // 负数加法
  assert_eq(-5, add(-8, 3))      // 正负数加法
  assert_eq(0, add(-5, 5))       // 相反数相加
  assert_eq(15, multiply(-3, -5)) // 负数乘法
  assert_eq(-15, multiply(3, -5)) // 正负数乘法
  assert_eq(-3, divide_with_ceil(-10, 3)) // 负数除法
  assert_eq(4, divide_with_ceil(-10, -3)) // 负数除负数
}

test "packaging_optimization" {
  // 包装优化计算：计算需要的包装箱数量
  let total_items = 47
  let items_per_small_box = 12
  let items_per_large_box = 25
  
  let small_boxes_needed = divide_with_ceil(total_items, items_per_small_box)
  let large_boxes_needed = divide_with_ceil(total_items, items_per_large_box)
  
  assert_eq(4, small_boxes_needed)  // ceil(47/12) = 4
  assert_eq(2, large_boxes_needed)  // ceil(47/25) = 2
}

test "time_management_calculation" {
  // 时间管理计算：项目完成时间预估
  let total_tasks = 35
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let weeks_needed = divide_with_ceil(days_needed, work_days_per_week)
  
  assert_eq(5, days_needed)   // ceil(35/8) = 5 天
  assert_eq(1, weeks_needed)  // ceil(5/5) = 1 周
}

test "financial_compound_interest" {
  // 金融复利计算（简化版）
  let principal = 1000
  let interest_rate = 5  // 5%
  let years = 3
  
  // 第一年
  let amount_year1 = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  // 第二年
  let amount_year2 = add(amount_year1, divide_with_ceil(multiply(amount_year1, interest_rate), 100))
  // 第三年
  let amount_year3 = add(amount_year2, divide_with_ceil(multiply(amount_year2, interest_rate), 100))
  
  assert_eq(1050, amount_year1)  // 1000 + ceil(1000×5/100) = 1000 + 50 = 1050
  assert_eq(1103, amount_year2)  // 1050 + ceil(1050×5/100) = 1050 + 53 = 1103
  assert_eq(1158, amount_year3)  // 1103 + ceil(1103×5/100) = 1103 + 55 = 1158
}