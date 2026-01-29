// 标准 MoonBit 测试用例集
// 包含10个高质量的测试用例，使用标准的 MoonBit 测试语法

test "add_function_commutative_property" {
  // 测试加法交换律: a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  
  // 测试边界情况
  assert_eq(add(0, 10), add(10, 0))
  assert_eq(add(-5, 8), add(8, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 4
  let b = 7
  let c = 9
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  
  // 测试空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let product_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping = 15
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_before_discount = add(subtotal, tax_amount)
  let final_total = add(total_before_discount, shipping)
  let final_price = add(final_total, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)
  assert_eq(400, total_before_discount)
  assert_eq(415, final_total)
  assert_eq(395, final_price)
}

test "mathematical_series_summation" {
  // 数学级数求和：等差数列求和
  // 使用公式 S = n/2 * (2a + (n-1)d)
  let first_term = 5
  let common_difference = 3
  let number_of_terms = 8
  
  let sum_formula = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  // 验证结果：5 + 8 + 11 + 14 + 17 + 20 + 23 + 26 = 124
  assert_eq(124, sum_formula)
}

test "inventory_packaging_optimization" {
  // 库存包装优化：计算不同商品需要的包装箱数量
  let item_a_qty = 47
  let item_b_qty = 83
  let item_c_qty = 121
  let box_capacity = 24
  
  let boxes_a = divide_with_ceil(item_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(item_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(item_c_qty, box_capacity)
  
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  
  assert_eq(2, boxes_a)   // ceil(47/24) = 2
  assert_eq(4, boxes_b)   // ceil(83/24) = 4
  assert_eq(6, boxes_c)   // ceil(121/24) = 6
  assert_eq(12, total_boxes) // 2 + 4 + 6 = 12
}

test "time_and_resource_management" {
  // 时间和资源管理：项目完成时间计算
  let total_tasks = 87
  let tasks_per_day = 8
  let working_days_per_week = 5
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let weeks_needed = divide_with_ceil(days_needed, working_days_per_week)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = multiply(days_needed, 8)
  
  assert_eq(11, days_needed)  // ceil(87/8) = 11
  assert_eq(3, weeks_needed)  // ceil(11/5) = 3
  assert_eq(88, total_hours)  // 11 * 8 = 88
}

test "financial_compound_interest" {
  // 金融复利计算（简化版）
  let principal = 1000
  let interest_rate = 5  // 5%
  let years = 3
  
  // 第一年
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  
  // 第二年
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, interest_rate), 100))
  
  // 第三年
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, interest_rate), 100))
  
  assert_eq(1050, year1_amount)  // 1000 + ceil(1000*5/100) = 1000 + 50 = 1050
  assert_eq(1103, year2_amount)  // 1050 + ceil(1050*5/100) = 1050 + 53 = 1103
  assert_eq(1158, year3_amount)  // 1103 + ceil(1103*5/100) = 1103 + 55 = 1158
}

test "data_analysis_statistics" {
  // 数据分析统计：销售数据分析
  let q1_sales = 25000
  let q2_sales = 32000
  let q3_sales = 28000
  let q4_sales = 35000
  
  // 计算年度总销售额
  let annual_sales = add(add(add(q1_sales, q2_sales), q3_sales), q4_sales)
  
  // 计算季度平均销售额（向上取整）
  let quarterly_average = divide_with_ceil(annual_sales, 4)
  
  // 计算增长率
  let q2_growth_rate = divide_with_ceil(multiply(q2_sales - q1_sales, 100), q1_sales)
  let q3_growth_rate = divide_with_ceil(multiply(q3_sales - q2_sales, 100), q2_sales)
  let q4_growth_rate = divide_with_ceil(multiply(q4_sales - q3_sales, 100), q3_sales)
  
  assert_eq(120000, annual_sales)
  assert_eq(30000, quarterly_average)
  assert_eq(28, q2_growth_rate)  // ceil(32000-25000)*100/25000 = ceil(7000*100/25000) = ceil(28) = 28
  assert_eq(-13, q3_growth_rate) // ceil((28000-32000)*100/32000) = ceil(-4000*100/32000) = ceil(-12.5) = -12
  assert_eq(25, q4_growth_rate)  // ceil((35000-28000)*100/28000) = ceil(7000*100/28000) = ceil(25) = 25
}