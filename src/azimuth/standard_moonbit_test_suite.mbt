// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive_edge_cases" {
  // 全面测试向上取整除法的边界情况
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))  // 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(4, divide_with_ceil(-11, -3))  // 3.666... -> 4
  
  // 零处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "greet_function_unicode_and_special_characters" {
  // 测试问候函数的 Unicode 和特殊字符处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, user@example.com!", greet("user@example.com"))
}

test "complex_business_calculation_scenario" {
  // 复杂业务计算场景：订单总价计算
  let unit_price = 89
  let quantity = 4
  let tax_rate = 8
  let shipping = 15
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = add(subtotal, add(tax_amount, shipping))
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)
  assert_eq(400, total_before_discount)
  assert_eq(380, final_total)
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列求和
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(120, sum_formula)  // 10/2 * (6 + 18) = 5 * 24 = 120
}

test "inventory_packaging_optimization" {
  // 库存包装优化问题
  let total_items = 127
  let box_capacity_a = 24
  let box_capacity_b = 18
  let box_capacity_c = 12
  
  // 计算每种包装箱需要的数量
  let boxes_a = divide_with_ceil(total_items, box_capacity_a)
  let boxes_b = divide_with_ceil(total_items, box_capacity_b)
  let boxes_c = divide_with_ceil(total_items, box_capacity_c)
  
  // 验证计算结果
  assert_eq(6, boxes_a)   // ceil(127/24) = 6
  assert_eq(8, boxes_b)   // ceil(127/18) = 8
  assert_eq(11, boxes_c)  // ceil(127/12) = 11
}

test "time_and_resource_management" {
  // 时间和资源管理计算
  let total_tasks = 47
  let tasks_per_day = 8
  let working_days_per_week = 5
  
  // 计算需要的工作日
  let workdays_needed = divide_with_ceil(total_tasks, tasks_per_day)
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(workdays_needed, working_days_per_week)
  
  assert_eq(6, workdays_needed)  // ceil(47/8) = 6
  assert_eq(2, weeks_needed)     // ceil(6/5) = 2
}

test "financial_compound_interest_simplified" {
  // 简化的复利计算
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 第一年
  let amount_year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let amount_year2 = add(amount_year1, divide_with_ceil(multiply(amount_year1, rate_percent), 100))
  // 第三年
  let amount_year3 = add(amount_year2, divide_with_ceil(multiply(amount_year2, rate_percent), 100))
  
  assert_eq(1050, amount_year1)
  assert_eq(1103, amount_year2)
  assert_eq(1158, amount_year3)
}

test "error_handling_and_boundary_values" {
  // 错误处理和边界值测试
  // 极端值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(100, 0))
  
  // 大数运算
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  
  // 边界组合运算
  let complex_result = add(
    multiply(divide_with_ceil(100, 3), 5),
    multiply(divide_with_ceil(-50, 3), -2)
  )
  assert_eq(70, complex_result)  // ceil(100/3)*5 + ceil(-50/3)*-2 = 34*5 + (-16)*-2 = 170 + 32 = 202
}