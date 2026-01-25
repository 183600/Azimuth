// 新增的10个标准 MoonBit 测试用例

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 3 * (4 + 5) = 3 * 4 + 3 * 5 = 27
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 全面测试向上取整除法的各种场景
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))   // 10/3 = 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))    // 9/3 = 3 -> 3
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_international_characters" {
  // 测试问候函数处理国际字符的能力
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, México!", greet("México"))
  assert_eq_string("Hello, !", greet(""))
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
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = add(subtotal, tax)
  let final_total = add(add(total_before_discount, shipping), -discount)
  
  assert_eq(356, subtotal)     // 89 * 4 = 356
  assert_eq(29, tax)           // ceil(356 * 8 / 100) = ceil(28.48) = 29
  assert_eq(377, final_total)  // 356 + 29 + 12 - 20 = 377
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
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

test "packaging_optimization" {
  // 包装优化问题：计算最少需要的包装箱数量
  let small_items = 25
  let medium_items = 47
  let large_items = 83
  let items_per_box = 12
  
  // 计算每种物品需要的箱子数量
  let small_boxes = divide_with_ceil(small_items, items_per_box)
  let medium_boxes = divide_with_ceil(medium_items, items_per_box)
  let large_boxes = divide_with_ceil(large_items, items_per_box)
  
  // 计算总箱子数
  let total_boxes = add(add(small_boxes, medium_boxes), large_boxes)
  
  assert_eq(3, small_boxes)   // ceil(25/12) = 3
  assert_eq(4, medium_boxes)  // ceil(47/12) = 4
  assert_eq(7, large_boxes)   // ceil(83/12) = 7
  assert_eq(14, total_boxes)  // 3 + 4 + 7 = 14
}

test "time_management_calculation" {
  // 时间管理计算：项目完成时间估算
  let total_tasks = 67
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  // 计算需要的工作日
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数（向上取整）
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  assert_eq(9, work_days_needed)  // ceil(67/8) = 9
  assert_eq(2, weeks_needed)      // ceil(9/5) = 2
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
  
  assert_eq(1050, amount_year1)  // 1000 + ceil(1000*5/100) = 1000 + 50 = 1050
  assert_eq(1103, amount_year2)  // 1050 + ceil(1050*5/100) = 1050 + 53 = 1103
  assert_eq(1158, amount_year3)  // 1103 + ceil(1103*5/100) = 1103 + 55 = 1158
}

test "error_handling_boundary_values" {
  // 错误处理和边界值测试
  // 极值测试
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与相反数相加
  assert_eq(0, multiply(0, 2147483647))       // 零乘以最大值
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(1, 1))         // 相等数相除
  assert_eq(0, divide_with_ceil(5, 0))         // 除数为零
  
  // 空字符串和特殊字符处理
  assert_eq_string("Hello, !", greet(""))      // 空字符串
  assert_eq_string("Hello,   !", greet("  "))  // 空格字符串
}