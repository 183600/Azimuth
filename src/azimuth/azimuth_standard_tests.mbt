// 标准 MoonBit 测试用例 - azimuth 库
// 包含10个全面的测试用例，覆盖库的主要功能

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-100, add(-60, -40))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 3
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 4 * (6 + 3) = 4 * 6 + 4 * 3 = 36
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 名字!", greet("名字"))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(7, divide_with_ceil(13, 2))  // 13/2 = 6.5 -> 7
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))  // -11/3 = -3.666... -> -4
  
  // 零除数安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 85
  let quantity = 12
  let tax_rate = 8  // 8%
  let shipping = 25
  let discount = 50
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = add(add(subtotal, tax), shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(1020, subtotal)
  assert_eq(82, tax)
  assert_eq(1127, total_before_discount)
  assert_eq(1077, final_total)
}

test "mathematical_series_summation" {
  // 数学级数求和：等差数列求和
  let first_term = 3
  let common_difference = 4
  let number_of_terms = 6
  
  // 使用公式 S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(78, sum)  // 6/2 * (6 + 20) = 3 * 26 = 78，等差数列：3+7+11+15+19+23 = 78
}

test "packaging_optimization" {
  // 包装优化：计算最少需要的包装箱数量
  let items_per_large_box = 24
  let items_per_small_box = 12
  let total_items = 87
  
  // 优先使用大箱子
  let large_boxes = total_items / items_per_large_box
  let remaining_items = add(total_items, -multiply(large_boxes, items_per_large_box))
  
  // 剩余物品用小箱子
  let small_boxes = if remaining_items > 0 {
    divide_with_ceil(remaining_items, items_per_small_box)
  } else {
    0
  }
  
  let total_boxes = add(large_boxes, small_boxes)
  
  assert_eq(3, large_boxes)    // 87/24 = 3
  assert_eq(15, remaining_items)  // 87 - 3*24 = 87 - 72 = 15
  assert_eq(2, small_boxes)   // ceil(15/12) = 2
  assert_eq(5, total_boxes)   // 总共5个箱子
}

test "time_management_calculation" {
  // 时间管理计算：项目完成时间估算
  let tasks_per_day = 6
  let total_tasks = 47
  let working_days_per_week = 5
  
  // 计算需要的工作日
  let workdays_needed = divide_with_ceil(total_tasks, tasks_per_day)
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(workdays_needed, working_days_per_week)
  
  assert_eq(8, workdays_needed)  // ceil(47/6) = 8
  assert_eq(2, weeks_needed)     // ceil(8/5) = 2
}

test "financial_compound_interest" {
  // 金融复利计算（简化版）
  let principal = 1000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, annual_rate), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, annual_rate), 100))
  
  assert_eq(1050, year1)  // 1000 + ceil(1000*5/100) = 1000 + 50 = 1050
  assert_eq(1103, year2)  // 1050 + ceil(1050*5/100) = 1050 + 53 = 1103
  assert_eq(1158, year3)  // 1103 + ceil(1103*5/100) = 1103 + 55 = 1158
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试各种边界情况和异常输入
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000, multiply(10000, 100000))
  
  // 零值运算
  assert_eq(0, multiply(0, 123456))
  assert_eq(123456, add(0, 123456))
  assert_eq(0, divide_with_ceil(0, 123456))
  
  // 负数运算
  assert_eq(-100, add(50, -150))
  assert_eq(150, multiply(-10, -15))
  assert_eq(-5, divide_with_ceil(-15, 3))
  
  // 极端除法情况
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(0, divide_with_ceil(999999, 0))
}