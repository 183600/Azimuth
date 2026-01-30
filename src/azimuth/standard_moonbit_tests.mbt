// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含基本功能测试和实际应用场景

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive_test" {
  // 全面测试向上取整除法功能
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))   // 10/3 = 3.33 -> 4
  assert_eq(3, divide_with_ceil(9, 3))    // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.33 -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // -10/-3 = 3.33 -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为0返回0
  assert_eq(0, divide_with_ceil(0, 5))     // 0除以任何数返回0
}

test "greet_function_international_support" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, México!", greet("México"))
  assert_eq_string("Hello, ,!@#$!", greet(",!@#$"))
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
  // 计算总价（包含运费）
  let total_before_discount = add(subtotal, shipping_cost)
  // 应用折扣
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(371, total_before_discount)
  assert_eq(351, final_total)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 3
  let common_difference = 2
  let terms_count = 10
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let double_sum = multiply(terms_count, add(multiply(2, first_term), multiply(terms_count - 1, common_difference)))
  let sum = divide_with_ceil(double_sum, 2)
  
  assert_eq(120, sum)  // 10/2 * (6 + 18) = 5 * 24 = 120
}

test "packaging_optimization" {
  // 包装优化：计算最少的包装箱数量
  let items_per_box = 24
  let total_items = 85
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let remaining_space = add(multiply(boxes_needed, items_per_box), -total_items)
  
  assert_eq(4, boxes_needed)        // ceil(85/24) = 4
  assert_eq(11, remaining_space)    // 4*24 - 85 = 11个空位
}

test "time_management_calculation" {
  // 时间管理计算：项目完成时间估算
  let tasks_per_day = 6
  let total_tasks = 47
  let work_hours_per_day = 8
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_work_hours = multiply(days_needed, work_hours_per_day)
  
  assert_eq(8, days_needed)      // ceil(47/6) = 8天
  assert_eq(64, total_work_hours) // 8天 * 8小时/天 = 64小时
}

test "financial_compound_interest" {
  // 金融复利计算（简化版）
  let principal = 1000
  let interest_rate = 5  // 5%
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, interest_rate), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, interest_rate), 100))
  
  assert_eq(1050, year1)  // 1000 + ceil(1000*5/100) = 1000 + 50 = 1050
  assert_eq(1103, year2)  // 1050 + ceil(1050*5/100) = 1050 + 53 = 1103
  assert_eq(1158, year3)  // 1103 + ceil(1103*5/100) = 1103 + 55 = 1158
}