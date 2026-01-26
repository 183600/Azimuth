// MoonBit 标准测试用例 - 针对 azimuth 库核心功能的全面测试
// 包含基础功能、边界情况和实际应用场景

test "azimuth_add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  
  // 测试包含零的情况
  assert_eq(add(a, 0), a)
  assert_eq(add(0, b), b)
  
  // 测试负数情况
  assert_eq(add(-5, 10), add(10, -5))
}

test "azimuth_multiply_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "azimuth_divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))  // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(3, divide_with_ceil(10, -3))   // -3.333... -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // 3.333... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为零
  assert_eq(0, divide_with_ceil(0, 5))    // 被除数为零
}

test "azimuth_greet_unicode_handling" {
  // 测试问候函数的 Unicode 字符处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  
  // 特殊字符和数字
  assert_eq_string("Hello, User123!", greet("User123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  
  // 空字符串和空格
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "azimuth_complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping = 15
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_before_discount = add(add(subtotal, tax_amount), shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)  // ceil(356 * 8 / 100) = ceil(28.48) = 29
  assert_eq(400, total_before_discount)
  assert_eq(380, final_total)
}

test "azimuth_packaging_optimization" {
  // 包装优化问题：计算最少需要的包装箱数量
  let items_per_box = 24
  
  // 不同数量的商品
  let order1_items = 100
  let order2_items = 250
  let order3_items = 1000
  
  let boxes1 = divide_with_ceil(order1_items, items_per_box)
  let boxes2 = divide_with_ceil(order2_items, items_per_box)
  let boxes3 = divide_with_ceil(order3_items, items_per_box)
  
  let total_boxes = add(add(boxes1, boxes2), boxes3)
  
  assert_eq(5, boxes1)   // ceil(100/24) = 5
  assert_eq(11, boxes2)  // ceil(250/24) = 11
  assert_eq(42, boxes3)  // ceil(1000/24) = 42
  assert_eq(58, total_boxes)
}

test "azimuth_mathematical_series" {
  // 数学级数：计算等差数列的和
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(120, sum_formula)  // 10/2 * (6 + 18) = 5 * 24 = 120
}

test "azimuth_resource_allocation" {
  // 资源分配：项目预算分配
  let total_budget = 10000
  let project_costs = multiply(1200, 6)
  let remaining_budget = add(total_budget, -project_costs)
  
  // 计算可以支持的小项目数量
  let small_project_cost = 800
  let small_projects = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(7200, project_costs)
  assert_eq(2800, remaining_budget)
  assert_eq(4, small_projects)
}

test "azimuth_time_management" {
  // 时间管理：任务完成时间计算
  let tasks_per_day = 8
  let total_tasks = 67
  
  let workdays_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_work_hours = multiply(workdays_needed, 8)
  
  // 计算剩余时间（假设每天还有2小时额外工作）
  let extra_hours = multiply(workdays_needed, 2)
  let total_hours_including_extra = add(total_work_hours, extra_hours)
  
  assert_eq(9, workdays_needed)      // ceil(67/8) = 9
  assert_eq(72, total_work_hours)    // 9 * 8 = 72
  assert_eq(90, total_hours_including_extra)
}

test "azimuth_financial_compound_interest" {
  // 金融计算：简化复利计算
  let principal = 5000
  let rate_percent = 6
  let years = 3
  
  // 第一年
  let amount_year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  
  // 第二年
  let amount_year2 = add(amount_year1, divide_with_ceil(multiply(amount_year1, rate_percent), 100))
  
  // 第三年
  let amount_year3 = add(amount_year2, divide_with_ceil(multiply(amount_year2, rate_percent), 100))
  
  assert_eq(5300, amount_year1)  // 5000 + ceil(5000*6/100) = 5000 + 300 = 5300
  assert_eq(5618, amount_year2)  // 5300 + ceil(5300*6/100) = 5300 + 318 = 5618
  assert_eq(5955, amount_year3)  // 5618 + ceil(5618*6/100) = 5618 + 337 = 5955
}