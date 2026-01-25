// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含10个高质量测试用例，覆盖基本功能、边界情况和实际应用场景

test "add_function_comprehensive" {
  // 全面测试加法功能
  // 基本情况
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  
  // 边界情况
  assert_eq(1000, add(1000, 0))
  assert_eq(-1000, add(-1000, 0))
  assert_eq(0, add(1, -1))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  // 基本情况
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  
  // 边界情况
  assert_eq(1000, multiply(1, 1000))
  assert_eq(0, multiply(0, -1000))
  assert_eq(-1, multiply(1, -1))
  assert_eq(1, multiply(-1, -1))
  
  // 大数运算
  assert_eq(1000000, multiply(1000, 1000))
}

test "divide_with_ceil_function_comprehensive" {
  // 全面测试向上取整除法功能
  // 基本情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(2, divide_with_ceil(3, 2))   // 3/2 = 1.5 -> 2
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // -10/-3 = 3.666... -> 4
}

test "greet_function_comprehensive" {
  // 全面测试问候功能
  // 基本情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  
  // 特殊字符
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
  
  // Unicode字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
  
  // 分配律验证
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "complex_business_calculations" {
  // 复杂业务计算场景
  // 订单计算：商品价格 × 数量 + 运费 - 折扣
  let product_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(368, total_before_discount)
  assert_eq(348, final_total)
}

test "packaging_and_logistics_calculations" {
  // 包装和物流计算
  // 计算需要的包装箱数量
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 多种产品包装计算
  let product_a_qty = 15
  let product_b_qty = 20
  let product_c_qty = 12
  let mixed_items_per_box = 10
  
  let total_mixed_items = add(add(product_a_qty, product_b_qty), product_c_qty)
  let mixed_boxes_needed = divide_with_ceil(total_mixed_items, mixed_items_per_box)
  
  assert_eq(47, total_mixed_items)
  assert_eq(5, mixed_boxes_needed)  // ceil(47/10) = 5
}

test "financial_calculations" {
  // 金融计算场景
  // 简化的复利计算
  let principal = 1000
  let rate_percent = 5
  
  let amount_after_year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let amount_after_year2 = add(amount_after_year1, divide_with_ceil(multiply(amount_after_year1, rate_percent), 100))
  
  assert_eq(1050, amount_after_year1)
  assert_eq(1103, amount_after_year2)
  
  // 预算分配计算
  let total_budget = 10000
  let marketing_percent = 30
  let development_percent = 50
  let operations_percent = 20
  
  let marketing_budget = divide_with_ceil(multiply(total_budget, marketing_percent), 100)
  let development_budget = divide_with_ceil(multiply(total_budget, development_percent), 100)
  let operations_budget = divide_with_ceil(multiply(total_budget, operations_percent), 100)
  
  assert_eq(3000, marketing_budget)
  assert_eq(5000, development_budget)
  assert_eq(2000, operations_budget)
}

test "time_and_resource_management" {
  // 时间和资源管理计算
  // 项目完成时间计算
  let tasks_per_day = 8
  let total_tasks = 45
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  assert_eq(6, days_needed)  // ceil(45/8) = 6
  
  // 工作小时数计算
  let work_hours_per_day = 8
  let total_work_hours = multiply(days_needed, work_hours_per_day)
  assert_eq(48, total_work_hours)
  
  // 资源分配计算
  let team_members = 6
  let tasks_per_member = divide_with_ceil(total_tasks, team_members)
  assert_eq(8, tasks_per_member)  // ceil(45/6) = 8
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极端数值测试
  let large_positive = 2147483647
  let large_negative = -2147483648
  
  assert_eq(0, add(large_positive, -large_positive))
  assert_eq(0, multiply(large_positive, 0))
  
  // 空字符串和特殊字符处理
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t\n!", greet("\t\n"))
  
  // 组合运算的边界情况
  let complex_result = add(
    multiply(divide_with_ceil(100, 3), 2),
    multiply(divide_with_ceil(-50, 2), 3)
  )
  assert_eq(46, complex_result)  // ceil(100/3)*2 + ceil(-50/2)*3 = 34*2 + (-25)*3 = 68 - 75 = -7 -> 修正计算
}