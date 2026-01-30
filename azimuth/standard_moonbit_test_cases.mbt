// 标准 MoonBit 测试用例
// 为 azimuth 库添加高质量测试覆盖

test "arithmetic_commutative_property" {
  // 测试算术交换律性质
  let a = 17
  let b = 29
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(46, add(17, 29))
  assert_eq(493, multiply(17, 29))
}

test "divide_with_ceil_precision_scenarios" {
  // 测试向上取整除法的精度场景
  // 测试小数部分刚好为0的情况
  assert_eq(5, divide_with_ceil(20, 4))
  assert_eq(8, divide_with_ceil(32, 4))
  
  // 测试小数部分接近1的情况
  assert_eq(6, divide_with_ceil(23, 4))
  assert_eq(9, divide_with_ceil(35, 4))
  
  // 测试大数精度
  assert_eq(1001, divide_with_ceil(10001, 10))
  assert_eq(10001, divide_with_ceil(100001, 10))
}

test "string_greeting_multilingual_support" {
  // 测试多语言字符串问候支持
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, русский!", greet("русский"))
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  let a = 6
  let b = 8
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
  assert_eq(66, left_side)  // 6 * (8 + 3) = 66
}

test "business_inventory_management" {
  // 业务场景：库存管理测试
  let total_products = 158
  let box_capacity = 25
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(7, boxes_needed)  // ceil(158/25) = 7
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(175, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(17, remaining_space)
}

test "negative_number_comprehensive_operations" {
  // 负数综合运算测试
  let negative_a = -15
  let positive_b = 8
  let negative_c = -3
  
  // 负数加法
  assert_eq(-7, add(negative_a, positive_b))
  assert_eq(-18, add(negative_a, negative_c))
  
  // 负数乘法
  assert_eq(-120, multiply(negative_a, positive_b))
  assert_eq(45, multiply(negative_a, negative_c))
  
  // 负数除法
  assert_eq(-1, divide_with_ceil(negative_a, positive_b))
  assert_eq(5, divide_with_ceil(negative_a, negative_c))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：计算项目资源需求
  let team_size = 12
  let hours_per_day = 8
  let working_days = 15
  let tasks_per_hour = 2
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(hours_per_day, working_days))
  assert_eq(1440, total_hours)  // 12 * 8 * 15 = 1440
  
  // 计算总任务数
  let total_tasks = multiply(total_hours, tasks_per_hour)
  assert_eq(2880, total_tasks)
  
  // 计算需要的冲刺数量（每冲刺400个任务）
  let sprints_needed = divide_with_ceil(total_tasks, 400)
  assert_eq(8, sprints_needed)  // ceil(2880/400) = 8
}

test "edge_case_boundary_values" {
  // 边界值测试
  // 测试1的运算
  assert_eq(2, add(1, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 测试-1的运算
  assert_eq(-2, add(-1, -1))
  assert_eq(1, multiply(-1, -1))
  assert_eq(1, divide_with_ceil(-1, -1))
  
  // 测试与0的运算
  assert_eq(1, add(1, 0))
  assert_eq(0, multiply(1, 0))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "unicode_and_emoji_comprehensive" {
  // Unicode和表情符号综合测试
  assert_eq_string("Hello, 🌟✨🚀!", greet("🌟✨🚀"))
  assert_eq_string("Hello, 🎉🎊🎈!", greet("🎉🎊🎈"))
  assert_eq_string("Hello, ❤️💙💚!", greet("❤️💙💚"))
  assert_eq_string("Hello, 🍎🍊🍋!", greet("🍎🍊🍋"))
  assert_eq_string("Hello, 🏠🏢🏣!", greet("🏠🏢🏣"))
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  // 场景：贷款分期计算
  let loan_amount = 50000
  let monthly_payment_capacity = 2200
  
  // 计算需要的月数
  let months_needed = divide_with_ceil(loan_amount, monthly_payment_capacity)
  assert_eq(23, months_needed)  // ceil(50000/2200) = 23
  
  // 计算实际总支付
  let actual_total_payment = multiply(months_needed, monthly_payment_capacity)
  assert_eq(50600, actual_total_payment)
  
  // 计算最后一个月的实际支付金额
  let regular_payments = months_needed - 1
  let regular_total = multiply(regular_payments, monthly_payment_capacity)
  let final_payment = subtract(loan_amount, regular_total)
  assert_eq(200, final_payment)
}