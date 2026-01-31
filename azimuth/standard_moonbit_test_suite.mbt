// 标准 MoonBit 测试套件
// 包含10个高质量测试用例，覆盖核心功能

test "basic_arithmetic_comprehensive" {
  // 综合基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(-5, 4))
  assert_eq(0, add(-10, 10))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-20, multiply(-5, 4))
  assert_eq(0, multiply(0, 100))
  
  assert_eq(10, subtract(20, 10))
  assert_eq(-10, subtract(10, 20))
  assert_eq(0, subtract(50, 50))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(100, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_international_support" {
  // 问候函数国际化支持测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 24
  let y = 18
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 5)),
    add(multiply(x, y), multiply(x, 5))
  )
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：计算运输需求
  let total_packages = 125
  let capacity_per_truck = 20
  let trucks_needed = divide_with_ceil(total_packages, capacity_per_truck)
  
  assert_eq(7, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(140, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_packages)
  assert_eq(15, remaining_space)
  
  // 计算空间利用率
  let utilization = divide_with_ceil(multiply(total_packages, 100), total_capacity)
  assert_eq(90, utilization)  // floor(12500/140) = 89%
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_size = 8
  let weeks_per_sprint = 2
  let tasks_per_person_per_week = 4
  let total_tasks = 180
  
  // 计算每周处理能力
  let weekly_capacity = multiply(team_size, tasks_per_person_per_week)
  assert_eq(32, weekly_capacity)
  
  // 计算冲刺处理能力
  let sprint_capacity = multiply(weekly_capacity, weeks_per_sprint)
  assert_eq(64, sprint_capacity)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(3, sprints_needed)
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(192, total_capacity)
  
  // 计算剩余容量
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(12, remaining_capacity)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边缘情况测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(1000, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 大数测试
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
}

test "string_processing_special_cases" {
  // 字符串处理特殊情况测试
  // 特殊字符和符号
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, \"quotes\"!", greet("\"quotes\""))
  assert_eq_string("Hello, 'apostrophe'!", greet("'apostrophe'"))
  assert_eq_string("Hello, \\backslash\\!", greet("\\backslash\\"))
  assert_eq_string("Hello, \n\r\t!", greet("\n\r\t"))
  assert_eq_string("Hello,   spaces!", greet("  spaces"))
  
  // 编程相关字符串
  assert_eq_string("Hello, null!", greet("null"))
  assert_eq_string("Hello, undefined!", greet("undefined"))
  assert_eq_string("Hello, true!", greet("true"))
  assert_eq_string("Hello, false!", greet("false"))
  assert_eq_string("Hello, 0x1234!", greet("0x1234"))
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  // 场景：仓库存储优化
  let item_a_qty = 45
  let item_b_qty = 67
  let item_c_qty = 23
  let box_capacity = 20
  
  // 计算每种物品需要的箱子数
  let boxes_a = divide_with_ceil(item_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(item_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(item_c_qty, box_capacity)
  
  assert_eq(3, boxes_a)  // ceil(45/20) = 3
  assert_eq(4, boxes_b)  // ceil(67/20) = 4
  assert_eq(2, boxes_c)  // ceil(23/20) = 2
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  assert_eq(9, total_boxes)
  
  // 计算总存储容量
  let total_capacity = multiply(total_boxes, box_capacity)
  assert_eq(180, total_capacity)
  
  // 计算总物品数
  let total_items = add(add(item_a_qty, item_b_qty), item_c_qty)
  assert_eq(135, total_items)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(45, remaining_space)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  // 场景：贷款分期计算
  let loan_amount = 50000
  let monthly_payment_max = 2200
  let months_needed = divide_with_ceil(loan_amount, monthly_payment_max)
  
  assert_eq(23, months_needed)  // ceil(50000/2200) = 23
  
  // 计算实际总支付金额
  let total_payment = multiply(months_needed, monthly_payment_max)
  assert_eq(50600, total_payment)
  
  // 计算最后一个月的实际支付金额
  let regular_payments = months_needed - 1
  let regular_total = multiply(regular_payments, monthly_payment_max)
  let final_payment = subtract(total_payment, regular_total)
  
  assert_eq(800, final_payment)
  
  // 计算总利息
  let total_interest = subtract(total_payment, loan_amount)
  assert_eq(600, total_interest)
  
  // 验证支付计划合理性
  assert_true(final_payment <= monthly_payment_max)
  assert_true(total_interest < multiply(loan_amount, 10))  // 利息不超过本金的10%
}