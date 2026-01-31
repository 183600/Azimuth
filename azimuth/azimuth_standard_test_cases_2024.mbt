// Azimuth 标准测试用例 2024
// 包含10个高质量的 MoonBit 标准测试用例

test "basic_arithmetic_comprehensive" {
  // 基础算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(3, -4))
  assert_eq(0, add(5, -5))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-24, multiply(-3, 8))
  assert_eq(24, multiply(-3, -8))
  
  assert_eq(3, divide_with_ceil(25, 10))
  assert_eq(-2, divide_with_ceil(-15, 10))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律和结合律
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法交换律和结合律
  assert_eq(multiply(a, b), multiply(b, a))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "boundary_conditions_extreme" {
  // 边界条件和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(0, divide_with_ceil(0, 12345))
  
  // 单位元测试
  assert_eq(12345, add(12345, 0))
  assert_eq(12345, multiply(12345, 1))
  assert_eq(1, divide_with_ceil(12345, 12345))
  
  // 负数边界测试
  assert_eq(-10, add(-5, -5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-1, divide_with_ceil(-5, 5))
}

test "string_processing_international" {
  // 字符串处理国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "business_logistics_calculation" {
  // 物流业务计算测试
  // 场景：仓库货物装箱
  let product_a_qty = 47
  let product_b_qty = 63
  let product_c_qty = 29
  let box_capacity = 15
  
  // 计算各产品需要的箱子数
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity)
  
  assert_eq(4, boxes_a)  // ceil(47/15) = 4
  assert_eq(5, boxes_b)  // ceil(63/15) = 5
  assert_eq(2, boxes_c)  // ceil(29/15) = 2
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = multiply(total_boxes, box_capacity)
  
  assert_eq(11, total_boxes)
  assert_eq(165, total_capacity)
  
  // 计算总货物数
  let total_products = add(add(product_a_qty, product_b_qty), product_c_qty)
  assert_eq(139, total_products)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  // 场景：贷款分期计算
  let loan_amount = 50000
  let monthly_payment_max = 2200
  
  // 计算最少还款期数
  let min_months = divide_with_ceil(loan_amount, monthly_payment_max)
  assert_eq(23, min_months)  // ceil(50000/2200) = 23
  
  // 计算实际总还款额
  let actual_total = multiply(min_months, monthly_payment_max)
  assert_eq(50600, actual_total)
  
  // 计算最后一期还款金额
  let previous_payments = multiply(min_months - 1, monthly_payment_max)
  let final_payment = subtract(loan_amount, previous_payments)
  assert_eq(200, final_payment)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  
  // 极值运算测试
  let large_num = 1000000
  assert_eq(0, multiply(large_num, 0))
  assert_eq(large_num, add(large_num, 0))
  assert_eq(1, divide_with_ceil(large_num, large_num))
  
  // 负数除零测试
  assert_eq(0, divide_with_ceil(-large_num, 0))
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let input_size = 1000
  
  // 模拟O(n)算法
  let linear_ops = multiply(input_size, 3)
  assert_eq(3000, linear_ops)
  
  // 模拟O(log n)算法（简化为除法运算）
  let log_ops = divide_with_ceil(input_size, 10)
  assert_eq(100, log_ops)
  
  // 模拟O(n²)算法
  let quadratic_ops = multiply(input_size, divide_with_ceil(input_size, 100))
  assert_eq(10000, quadratic_ops)
  
  // 效率比较
  let efficiency_ratio = divide_with_ceil(linear_ops, log_ops)
  assert_eq(30, efficiency_ratio)
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  // 场景：生产资源分配
  let total_budget = 25000
  let project_a_cost = 8000
  let project_b_cost = 12000
  let project_c_cost = 6000
  
  // 计算各项目需要的资金批次（每批最多5000）
  let batch_limit = 5000
  let batches_a = divide_with_ceil(project_a_cost, batch_limit)
  let batches_b = divide_with_ceil(project_b_cost, batch_limit)
  let batches_c = divide_with_ceil(project_c_cost, batch_limit)
  
  assert_eq(2, batches_a)  // ceil(8000/5000) = 2
  assert_eq(3, batches_b)  // ceil(12000/5000) = 3
  assert_eq(2, batches_c)  // ceil(6000/5000) = 2
  
  // 计算总批次
  let total_batches = add(add(batches_a, batches_b), batches_c)
  assert_eq(7, total_batches)
  
  // 验证预算充足性
  let total_cost = add(add(project_a_cost, project_b_cost), project_c_cost)
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(-1000, remaining_budget)  // 预算不足
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  // 场景：会议活动安排
  let attendees = 256
  let tables_available = 20
  let chairs_per_table = 12
  let cost_per_chair = 45
  let budget_limit = 15000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(22, tables_needed)  // ceil(256/12) = 22
  
  // 检查桌子是否足够
  let additional_tables_needed = subtract(tables_needed, tables_available)
  assert_eq(2, additional_tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(264, total_seats)
  
  // 计算剩余座位
  let empty_seats = subtract(total_seats, attendees)
  assert_eq(8, empty_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(11880, total_cost)
  
  // 验证预算
  assert_true(total_cost <= budget_limit)
  
  // 计算剩余预算
  let remaining_budget = subtract(budget_limit, total_cost)
  assert_eq(3120, remaining_budget)
}