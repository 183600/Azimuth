// Azimuth 增强标准测试用例
// 包含10个高质量的标准 MoonBit 测试用例
// 测试覆盖所有主要功能和边界情况

test "enhanced_arithmetic_precision" {
  // 精密算术运算测试
  // 测试大数运算和精度保持
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(999999, subtract(1000000, 1))
  assert_eq(500000, divide_with_ceil(1000000, 2))
  assert_eq(333334, divide_with_ceil(1000000, 3))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 场景：电商订单处理
  let order_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(order_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 计算总容量和剩余空间
  let total_capacity = multiply(boxes_needed, items_per_box)
  let remaining_space = subtract(total_capacity, order_items)
  
  assert_eq(48, total_capacity)
  assert_eq(1, remaining_space)
  
  // 验证装箱效率
  let efficiency = divide_with_ceil(multiply(order_items, 100), total_capacity)
  assert_eq(98, efficiency)  // 47/48 = 97.9% -> 98%
}

test "mathematical_properties_comprehensive" {
  // 综合数学性质测试
  let a = 17
  let b = 23
  let c = 5
  
  // 交换律测试
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 结合律测试
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律测试
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 零元素性质
  assert_eq(a, add(a, 0))
  assert_eq(0, multiply(a, 0))
  
  // 单位元素性质
  assert_eq(a, multiply(a, 1))
}

test "negative_number_scenarios" {
  // 负数场景全面测试
  // 负数基本运算
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-3, 7))
  assert_eq(-4, add(3, -7))
  
  assert_eq(21, multiply(-3, -7))
  assert_eq(-21, multiply(3, -7))
  assert_eq(-21, multiply(-3, 7))
  
  assert_eq(-10, subtract(-3, 7))
  assert_eq(10, subtract(3, -7))
  assert_eq(4, subtract(-3, -7))
  
  // 负数除法测试
  assert_eq(-2, divide_with_ceil(-10, 5))
  assert_eq(-1, divide_with_ceil(-3, 5))
  assert_eq(2, divide_with_ceil(-10, -5))
  assert_eq(-3, divide_with_ceil(10, -4))
}

test "string_processing_international" {
  // 国际化字符串处理测试
  // 基本英文字符串
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 中文字符串
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 月球!", greet("月球"))
  
  // 特殊字符和Emoji
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  
  // 边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：项目团队资源分配
  let total_tasks = 125
  let team_members = 8
  let working_days = 15
  
  // 计算每人需要完成的任务数
  let tasks_per_person = divide_with_ceil(total_tasks, team_members)
  assert_eq(16, tasks_per_person)
  
  // 验证分配的总任务数
  let allocated_tasks = multiply(tasks_per_person, team_members)
  assert_eq(128, allocated_tasks)
  
  // 计算每天需要完成的任务数
  let daily_tasks = divide_with_ceil(tasks_per_person, working_days)
  assert_eq(2, daily_tasks)
  
  // 验证总工作天数
  let total_days_needed = multiply(daily_tasks, working_days)
  assert_eq(30, total_days_needed)
  
  // 计算任务完成率
  let completion_rate = divide_with_ceil(multiply(total_tasks, 100), allocated_tasks)
  assert_eq(98, completion_rate)  // 125/128 = 97.6% -> 98%
}

test "boundary_conditions_extreme" {
  // 极端边界条件测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 单位值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, add(5, 0))
  
  // 大数边界测试
  assert_eq(10000, multiply(100, 100))
  assert_eq(10001, divide_with_ceil(10001, 1))
  assert_eq(10001, divide_with_ceil(10002, 2))
  
  // 相等数测试
  assert_eq(0, subtract(100, 100))
  assert_eq(1, divide_with_ceil(100, 100))
  assert_eq(0, divide_with_ceil(0, 100))
}

test "financial_calculation_accuracy" {
  // 金融计算精度测试
  // 场景：贷款分期计算
  let loan_amount = 150000
  let monthly_payment = 4500
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(34, months_needed)  // ceil(150000/4500) = 34
  
  // 计算总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(153000, total_payment)
  
  // 计算额外利息
  let extra_interest = subtract(total_payment, loan_amount)
  assert_eq(3000, extra_interest)
  
  // 计算实际月供（最后一期）
  let final_payment = subtract(total_payment, multiply(months_needed - 1, monthly_payment))
  assert_eq(1500, final_payment)
  
  // 验证支付计划准确性
  let regular_payments = months_needed - 1
  let regular_total = multiply(regular_payments, monthly_payment)
  let verification = add(regular_total, final_payment)
  assert_eq(total_payment, verification)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  
  // 极值运算测试
  let large_num = 1000000
  let small_num = 1
  
  assert_eq(large_num, add(large_num, 0))
  assert_eq(0, multiply(large_num, 0))
  assert_eq(large_num, multiply(large_num, 1))
  assert_eq(large_num, divide_with_ceil(large_num, 1))
  assert_eq(1, divide_with_ceil(large_num, large_num))
  
  // 负数极值测试
  let neg_large = -1000000
  assert_eq(neg_large, add(neg_large, 0))
  assert_eq(0, multiply(neg_large, 0))
  assert_eq(neg_large, multiply(neg_large, 1))
  assert_eq(neg_large, divide_with_ceil(neg_large, 1))
}

test "real_world_logistics_simulation" {
  // 真实世界物流模拟测试
  // 场景：仓库库存管理和配送
  let total_products = 587
  let box_capacity = 24
  let truck_capacity = 120
  let stores = 7
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(25, boxes_needed)
  
  // 计算总箱子容量和剩余空间
  let total_box_capacity = multiply(boxes_needed, box_capacity)
  let box_remaining = subtract(total_box_capacity, total_products)
  assert_eq(600, total_box_capacity)
  assert_eq(13, box_remaining)
  
  // 计算需要的卡车数量
  let trucks_needed = divide_with_ceil(boxes_needed, divide_with_ceil(truck_capacity, box_capacity))
  let boxes_per_truck = divide_with_ceil(truck_capacity, box_capacity)
  assert_eq(5, boxes_per_truck)  // 每辆卡车可装5箱
  assert_eq(5, trucks_needed)    // 需要5辆卡车
  
  // 计算每家商店分配
  let products_per_store = divide_with_ceil(total_products, stores)
  assert_eq(84, products_per_store)
  
  // 验证分配结果
  let total_allocated = multiply(products_per_store, stores)
  let allocation_surplus = subtract(total_allocated, total_products)
  assert_eq(588, total_allocated)
  assert_eq(1, allocation_surplus)
  
  // 计算物流效率
  let space_efficiency = divide_with_ceil(multiply(total_products, 100), total_box_capacity)
  let distribution_efficiency = divide_with_ceil(multiply(total_products, 100), total_allocated)
  assert_eq(98, space_efficiency)      // 587/600 = 97.8% -> 98%
  assert_eq(100, distribution_efficiency)  // 587/588 = 99.8% -> 100%
}