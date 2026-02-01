// Azimuth 项目综合新测试用例
// 使用标准 MoonBit 测试语法，包含高质量测试用例

test "basic_arithmetic_comprehensive" {
  // 基础算术综合测试
  assert_eq(10, add(3, 7))
  assert_eq(-4, add(-10, 6))
  assert_eq(0, add(5, -5))
  
  assert_eq(21, multiply(3, 7))
  assert_eq(-30, multiply(-5, 6))
  assert_eq(0, multiply(8, 0))
  
  assert_eq(5, subtract(12, 7))
  assert_eq(-15, subtract(8, 23))
  assert_eq(0, subtract(10, 10))
}

test "divide_with_ceil_business_logic" {
  // 向上取整除法业务逻辑测试
  // 场景：计算需要的运输车辆
  let total_packages = 47
  let capacity_per_vehicle = 10
  let vehicles_needed = divide_with_ceil(total_packages, capacity_per_vehicle)
  
  assert_eq(5, vehicles_needed)
  
  // 验证总运输能力
  let total_capacity = multiply(vehicles_needed, capacity_per_vehicle)
  assert_eq(50, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_packages)
  assert_eq(3, remaining_space)
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, José!", greet("José"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 15
  let b = 8
  let c = 3
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(
    multiply(a, add(b, c)),
    add(multiply(a, b), multiply(a, c))
  )
  
  // 结合律验证
  assert_eq(
    add(add(a, b), c),
    add(a, add(b, c))
  )
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源规划
  let team_members = 7
  let hours_per_day = 8
  let working_days = 15
  let tasks_per_hour = 2
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, working_days))
  
  // 计算总任务数
  let total_tasks = multiply(total_hours, tasks_per_hour)
  
  // 计算需要的冲刺数（每个冲刺最多100个任务）
  let sprint_capacity = 100
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(840, total_hours)  // 7 * 8 * 15 = 840
  assert_eq(1680, total_tasks)  // 840 * 2 = 1680
  assert_eq(17, sprints_needed)  // ceil(1680/100) = 17
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 单位值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(10, multiply(10, 1))
  assert_eq(10, divide_with_ceil(10, 1))
  
  // 负数边界测试
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  // 场景：贷款分期计算
  let loan_amount = 50000
  let monthly_payment = 2200
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(23, months_needed)  // ceil(50000/2200) = 23
  
  // 计算总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(50600, total_payment)
  
  // 计算最后一个月的实际支付金额
  let regular_payments = subtract(months_needed, 1)
  let regular_total = multiply(regular_payments, monthly_payment)
  let final_payment = subtract(loan_amount, regular_total)
  
  assert_eq(600, final_payment)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 大数运算测试
  let large_num = 10000
  assert_eq(20000, add(large_num, large_num))
  assert_eq(100000000, multiply(large_num, large_num))
  assert_eq(1, divide_with_ceil(large_num, large_num))
  
  // 极值减法测试
  assert_eq(0, subtract(large_num, large_num))
  assert_eq(200000, subtract(large_num, -large_num))
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  let product_types = 25
  let min_stock_per_type = 50
  let current_total_stock = 1800
  let storage_capacity_per_warehouse = 500
  
  // 计算最小总库存需求
  let min_total_required = multiply(product_types, min_stock_per_type)
  
  // 计算需要的仓库数量
  let warehouses_needed = divide_with_ceil(min_total_required, storage_capacity_per_warehouse)
  
  // 计算需要补充的库存
  let additional_stock_needed = subtract(min_total_required, current_total_stock)
  
  assert_eq(1250, min_total_required)  // 25 * 50 = 1250
  assert_eq(3, warehouses_needed)  // ceil(1250/500) = 3
  assert_eq(-550, additional_stock_needed)  // 1250 - 1800 = -550 (库存充足)
}