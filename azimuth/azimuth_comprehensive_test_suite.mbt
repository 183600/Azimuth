// Azimuth 综合测试套件 - 8个高质量测试用例
// 使用标准 MoonBit 测试语法

test "arithmetic_precision_tests" {
  // 算术精度测试
  assert_eq(100000, add(50000, 50000))
  assert_eq(-100000, add(-50000, -50000))
  assert_eq(0, add(100000, -100000))
  
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(-1000, 1000))
  assert_eq(1000000, multiply(-1000, -1000))
}

test "divide_with_ceil_business_logic" {
  // 向上取整除法业务逻辑测试
  // 场景：计算需要的运输车辆
  let total_packages = 127
  let capacity_per_truck = 20
  let trucks_needed = divide_with_ceil(total_packages, capacity_per_truck)
  
  assert_eq(7, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(140, total_capacity)
  
  // 验证剩余运力
  let remaining_capacity = subtract(total_capacity, total_packages)
  assert_eq(13, remaining_capacity)
}

test "greet_function_multilingual_support" {
  // 多语言支持测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "mathematical_commutative_and_associative" {
  // 数学交换律和结合律测试
  let a = 15
  let b = 25
  let c = 35
  
  // 交换律测试
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 结合律测试
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "complex_resource_allocation" {
  // 复杂资源分配测试
  // 场景：项目资源规划
  let project_duration = 12  // 月
  let team_members = 8
  let work_hours_per_day = 8
  let work_days_per_month = 22
  
  // 计算总工时
  let total_work_hours = multiply(
    project_duration,
    multiply(team_members, multiply(work_hours_per_day, work_days_per_month))
  )
  
  assert_eq(16896, total_work_hours)  // 12 * 8 * 8 * 22 = 16896
  
  // 计算需要的冲刺数（每冲刺200小时）
  let sprint_hours = 200
  let sprints_needed = divide_with_ceil(total_work_hours, sprint_hours)
  
  assert_eq(85, sprints_needed)  // ceil(16896/200) = 85
}

test "error_handling_edge_cases" {
  // 错误处理边缘情况测试
  // 除零情况的全面测试
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  
  // 极值运算测试
  assert_eq(0, multiply(999999, 0))
  assert_eq(0, multiply(-999999, 0))
  assert_eq(999999, add(999999, 0))
  assert_eq(-999999, add(-999999, 0))
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
  
  // 计算利息
  let interest = subtract(total_payment, loan_amount)
  assert_eq(600, interest)
}

test "inventory_optimization_problem" {
  // 库存优化问题测试
  // 场景：仓库存储优化
  let item_types = 15
  let items_per_type = 48
  let total_items = multiply(item_types, items_per_type)
  
  let pallet_capacity = 100
  let pallets_needed = divide_with_ceil(total_items, pallet_capacity)
  
  assert_eq(720, total_items)  // 15 * 48 = 720
  assert_eq(8, pallets_needed)  // ceil(720/100) = 8
  
  // 验证存储利用率
  let total_capacity = multiply(pallets_needed, pallet_capacity)
  let utilization_rate = divide_with_ceil(multiply(total_items, 100), total_capacity)
  
  assert_eq(800, total_capacity)  // 8 * 100 = 800
  assert_eq(90, utilization_rate)  // floor(72000/800) = 90%
}