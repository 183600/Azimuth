// Azimuth 高级测试用例
// 包含10个标准 MoonBit 测试用例，覆盖核心功能和边界情况

test "arithmetic_operations_comprehensive" {
  // 综合算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-3, add(-10, 7))
  assert_eq(0, add(5, -5))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-70, multiply(-7, 10))
  assert_eq(70, multiply(-7, -10))
  
  assert_eq(-1, subtract(5, 6))
  assert_eq(11, subtract(6, -5))
  assert_eq(0, subtract(7, 7))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
  
  // 结合律验证
  assert_eq(
    multiply(multiply(x, y), 2),
    multiply(x, multiply(y, 2))
  )
}

test "business_logistics_calculation" {
  // 物流业务计算测试
  let total_packages = 47
  let packages_per_box = 12
  let boxes_needed = divide_with_ceil(total_packages, packages_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, packages_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_packages)
  assert_eq(1, remaining_space)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  let loan_amount = 10000
  let monthly_payment = 750
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(14, months_needed)
  
  // 验证总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(10500, total_payment)
  
  // 验证最后一个月的实际支付金额
  let final_payment = subtract(total_payment, multiply(months_needed - 1, monthly_payment))
  assert_eq(500, final_payment)
}

test "error_handling_boundary_conditions" {
  // 错误处理边界条件测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(1000, 0))
  assert_eq(0, divide_with_ceil(-1000, 0))
  
  // 极值运算测试
  assert_eq(0, multiply(1000, 0))
  assert_eq(0, multiply(-1000, 0))
  assert_eq(1000, add(1000, 0))
  assert_eq(-1000, add(-1000, 0))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 15
  let multiplier = 4
  let divisor = 7
  let addition = 9
  
  // 计算流程：((base * multiplier) + addition) / divisor
  let step1 = multiply(base_value, multiplier)
  let step2 = add(step1, addition)
  let result = divide_with_ceil(step2, divisor)
  
  assert_eq(12, result)  // ceil((15*4 + 9)/7) = ceil(69/7) = 10
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 89
  let teams = 4
  let min_resources_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(23, min_resources_per_team)
  
  // 验证分配结果
  let allocated_total = multiply(min_resources_per_team, teams)
  assert_eq(92, allocated_total)
  
  // 验证资源利用率
  let utilization = divide_with_ceil(multiply(total_resources, 100), allocated_total)
  assert_eq(97, utilization)  // floor(8900/92) = 96%
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let participants = 125
  let tables_available = 15
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(16, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(128, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(3, remaining_seats)
}