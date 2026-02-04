// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_comprehensive" {
  // 综合基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-3, add(-10, 7))
  assert_eq(0, add(0, 0))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-70, multiply(-7, 10))
  assert_eq(0, multiply(0, 100))
  
  assert_eq(-1, subtract(5, 6))
  assert_eq(10, subtract(0, -10))
  assert_eq(0, subtract(25, 25))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(15, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(
    multiply(a, add(b, 3)),
    add(multiply(a, b), multiply(a, 3))
  )
  
  // 恒等式
  assert_eq(a, add(a, 0))
  assert_eq(a, multiply(a, 1))
  assert_eq(0, multiply(a, 0))
}

test "business_logistics_calculation" {
  // 物流计算业务场景测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
  
  // 验证空间利用率
  let utilization_percent = divide_with_ceil(
    multiply(total_products, 100), 
    total_capacity
  )
  assert_eq(94, utilization_percent)  // floor(15700/168) = 93%
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目成本估算
  let team_members = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 15
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, project_days))
  assert_eq(960, total_hours)
  
  // 计算总成本
  let total_cost = multiply(total_hours, hourly_rate)
  assert_eq(72000, total_cost)
  
  // 计算需要的支付周期（每周期支付不超过20000）
  let payment_limit = 20000
  let payment_periods = divide_with_ceil(total_cost, payment_limit)
  assert_eq(4, payment_periods)
  
  // 计算最后一期的支付金额
  let full_periods = subtract(payment_periods, 1)
  let full_payments = multiply(full_periods, payment_limit)
  let final_payment = subtract(total_cost, full_payments)
  assert_eq(12000, final_payment)
}

test "boundary_conditions_and_edge_cases" {
  // 边界条件和边缘情况测试
  // 极值测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 特殊字符串测试
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(9999, 0))
  assert_eq(0, divide_with_ceil(-9999, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算稳定性测试
  let max_safe = 1000000
  let min_safe = -1000000
  
  assert_eq(0, multiply(max_safe, 0))
  assert_eq(0, multiply(min_safe, 0))
  assert_eq(max_safe, add(max_safe, 0))
  assert_eq(min_safe, add(min_safe, 0))
  
  // 大数精度测试
  assert_eq(100001, divide_with_ceil(200001, 2))
  assert_eq(66667, divide_with_ceil(200000, 3))
  assert_eq(33334, divide_with_ceil(100000, 3))
}