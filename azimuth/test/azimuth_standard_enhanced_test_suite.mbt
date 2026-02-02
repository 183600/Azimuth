// Azimuth 标准增强测试套件
// 包含10个高质量 MoonBit 测试用例

test "add_function_comprehensive" {
  // 加法函数综合测试
  @azimuth.assert_eq(10, @azimuth.add(6, 4))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-10, @azimuth.add(-6, -4))
  @azimuth.assert_eq(2, @azimuth.add(-6, 8))
  @azimuth.assert_eq(100000, @azimuth.add(50000, 50000))
  @azimuth.assert_eq(-2, @azimuth.add(-10, 8))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 999))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 999999))
  @azimuth.assert_eq(-999999, @azimuth.multiply(-1, 999999))
}

test "divide_with_ceil_precision" {
  // 向上取整除法精度测试
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(21, 4))
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(24, 4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-21, 4))
}

test "subtract_function_variations" {
  // 减法函数变体测试
  @azimuth.assert_eq(6, @azimuth.subtract(10, 4))
  @azimuth.assert_eq(-6, @azimuth.subtract(4, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
  @azimuth.assert_eq(20, @azimuth.subtract(10, -10))
  @azimuth.assert_eq(-20, @azimuth.subtract(-10, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(-10, -10))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法恒等式
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(0, a))
  
  // 乘法恒等式
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(a, @azimuth.multiply(1, a))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试：项目资源规划
  let team_members = 7
  let tasks_per_member = 9
  let total_tasks = @azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  
  @azimuth.assert_eq(63, total_tasks)  // 7 * 9 = 63
  @azimuth.assert_eq(4, sprints_needed)  // ceil(63/20) = 4
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(sprints_needed, sprint_capacity)
  @azimuth.assert_eq(80, total_capacity)
  
  // 验证剩余容量
  let remaining_capacity = @azimuth.subtract(total_capacity, total_tasks)
  @azimuth.assert_eq(17, remaining_capacity)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值运算
  let large_num = 100000
  @azimuth.assert_eq(200000, @azimuth.add(large_num, large_num))
  @azimuth.assert_eq(10000000000, @azimuth.multiply(large_num, large_num))
  
  // 边界减法
  @azimuth.assert_eq(100000, @azimuth.subtract(large_num, 0))
  @azimuth.assert_eq(-100000, @azimuth.subtract(0, large_num))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试：库存管理
  let total_products = 158
  let box_capacity = 25
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  
  @azimuth.assert_eq(7, boxes_needed)
  
  // 计算总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(175, total_capacity)
  
  // 计算剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(17, remaining_space)
  
  // 计算空间利用率
  let utilization_percent = @azimuth.divide_with_ceil(@azimuth.multiply(total_products, 100), total_capacity)
  @azimuth.assert_eq(91, utilization_percent)  // floor(15800/175) = 90%
}

test "real_world_financial_calculation" {
  // 真实世界金融计算测试：贷款分期
  let loan_amount = 15000
  let monthly_payment = 1200
  let months_needed = @azimuth.divide_with_ceil(loan_amount, monthly_payment)
  
  @azimuth.assert_eq(13, months_needed)  // ceil(15000/1200) = 13
  
  // 计算总支付金额
  let total_payment = @azimuth.multiply(months_needed, monthly_payment)
  @azimuth.assert_eq(15600, total_payment)
  
  // 计算额外利息
  let extra_interest = @azimuth.subtract(total_payment, loan_amount)
  @azimuth.assert_eq(600, extra_interest)
  
  // 计算月均实际支付
  let average_monthly_payment = @azimuth.divide_with_ceil(total_payment, months_needed)
  @azimuth.assert_eq(1200, average_monthly_payment)
}