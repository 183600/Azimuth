// Azimuth 增强单元测试 - 8个高质量测试用例
// 使用标准 MoonBit 测试语法

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(0, 5))
  @azimuth.assert_eq(15, @azimuth.subtract(10, -5))
  @azimuth.assert_eq(-15, @azimuth.subtract(-10, 5))
  @azimuth.assert_eq(0, @azimuth.subtract(0, 0))
}

test "divide_with_ceil_negative_divisor" {
  // 测试负除数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -4))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(7, -4))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(3, -4))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -4))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-7, -4))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -4))
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 2
  
  // a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // (a + b) * c = a * c + b * c
  let left_side2 = @azimuth.multiply(@azimuth.add(a, b), c)
  let right_side2 = @azimuth.add(@azimuth.multiply(a, c), @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_side2, right_side2)
}

test "resource_planning_scenario" {
  // 资源规划场景测试
  // 场景：项目团队资源分配
  let total_tasks = 67
  let team_members = 5
  let working_days = 14
  
  // 计算每人需要完成的任务数
  let tasks_per_person = @azimuth.divide_with_ceil(total_tasks, team_members)
  @azimuth.assert_eq(14, tasks_per_person)  // ceil(67/5) = 14
  
  // 计算每人每天需要完成的任务数
  let daily_tasks_per_person = @azimuth.divide_with_ceil(tasks_per_person, working_days)
  @azimuth.assert_eq(1, daily_tasks_per_person)  // ceil(14/14) = 1
  
  // 验证总任务容量
  let total_capacity = @azimuth.multiply(tasks_per_person, team_members)
  @azimuth.assert_eq(70, total_capacity)
  
  // 验证剩余任务容量
  let spare_capacity = @azimuth.subtract(total_capacity, total_tasks)
  @azimuth.assert_eq(3, spare_capacity)
}

test "string_boundary_conditions" {
  // 字符串边界条件测试
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !@#$!", @azimuth.greet("!@#$"))
  @azimuth.assert_eq_string("Hello, mixedCASE123!", @azimuth.greet("mixedCASE123"))
}

test "complex_financial_calculation" {
  // 复杂金融计算测试
  // 场景：贷款分期计算
  let loan_amount = 15000
  let monthly_payment_max = 800
  let months_needed = @azimuth.divide_with_ceil(loan_amount, monthly_payment_max)
  @azimuth.assert_eq(19, months_needed)  // ceil(15000/800) = 19
  
  // 计算实际月供（均摊）
  let actual_monthly_payment = @azimuth.divide_with_ceil(loan_amount, months_needed)
  @azimuth.assert_eq(791, actual_monthly_payment)  // ceil(15000/19) = 791
  
  // 验证总支付金额
  let total_payment = @azimuth.multiply(actual_monthly_payment, months_needed)
  @azimuth.assert_eq(15029, total_payment)
  
  // 计算最后一个月的实际支付金额
  let final_payment = @azimuth.subtract(
    total_payment, 
    @azimuth.multiply(actual_monthly_payment, @azimuth.subtract(months_needed, 1))
  )
  @azimuth.assert_eq(771, final_payment)
}

test "extreme_value_operations" {
  // 极值运算测试
  // 大数运算（调整到安全范围内）
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1000000, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1000000, 1000000))
  
  // 负数极值
  @azimuth.assert_eq(-2000000, @azimuth.add(-1000000, -1000000))
  @azimuth.assert_eq(-1000000, @azimuth.divide_with_ceil(-1000000, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1000000, 1000000))
}

test "algorithm_efficiency_comparison" {
  // 算法效率比较测试
  // 模拟不同算法的时间复杂度
  let input_size = 1000
  
  // O(1) 算法 - 常数时间
  let constant_time = 5
  
  // O(log n) 算法 - 对数时间（简化模型）
  let log_time = @azimuth.divide_with_ceil(input_size, 100)
  
  // O(n) 算法 - 线性时间
  let linear_time = @azimuth.multiply(input_size, 2)
  
  // O(n²) 算法 - 平方时间（简化模型）
  let quadratic_time = @azimuth.multiply(input_size, @azimuth.divide_with_ceil(input_size, 100))
  
  // 验证复杂度关系
  @azimuth.assert_eq(5, constant_time)
  @azimuth.assert_eq(10, log_time)  // ceil(1000/100)
  @azimuth.assert_eq(2000, linear_time)
  @azimuth.assert_eq(10000, quadratic_time)  // 1000 * ceil(1000/100)
  
  // 验证效率差异
  @azimuth.assert_true(linear_time > log_time)
  @azimuth.assert_true(quadratic_time > linear_time)
}