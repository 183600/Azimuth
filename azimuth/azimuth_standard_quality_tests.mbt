// Azimuth 项目标准质量测试用例
// 包含10个高质量的 MoonBit 测试用例，覆盖核心功能和实际应用场景

test "basic_arithmetic_operations_comprehensive" {
  // 全面的基本算术运算测试
  add(15, 27) = 42
  add(-8, 3) = -5
  add(0, 100) = 100
  
  multiply(6, 7) = 42
  multiply(-4, 5) = -20
  multiply(0, 999) = 0
  
  subtract(50, 23) = 27
  subtract(10, 30) = -20
  subtract(0, 0) = 0
}

test "divide_with_ceil_edge_cases_and_boundaries" {
  // 向上取整除法的边界情况和边缘条件测试
  divide_with_ceil(1, 1) = 1
  divide_with_ceil(0, 1) = 0
  divide_with_ceil(1, 2) = 1
  divide_with_ceil(99, 100) = 1
  divide_with_ceil(100, 100) = 1
  divide_with_ceil(101, 100) = 2
  
  // 除零错误处理
  divide_with_ceil(50, 0) = 0
  divide_with_ceil(0, 0) = 0
  divide_with_ceil(-50, 0) = 0
}

test "negative_numbers_comprehensive_handling" {
  // 负数的全面处理测试
  add(-10, -15) = -25
  add(-8, 3) = -5
  add(12, -7) = 5
  
  multiply(-3, 4) = -12
  multiply(-3, -4) = 12
  multiply(5, -2) = -10
  
  divide_with_ceil(-10, 3) = -3
  divide_with_ceil(10, -3) = -3
  divide_with_ceil(-10, -3) = 4
}

test "string_processing_and_unicode_support" {
  // 字符串处理和Unicode支持测试
  greet("World") = "Hello, World!"
  greet("") = "Hello, !"
  greet("123") = "Hello, 123!"
  greet("世界") = "Hello, 世界!"
  greet("🚀") = "Hello, 🚀!"
  greet("café") = "Hello, café!"
  greet("@#$%") = "Hello, @#$%!"
}

test "mathematical_properties_and_laws" {
  // 数学性质和定律验证测试
  let x = 17
  let y = 23
  
  // 加法交换律
  add(x, y) = add(y, x)
  
  // 乘法交换律
  multiply(x, y) = multiply(y, x)
  
  // 加法恒等式
  add(x, 0) = x
  add(0, x) = x
  
  // 乘法恒等式
  multiply(x, 1) = x
  multiply(1, x) = x
  multiply(x, 0) = 0
  
  // 减法恒等式
  subtract(x, 0) = x
  subtract(x, x) = 0
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 嵌套函数调用
  add(multiply(3, 4), multiply(5, 6)) = 42
  subtract(multiply(10, 5), divide_with_ceil(47, 3)) = 42
  
  // 多步骤计算链
  let step1 = add(5, 3) = 8
  let step2 = multiply(step1, 4) = 32
  let step3 = divide_with_ceil(step2, 7) = 5
  let step4 = subtract(step3, 2) = 3
}

test "business_logic_inventory_management" {
  // 业务逻辑：库存管理场景测试
  // 场景：计算仓库存储需求
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  boxes_needed = 7
  
  // 验证总存储容量
  let total_capacity = multiply(boxes_needed, box_capacity) = 168
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products) = 11
  
  // 验证空间利用率
  let utilization_percent = divide_with_ceil(multiply(total_products, 100), total_capacity) = 93
}

test "project_management_resource_allocation" {
  // 项目管理：资源分配场景测试
  // 场景：计算项目完成时间和资源需求
  let total_tasks = 125
  let team_size = 8
  let tasks_per_person_per_day = 2
  
  // 计算每日处理能力
  let daily_capacity = multiply(team_size, tasks_per_person_per_day) = 16
  
  // 计算需要的天数
  let days_needed = divide_with_ceil(total_tasks, daily_capacity) = 8
  
  // 计算总处理能力
  let total_capacity = multiply(days_needed, daily_capacity) = 128
  
  // 计算剩余处理能力
  let spare_capacity = subtract(total_capacity, total_tasks) = 3
}

test "financial_calculation_loan_amortization" {
  // 金融计算：贷款摊还场景测试
  // 场景：计算贷款分期付款
  let loan_amount = 50000
  let monthly_payment_max = 1800
  
  // 计算需要的月数
  let months_needed = divide_with_ceil(loan_amount, monthly_payment_max) = 28
  
  // 计算实际总支付金额
  let total_payment = multiply(months_needed, monthly_payment_max) = 50400
  
  // 计算最后一个月的实际支付金额
  let regular_months = subtract(months_needed, 1) = 27
  let regular_payments = multiply(regular_months, monthly_payment_max) = 48600
  let final_payment = subtract(total_payment, regular_payments) = 1800
  
  // 验证总支付金额正确
  add(regular_payments, final_payment) = total_payment
}

test "error_handling_and_boundary_validation" {
  // 错误处理和边界验证测试
  // 极值测试
  add(2147483647, 0) = 2147483647
  add(-2147483648, 0) = -2147483648
  multiply(46340, 46340) = 2147395956
  multiply(-46340, 46340) = -2147395956
  
  // 边界除法测试
  divide_with_ceil(2147483647, 1) = 2147483647
  divide_with_ceil(2147483647, 2147483647) = 1
  divide_with_ceil(1, 2147483647) = 1
  
  // 错误恢复测试
  divide_with_ceil(2147483647, 0) = 0
  divide_with_ceil(-2147483648, 0) = 0
  divide_with_ceil(0, 0) = 0
}