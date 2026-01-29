// Azimuth 项目标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，覆盖核心功能和边界情况

test "mathematical_identity_comprehensive" {
  // 综合数学恒等式测试
  let x = 42
  let y = 17
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  assert_eq(add(x, y), add(y, x))  // 交换律
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  assert_eq(multiply(x, y), multiply(y, x))  // 交换律
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 零值边界
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(12345, 0))
  assert_eq(0, divide_with_ceil(0, 12345))
  assert_eq(0, divide_with_ceil(12345, 0))
  
  // 单位边界
  assert_eq(1, add(1, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
  
  // 负数边界
  assert_eq(-1, multiply(-1, 1))
  assert_eq(1, multiply(-1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "real_world_shipping_calculation" {
  // 真实世界运输计算场景
  let packages = [127, 85, 43, 256, 91]  // 5个地点的包裹数量
  let truck_capacity = 50
  
  // 计算每个地点需要的卡车数量
  let trucks_1 = divide_with_ceil(127, 50)  // ceil(127/50) = 3
  let trucks_2 = divide_with_ceil(85, 50)   // ceil(85/50) = 2
  let trucks_3 = divide_with_ceil(43, 50)   // ceil(43/50) = 1
  let trucks_4 = divide_with_ceil(256, 50)  // ceil(256/50) = 6
  let trucks_5 = divide_with_ceil(91, 50)   // ceil(91/50) = 2
  
  // 验证计算结果
  assert_eq(3, trucks_1)
  assert_eq(2, trucks_2)
  assert_eq(1, trucks_3)
  assert_eq(6, trucks_4)
  assert_eq(2, trucks_5)
  
  // 计算总卡车数量和总容量
  let total_trucks = add(add(add(add(trucks_1, trucks_2), trucks_3), trucks_4), trucks_5)
  let total_capacity = multiply(total_trucks, truck_capacity)
  
  assert_eq(14, total_trucks)
  assert_eq(700, total_capacity)
  
  // 计算总包裹数
  let total_packages = add(add(add(add(127, 85), 43), 256), 91)
  assert_eq(602, total_packages)
  
  // 验证容量充足
  assert_true(total_capacity >= total_packages)
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况测试
  // 特殊字符和编码测试
  assert_eq_string("Hello, null!", greet("null"))
  assert_eq_string("Hello, undefined!", greet("undefined"))
  assert_eq_string("Hello, 0x123!", greet("0x123"))
  assert_eq_string("Hello, NaN!", greet("NaN"))
  assert_eq_string("Hello, Infinity!", greet("Infinity"))
  
  // 空白字符测试
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, \t\n\r!", greet("\t\n\r"))
  assert_eq_string("Hello,  leading!", greet(" leading"))
  assert_eq_string("Hello, trailing !", greet("trailing "))
  
  // 特殊符号测试
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, []{}<>!", greet("[]{}<>"))
  assert_eq_string("Hello, \\|/~`!", greet("\\|/~`"))
}

test "complex_financial_amortization" {
  // 复杂金融分期计算测试
  let loan_amount = 250000
  let annual_rate = 6  // 6%
  let loan_term_years = 30
  
  // 计算总月数
  let total_months = multiply(loan_term_years, 12)
  assert_eq(360, total_months)
  
  // 简化的月供计算（使用向上取整确保覆盖）
  let monthly_rate = divide_with_ceil(annual_rate, 12)  // 月利率 0.5%
  let monthly_payment = divide_with_ceil(
    multiply(loan_amount, add(100, monthly_rate)),
    multiply(total_months, 100)
  )
  
  // 验证月供合理性
  assert_eq(735, monthly_payment)
  
  // 计算总支付金额
  let total_payment = multiply(monthly_payment, total_months)
  assert_eq(264600, total_payment)
  
  // 计算总利息
  let total_interest = subtract(total_payment, loan_amount)
  assert_eq(14600, total_interest)
  
  // 验证利息不超过本金的50%
  assert_true(total_interest < divide_with_ceil(loan_amount, 2))
}

test "data_structure_simulation" {
  // 数据结构操作模拟测试
  // 模拟栈操作序列
  let stack_capacity = 10
  let stack_size = 0
  
  // 模拟 push 操作
  let size_after_pushes = add(stack_size, 5)
  assert_eq(5, size_after_pushes)
  
  // 模拟 pop 操作
  let size_after_pops = subtract(size_after_pushes, 3)
  assert_eq(2, size_after_pops)
  
  // 检查栈是否为空
  let is_empty = size_after_pops == 0
  assert_false(is_empty)
  
  // 检查栈是否已满
  let is_full = size_after_pops >= stack_capacity
  assert_false(is_full)
  
  // 模拟队列操作
  let queue_capacity = 15
  let queue_head = 3
  let queue_tail = 12
  let queue_size = subtract(queue_tail, queue_head)
  assert_eq(9, queue_size)
  
  // 检查队列剩余空间
  let remaining_space = subtract(queue_capacity, queue_size)
  assert_eq(6, remaining_space)
}

test "algorithm_complexity_validation" {
  // 算法复杂度验证测试
  let input_sizes = [100, 1000, 10000]
  
  // 模拟 O(n) 线性算法
  let linear_100 = multiply(100, 3)
  let linear_1000 = multiply(1000, 3)
  let linear_10000 = multiply(10000, 3)
  
  assert_eq(300, linear_100)
  assert_eq(3000, linear_1000)
  assert_eq(30000, linear_10000)
  
  // 模拟 O(log n) 对数算法（简化为除法）
  let log_100 = divide_with_ceil(100, 10)
  let log_1000 = divide_with_ceil(1000, 10)
  let log_10000 = divide_with_ceil(10000, 10)
  
  assert_eq(10, log_100)
  assert_eq(100, log_1000)
  assert_eq(1000, log_10000)
  
  // 验证复杂度比例
  let linear_ratio = divide_with_ceil(linear_1000, linear_100)
  let log_ratio = divide_with_ceil(log_1000, log_100)
  
  assert_eq(10, linear_ratio)  // 线性算法增长10倍
  assert_eq(10, log_ratio)     // 对数算法增长10倍
}

test "error_resilience_and_recovery" {
  // 错误恢复和韧性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(999999, 0))
  assert_eq(0, divide_with_ceil(-999999, 0))
  
  // 极值运算测试
  let max_safe_int = 1000000
  let min_safe_int = -1000000
  
  // 大数运算
  assert_eq(2000000, add(max_safe_int, max_safe_int))
  assert_eq(0, add(max_safe_int, min_safe_int))
  assert_eq(-2000000, add(min_safe_int, min_safe_int))
  
  // 极值乘法
  assert_eq(1000000000000, multiply(max_safe_int, max_safe_int))
  assert_eq(-1000000000000, multiply(max_safe_int, min_safe_int))
  assert_eq(1000000000000, multiply(min_safe_int, min_safe_int))
  
  // 极值除法
  assert_eq(1, divide_with_ceil(max_safe_int, max_safe_int))
  assert_eq(-1, divide_with_ceil(min_safe_int, max_safe_int))
  assert_eq(1, divide_with_ceil(min_safe_int, min_safe_int))
  
  // 验证除零安全性
  assert_eq(0, divide_with_ceil(max_safe_int, 0))
  assert_eq(0, divide_with_ceil(min_safe_int, 0))
}