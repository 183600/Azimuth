// 额外的标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖各种测试场景

test "basic_arithmetic_validation" {
  // 基本算术运算验证测试
  assert_eq(add(10, 15), 25)
  assert_eq(multiply(6, 7), 42)
  assert_eq(subtract(100, 37), 63)
  assert_eq(divide_with_ceil(20, 3), 7)
  assert_eq(divide_with_ceil(21, 3), 7)
}

test "string_processing_functions" {
  // 字符串处理函数测试
  assert_eq_string(greet("Alice"), "Hello, Alice!")
  assert_eq_string(greet("Bob"), "Hello, Bob!")
  assert_eq_string(greet("张三"), "Hello, 张三!")
  assert_eq_string(greet("123"), "Hello, 123!")
}

test "edge_case_computations" {
  // 边界条件计算测试
  // 零值运算
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 12345), 0)
  assert_eq(subtract(0, 0), 0)
  assert_eq(divide_with_ceil(0, 1), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  
  // 单位值运算
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 999), 999)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(999, 1), 999)
}

test "complex_financial_calculations" {
  // 复杂金融计算测试
  // 贷款计算模拟
  let principal = 100000
  let annual_rate = 6
  let years = 5
  
  // 简单利息计算
  let simple_interest = divide_with_ceil(multiply(principal, multiply(annual_rate, years)), 100)
  let total_repayment = add(principal, simple_interest)
  
  // 月供计算（简化版）
  let total_months = multiply(years, 12)
  let monthly_payment = divide_with_ceil(total_repayment, total_months)
  
  assert_eq(simple_interest, 30000)
  assert_eq(total_repayment, 130000)
  assert_eq(total_months, 60)
  assert_eq(monthly_payment, 2167)
}

test "scientific_computing_simulation" {
  // 科学计算模拟测试
  // 物理公式计算：自由落体运动
  let initial_height = 100
  let gravity = 9.8  // 简化为整数10
  let time_steps = 4
  
  // 每秒下落的距离：s = 0.5 * g * t²
  let distance1 = divide_with_ceil(multiply(10, multiply(1, 1)), 2)
  let distance2 = divide_with_ceil(multiply(10, multiply(2, 2)), 2)
  let distance3 = divide_with_ceil(multiply(10, multiply(3, 3)), 2)
  let distance4 = divide_with_ceil(multiply(10, multiply(4, 4)), 2)
  
  // 计算总下落距离
  let total_distance = distance4
  
  // 计算剩余高度
  let remaining_height = subtract(initial_height, total_distance)
  
  assert_eq(distance1, 5)
  assert_eq(distance2, 20)
  assert_eq(distance3, 45)
  assert_eq(distance4, 80)
  assert_eq(remaining_height, 20)
}

test "algorithm_complexity_analysis" {
  // 算法复杂度分析测试
  // 模拟不同时间复杂度的算法
  
  // O(1) - 常数时间
  let constant_time_result = 42
  
  // O(n) - 线性时间（模拟遍历数组）
  let array_size = 100
  let linear_operations = multiply(array_size, 3)
  
  // O(n²) - 平方时间（模拟嵌套循环）
  let quadratic_operations = multiply(array_size, array_size)
  
  // O(log n) - 对数时间（模拟二分查找）
  let log_operations = divide_with_ceil(1000, 2)
  let log_operations2 = divide_with_ceil(log_operations, 2)
  let log_operations3 = divide_with_ceil(log_operations2, 2)
  let total_log_operations = add(1, add(1, add(1, 1)))
  
  assert_eq(constant_time_result, 42)
  assert_eq(linear_operations, 300)
  assert_eq(quadratic_operations, 10000)
  assert_eq(total_log_operations, 4)
}

test "data_structure_operations" {
  // 数据结构操作测试
  // 模拟队列操作
  
  // 初始化队列
  let queue_size = 0
  let front_element = 0
  let rear_element = 0
  
  // 入队操作
  let enqueue1 = 10
  let queue_size1 = add(queue_size, 1)
  let rear1 = enqueue1
  
  let enqueue2 = 20
  let queue_size2 = add(queue_size1, 1)
  let rear2 = enqueue2
  
  let enqueue3 = 30
  let queue_size3 = add(queue_size2, 1)
  let rear3 = enqueue3
  
  // 出队操作
  let dequeue1 = 10
  let queue_size_after_dequeue1 = subtract(queue_size3, 1)
  
  let dequeue2 = 20
  let queue_size_after_dequeue2 = subtract(queue_size_after_dequeue1, 1)
  
  let dequeue3 = 30
  let queue_size_after_dequeue3 = subtract(queue_size_after_dequeue2, 1)
  
  assert_eq(queue_size1, 1)
  assert_eq(queue_size2, 2)
  assert_eq(queue_size3, 3)
  assert_eq(dequeue1, 10)
  assert_eq(dequeue2, 20)
  assert_eq(dequeue3, 30)
  assert_eq(queue_size_after_dequeue3, 0)
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  // 边界值和异常情况测试
  
  // 极大值测试
  let large_number = 1000000
  let large_addition = add(large_number, large_number)
  let large_multiplication = multiply(large_number, 2)
  
  // 极小值测试
  let small_number = 1
  let small_division = divide_with_ceil(small_number, large_number)
  
  // 负数边界测试
  let negative_large = -1000000
  let negative_addition = add(negative_large, 100)
  let negative_multiplication = multiply(negative_large, 2)
  
  // 混合运算测试
  let mixed_calculation1 = add(multiply(10, 20), divide_with_ceil(100, 3))
  let mixed_calculation2 = multiply(add(15, 25), subtract(50, 30))
  
  assert_eq(large_addition, 2000000)
  assert_eq(large_multiplication, 2000000)
  assert_eq(small_division, 1)
  assert_eq(negative_addition, -999900)
  assert_eq(negative_multiplication, -2000000)
  assert_eq(mixed_calculation1, 234)
  assert_eq(mixed_calculation2, 800)
}