// Azimuth 库标准增强测试用例
// 包含10个高质量的测试用例，使用标准 MoonBit 测试语法

test "subtract_function_basic_functionality" {
  // 测试减法函数的基本功能
  assert_eq(5, subtract(10, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(-5, subtract(0, 5))
  assert_eq(10, subtract(15, 5))
  assert_eq(-10, subtract(-5, 5))
}

test "mathematical_operation_chain" {
  // 测试数学运算链式操作
  let initial = 100
  let step1 = add(initial, 50)
  let step2 = subtract(step1, 30)
  let step3 = multiply(step2, 2)
  let step4 = divide_with_ceil(step3, 5)
  
  assert_eq(150, step1)
  assert_eq(120, step2)
  assert_eq(240, step3)
  assert_eq(48, step4)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  // 测试除数为零的情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "string_greeting_validation" {
  // 测试字符串问候验证
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 模拟订单计算：商品价格 × 数量 + 运费 - 折扣
  let product_price = 89
  let quantity = 3
  let shipping_cost = 15
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = subtract(total_before_discount, discount)
  
  assert_eq(267, subtotal)
  assert_eq(282, total_before_discount)
  assert_eq(262, final_total)
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 1000
  let team_size = 8
  let project_duration_days = 5
  
  // 计算每人每天预算
  let daily_budget_per_person = divide_with_ceil(total_budget, multiply(team_size, project_duration_days))
  
  // 计算总团队日预算
  let team_daily_budget = multiply(daily_budget_per_person, team_size)
  
  // 计算项目总预算验证
  let calculated_total = multiply(team_daily_budget, project_duration_days)
  
  assert_eq(25, daily_budget_per_person)
  assert_eq(200, team_daily_budget)
  assert_eq(1000, calculated_total)
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算
  // 计算等差数列前n项和：1 + 3 + 5 + 7 + 9
  let n = 5
  let first_term = 1
  let common_difference = 2
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(n, add(multiply(2, first_term), multiply(n - 1, common_difference))),
    2
  )
  
  assert_eq(25, sum)
}

test "performance_metrics_calculation" {
  // 测试性能指标计算
  let operations_completed = 850
  let total_time_seconds = 120
  let target_operations = 1000
  
  // 计算每秒操作数
  let ops_per_second = divide_with_ceil(operations_completed, total_time_seconds)
  
  // 计算完成百分比
  let completion_percentage = divide_with_ceil(multiply(operations_completed, 100), target_operations)
  
  // 计算剩余操作数
  let remaining_operations = subtract(target_operations, operations_completed)
  
  assert_eq(8, ops_per_second)
  assert_eq(85, completion_percentage)
  assert_eq(150, remaining_operations)
}

test "data_structure_simulation" {
  // 测试数据结构模拟
  // 模拟栈操作：push, push, pop, push
  let mut stack_size = 0
  
  // push 10
  stack_size = add(stack_size, 1)
  assert_eq(1, stack_size)
  
  // push 20
  stack_size = add(stack_size, 1)
  assert_eq(2, stack_size)
  
  // pop
  stack_size = subtract(stack_size, 1)
  assert_eq(1, stack_size)
  
  // push 30
  stack_size = add(stack_size, 1)
  assert_eq(2, stack_size)
}

test "comprehensive_function_integration" {
  // 测试综合功能集成
  let base_value = 42
  let multiplier = 3
  let divisor = 4
  let offset = 10
  
  // 复合计算：(base * multiplier) / divisor + offset
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let final_result = add(step2, offset)
  
  // 验证中间结果
  assert_eq(126, step1)
  assert_eq(32, step2)
  assert_eq(42, final_result)
  
  // 测试字符串集成
  let greeting_result = greet("Result_" + final_result.to_string())
  assert_eq_string("Hello, Result_42!", greeting_result)
}