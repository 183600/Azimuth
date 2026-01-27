// 标准 MoonBit 测试用例 - 10个高质量测试用例
// 覆盖边缘情况、实际应用场景和数学性质验证

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(10, 10))
  assert_eq(15, subtract(5, -10))
  assert_eq(-15, subtract(-5, 10))
  assert_eq(1000000, subtract(2000000, 1000000))
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "complex_shipping_calculation" {
  // 复杂运输计算场景
  let total_weight = 157  // 总重量(kg)
  let max_weight_per_box = 25  // 每箱最大重量
  let boxes_needed = divide_with_ceil(total_weight, max_weight_per_box)
  
  assert_eq(7, boxes_needed)  // ceil(157/25) = 7
  
  // 计算实际承载能力
  let total_capacity = multiply(boxes_needed, max_weight_per_box)
  assert_eq(175, total_capacity)
  
  // 计算剩余承载能力
  let remaining_capacity = subtract(total_capacity, total_weight)
  assert_eq(18, remaining_capacity)
}

test "temperature_conversion_scenario" {
  // 温度转换场景测试
  let celsius_temps = add(0, 0)  // 基准温度0°C
  let fahrenheit_base = 32
  
  // 模拟温度转换: F = C * 9/5 + 32
  let temp_20c = add(20, 0)
  let temp_20c_f = add(multiply(temp_20c, 9), divide_with_ceil(multiply(5, fahrenheit_base), 5))
  let temp_20c_f_corrected = divide_with_ceil(temp_20c_f, 5)
  
  assert_eq(68, temp_20c_f_corrected)  // 20°C = 68°F
  
  // 测试负温度
  let temp_minus_10c = subtract(0, 10)
  let temp_minus_10c_f = add(multiply(temp_minus_10c, 9), divide_with_ceil(multiply(5, fahrenheit_base), 5))
  let temp_minus_10c_f_corrected = divide_with_ceil(temp_minus_10c_f, 5)
  
  assert_eq(14, temp_minus_10c_f_corrected)  // -10°C = 14°F
}

test "inventory_restock_calculation" {
  // 库存补货计算
  let current_stock = 47
  let safety_stock = 20
  let max_order_quantity = 100
  
  // 计算需要订购的数量
  let needed_quantity = subtract(max_order_quantity, current_stock)
  let total_needed = add(current_stock, needed_quantity)
  
  assert_eq(53, needed_quantity)
  assert_eq(100, total_needed)
  
  // 计算分批配送次数（每批最多30件）
  let batch_size = 30
  let delivery_batches = divide_with_ceil(needed_quantity, batch_size)
  
  assert_eq(2, delivery_batches)  // ceil(53/30) = 2
  
  // 验证总配送量
  let total_delivered = multiply(delivery_batches, batch_size)
  assert_eq(60, total_delivered)
}

test "project_timeline_planning" {
  // 项目时间线规划
  let total_tasks = 87
  let team_members = 5
  let working_days = 22  // 每月工作日
  
  // 计算每人每天需要完成的任务数
  let tasks_per_day = divide_with_ceil(total_tasks, multiply(team_members, working_days))
  assert_eq(1, tasks_per_day)  // ceil(87/(5*22)) = 1
  
  // 计算实际完成能力
  let monthly_capacity = multiply(team_members, multiply(tasks_per_day, working_days))
  assert_eq(110, monthly_capacity)
  
  // 计算提前完成的天数
  let days_needed = divide_with_ceil(total_tasks, multiply(team_members, tasks_per_day))
  let days_saved = subtract(working_days, days_needed)
  
  assert_eq(18, days_needed)  // ceil(87/(5*1)) = 18
  assert_eq(4, days_saved)  // 22 - 18 = 4
}

test "financial_budget_allocation" {
  // 财务预算分配
  let total_budget = 50000
  let departments = 4
  let min_budget_per_dept = 10000
  
  // 计算每个部门的平均预算
  let avg_budget = divide_with_ceil(total_budget, departments)
  assert_eq(12500, avg_budget)  // ceil(50000/4) = 12500
  
  // 验证所有部门都能获得最低预算
  assert_true(avg_budget >= min_budget_per_dept)
  
  // 计算总分配预算
  let total_allocated = multiply(avg_budget, departments)
  assert_eq(50000, total_allocated)
  
  // 计算预算余量
  let budget_surplus = subtract(avg_budget, min_budget_per_dept)
  let total_surplus = multiply(budget_surplus, departments)
  
  assert_eq(2500, budget_surplus)
  assert_eq(10000, total_surplus)
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 极值运算测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  assert_eq(max_int, add(max_int, 0))
  assert_eq(min_int, add(min_int, 0))
  assert_eq(0, multiply(max_int, 0))
  assert_eq(0, multiply(min_int, 0))
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况
  // 测试各种特殊字符串
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, \n\r\t!", greet("\n\r\t"))
  assert_eq_string("Hello, \"quotes\"!", greet("\"quotes\""))
  assert_eq_string("Hello, 'apostrophe'!", greet("'apostrophe'"))
  assert_eq_string("Hello, \\backslash\\!", greet("\\backslash\\"))
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, []{}()!", greet("[]{}()"))
  assert_eq_string("Hello, <>?!", greet("<>?!"))
  assert_eq_string("Hello, |~`!", greet("|~`"))
}

test "mathematical_sequence_operations" {
  // 数学序列操作测试
  // 斐波那契数列相关计算
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)  // 2
  let fib4 = add(fib2, fib3)  // 3
  let fib5 = add(fib3, fib4)  // 5
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
  
  // 阶乘相关计算
  let factorial_3 = multiply(3, multiply(2, 1))
  let factorial_4 = multiply(4, factorial_3)
  let factorial_5 = multiply(5, factorial_4)
  
  assert_eq(6, factorial_3)
  assert_eq(24, factorial_4)
  assert_eq(120, factorial_5)
  
  // 计算组合数 C(5,2) = 5!/(2!*(5-2)!) = 10
  let numerator = factorial_5
  let denominator = multiply(factorial_3, factorial_2)
  let combination_5_2 = divide_with_ceil(numerator, denominator)
  
  assert_eq(10, combination_5_2)
}