// 额外的 MoonBit 标准测试用例
// 针对 azimuth 库函数的高质量测试

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(-10, 25), add(25, -10))
  assert_eq(add(-15, -30), add(-30, -15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 7
  let c = 3
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)  // 5 * (7 + 3) = 5 * 7 + 5 * 3 = 50
}

test "divide_with_ceil_fractions" {
  // 测试向上取整除法的小数情况
  assert_eq(2, divide_with_ceil(3, 2))    // 1.5 -> 2
  assert_eq(3, divide_with_ceil(8, 3))    // 2.666... -> 3
  assert_eq(5, divide_with_ceil(19, 4))   // 4.75 -> 5
  assert_eq(7, divide_with_ceil(50, 8))   // 6.25 -> 7
}

test "greet_function_multilingual_support" {
  // 测试多语言支持的问候功能
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Mundo!", greet("Mundo"))
  assert_eq_string("Hello, Мир!", greet("Мир"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "complex_financial_calculation" {
  // 复杂金融计算：计算贷款月供
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率百分比
  let years = 30          // 贷款年限
  
  // 简化计算：月供 = (本金 + 总利息) / 总月数，向上取整
  let total_interest = divide_with_ceil(multiply(principal, annual_rate), 100)
  let total_payment = add(principal, total_interest)
  let monthly_payment = divide_with_ceil(total_payment, multiply(years, 12))
  
  assert_eq(278, monthly_payment)  // (100000 + 6000) / 360 = 294.44 -> 295
}

test "inventory_optimization" {
  // 库存优化：计算最优订货量
  let monthly_demand = 450
  let ordering_cost = 50
  let holding_cost_per_unit = 2
  
  // EOQ公式简化版：sqrt(2 * D * S / H)，向上取整到整数
  let eoq_numerator = multiply(2, multiply(monthly_demand, ordering_cost))
  let eoq_approximation = divide_with_ceil(eoq_numerator, holding_cost_per_unit)
  
  // 验证计算结果
  assert_eq(22500, eoq_numerator)  // 2 * 450 * 50 = 45000
  assert_eq(11250, eoq_approximation)  // 45000 / 2 = 22500
}

test "time_management_scenario" {
  // 时间管理场景：计算项目完成时间
  let total_tasks = 87
  let tasks_per_day = 6
  let work_days_per_week = 5
  
  // 计算需要的工作日
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  assert_eq(15, work_days_needed)  // ceil(87/6) = 15
  assert_eq(3, weeks_needed)       // ceil(15/5) = 3
}

test "resource_allocation_problem" {
  // 资源分配问题：如何公平分配资源
  let total_resources = 73
  let departments = 8
  let minimum_per_dept = 5
  
  // 每个部门先分配最小资源，剩余资源平均分配
  let allocated_minimum = multiply(minimum_per_dept, departments)
  let remaining_resources = add(total_resources, -allocated_minimum)
  let additional_per_dept = divide_with_ceil(remaining_resources, departments)
  
  let total_per_dept = add(minimum_per_dept, additional_per_dept)
  
  assert_eq(40, allocated_minimum)      // 5 * 8 = 40
  assert_eq(33, remaining_resources)    // 73 - 40 = 33
  assert_eq(5, additional_per_dept)     // ceil(33/8) = 5
  assert_eq(10, total_per_dept)         // 5 + 5 = 10
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：斐波那契数列相关
  let n = 10
  let fib_1 = 1
  let fib_2 = 1
  
  // 使用加法计算前10项斐波那契数的和（简化版）
  let fib_sum = add(fib_1, fib_2)  // 前两项
  let fib_3 = add(fib_1, fib_2)    // 2
  let fib_4 = add(fib_2, fib_3)    // 3
  let fib_5 = add(fib_3, fib_4)    // 5
  let fib_6 = add(fib_4, fib_5)    // 8
  let fib_7 = add(fib_5, fib_6)    // 13
  let fib_8 = add(fib_6, fib_7)    // 21
  let fib_9 = add(fib_7, fib_8)    // 34
  let fib_10 = add(fib_8, fib_9)   // 55
  
  let total_sum = add(add(add(add(add(fib_sum, fib_3), add(fib_4, fib_5)), 
                      add(fib_6, fib_7)), add(fib_8, add(fib_9, fib_10)))
  
  assert_eq(143, total_sum)  // 1+1+2+3+5+8+13+21+34+55 = 143
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数的高级测试
  let prefix = "Dr."
  let first_name = "Jane"
  let last_name = "Smith"
  let title = "PhD"
  
  // 构建完整姓名
  let full_name = prefix + " " + first_name + " " + last_name + ", " + title
  let formal_greeting = greet(full_name)
  
  assert_eq_string("Hello, Dr. Jane Smith, PhD!", formal_greeting)
  
  // 测试空字符串和特殊字符的组合
  let empty_name = ""
  let special_chars = "!@#$%^&*()"
  let combined = empty_name + special_chars
  let special_greeting = greet(combined)
  
  assert_eq_string("Hello, !@#$%^&*()!", special_greeting)
}