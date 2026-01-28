// 高质量 MoonBit 测试用例 - 针对 azimuth 库的全面测试
// 使用标准 MoonBit 测试语法，覆盖核心功能和边界情况

test "addition_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 42
  let b = 17
  assert_eq(add(a, b), add(b, a))
  
  // 测试负数情况
  let x = -15
  let y = 23
  assert_eq(add(x, y), add(y, x))
  
  // 测试零的情况
  assert_eq(add(0, 100), add(100, 0))
  assert_eq(add(0, -50), add(-50, 0))
}

test "multiplication_identity_property" {
  // 测试乘法单位元性质：a * 1 = a
  let test_values = [0, 1, -1, 42, -17, 1000, -999]
  
  // 使用循环方式测试多个值
  assert_eq(multiply(test_values[0], 1), test_values[0])
  assert_eq(multiply(test_values[1], 1), test_values[1])
  assert_eq(multiply(test_values[2], 1), test_values[2])
  assert_eq(multiply(test_values[3], 1), test_values[3])
  assert_eq(multiply(test_values[4], 1), test_values[4])
  assert_eq(multiply(test_values[5], 1), test_values[5])
  assert_eq(multiply(test_values[6], 1), test_values[6])
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法功能
  
  // 正数除法测试
  assert_eq(4, divide_with_ceil(10, 3))   // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))    // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100))  // 0.01 -> 1
  assert_eq(7, divide_with_ceil(13, 2))   // 6.5 -> 7
  
  // 负数除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))  // -3.666... -> -4
  assert_eq(3, divide_with_ceil(10, -3))   // -3.333... -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // 3.333... -> 4
  
  // 边界情况测试
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为零
  assert_eq(0, divide_with_ceil(0, 5))     // 被除数为零
  assert_eq(0, divide_with_ceil(0, 0))     // 两者都为零
}

test "greet_function_unicode_handling" {
  // 测试问候函数的 Unicode 字符处理
  
  // 基本测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // Unicode 字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  
  // 边界情况测试
  assert_eq_string("Hello, !", greet(""))           // 空字符串
  assert_eq_string("Hello, A!", greet("A"))         // 单字符
  assert_eq_string("Hello, 123!", greet("123"))     // 数字字符串
}

test "complex_business_calculation" {
  // 复杂业务计算：电商订单总价计算
  
  let product_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping_cost = 12
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_before_discount = add(subtotal, tax_amount)
  let total_with_shipping = add(total_before_discount, shipping_cost)
  let final_total = add(total_with_shipping, -discount)
  
  // 验证计算结果
  assert_eq(356, subtotal)           // 89 * 4
  assert_eq(29, tax_amount)          // ceil(356 * 8 / 100) = ceil(28.48)
  assert_eq(385, total_before_discount)  // 356 + 29
  assert_eq(397, total_with_shipping)    // 385 + 12
  assert_eq(377, final_total)        // 397 - 20
}

test "mathematical_distributive_property" {
  // 测试分配律：a * (b + c) = a * b + a * c
  
  let a = 7
  let b = 12
  let c = 5
  
  // 计算左侧：a * (b + c)
  let left_side = multiply(a, add(b, c))
  
  // 计算右侧：a * b + a * c
  let right_side = add(multiply(a, b), multiply(a, c))
  
  // 验证分配律
  assert_eq(left_side, right_side)  // 7 * (12 + 5) = 7 * 12 + 7 * 5 = 119
}

test "resource_allocation_scenario" {
  // 资源分配场景：计算项目资源需求
  
  let total_tasks = 47
  let tasks_per_person = 5
  let work_days = 8
  
  // 计算需要的人员数量
  let people_needed = divide_with_ceil(total_tasks, tasks_per_person)
  
  // 计算总工作日（考虑周末）
  let calendar_days_needed = multiply(people_needed, work_days)
  
  // 计算工作周数（每周5个工作日）
  let weeks_needed = divide_with_ceil(calendar_days_needed, 5)
  
  // 验证计算结果
  assert_eq(10, people_needed)       // ceil(47 / 5) = 10
  assert_eq(80, calendar_days_needed)  // 10 * 8 = 80
  assert_eq(16, weeks_needed)        // ceil(80 / 5) = 16
}

test "performance_metrics_calculation" {
  // 性能指标计算：系统性能评估
  
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 15
  
  // 计算优化后的操作数
  let optimized_operations = multiply(base_operations, optimization_factor)
  
  // 计算系统开销
  let system_overhead = divide_with_ceil(multiply(optimized_operations, overhead_percentage), 100)
  
  // 计算实际性能
  let actual_performance = add(optimized_operations, -system_overhead)
  
  // 计算性能提升倍数
  let performance_improvement = divide_with_ceil(actual_performance, base_operations)
  
  // 验证计算结果
  assert_eq(3000, optimized_operations)     // 1000 * 3
  assert_eq(450, system_overhead)           // ceil(3000 * 15 / 100)
  assert_eq(2550, actual_performance)       // 3000 - 450
  assert_eq(3, performance_improvement)     // ceil(2550 / 1000)
}

test "edge_case_comprehensive" {
  // 综合边界情况测试
  
  // 极值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(1000, 0))
  assert_eq(1000, multiply(1000, 1))
  
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  
  // 除法边界测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(1, 0))
  
  // 字符串边界测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "real_world_financial_calculation" {
  // 真实世界金融计算：投资回报计算
  
  let initial_investment = 10000
  let annual_return_rate = 8  // 8%
  let years = 3
  let inflation_rate = 2      // 2%
  
  // 计算第一年收益
  let year1_return = divide_with_ceil(multiply(initial_investment, annual_return_rate), 100)
  let year1_total = add(initial_investment, year1_return)
  
  // 计算第二年收益（基于第一年总额）
  let year2_return = divide_with_ceil(multiply(year1_total, annual_return_rate), 100)
  let year2_total = add(year1_total, year2_return)
  
  // 计算第三年收益（基于第二年总额）
  let year3_return = divide_with_ceil(multiply(year2_total, annual_return_rate), 100)
  let year3_total = add(year2_total, year3_return)
  
  // 计算通货膨胀调整后的实际价值
  let inflation_factor = divide_with_ceil(multiply(year3_total, inflation_rate), 100)
  let real_value = add(year3_total, -inflation_factor)
  
  // 验证计算结果
  assert_eq(800, year1_return)           // ceil(10000 * 8 / 100)
  assert_eq(10800, year1_total)          // 10000 + 800
  assert_eq(864, year2_return)           // ceil(10800 * 8 / 100)
  assert_eq(11664, year2_total)          // 10800 + 864
  assert_eq(934, year3_return)           // ceil(11664 * 8 / 100)
  assert_eq(12598, year3_total)          // 11664 + 934
  assert_eq(252, inflation_factor)       // ceil(12598 * 2 / 100)
  assert_eq(12346, real_value)           // 12598 - 252
}