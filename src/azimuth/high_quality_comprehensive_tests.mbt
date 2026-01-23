// 高质量综合测试用例 - 针对 azimuth 库
// 使用标准 MoonBit 测试语法

test "mathematical_properties_commutative" {
  // 测试加法和乘法的交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "mathematical_properties_associative" {
  // 测试加法和乘法的结合律
  let x = 8
  let y = 12
  let z = 20
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
}

test "mathematical_properties_distributive" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 7
  let c = 9
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive_edge_cases" {
  // 全面测试向上取整除法的边界情况
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))  // -11/3 = -3.666... -> -4
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 零除数安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "greet_function_comprehensive" {
  // 全面测试问候函数
  // 基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 边界情况
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  
  // 特殊字符和Unicode
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, user@example.com!", greet("user@example.com"))
}

test "complex_real_world_scenario" {
  // 复杂真实世界场景：电商订单计算
  let product_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping_cost = 15
  let discount_threshold = 300
  let discount_amount = 25
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费（使用向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = add(add(subtotal, tax_amount), shipping_cost)
  
  // 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)
  assert_eq(400, pre_discount_total)
  assert_eq(375, final_total)
}

test "performance_calculation_scenario" {
  // 性能计算场景：算法效率比较
  let algorithm_a_time = 120
  let algorithm_b_time = 85
  let test_iterations = 1000
  
  // 计算总执行时间
  let total_time_a = multiply(algorithm_a_time, test_iterations)
  let total_time_b = multiply(algorithm_b_time, test_iterations)
  
  // 计算性能提升百分比
  let time_difference = add(total_time_a, -total_time_b)
  let improvement_percentage = divide_with_ceil(multiply(time_difference, 100), total_time_a)
  
  assert_eq(120000, total_time_a)
  assert_eq(85000, total_time_b)
  assert_eq(35000, time_difference)
  assert_eq(30, improvement_percentage)
}

test "resource_allocation_optimization" {
  // 资源分配优化场景
  let total_budget = 10000
  let project_a_cost = 3500
  let project_b_cost = 2800
  let project_c_cost = 2200
  
  // 计算已分配预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以启动的小项目数量（每个小项目成本500）
  let small_project_cost = 500
  let small_projects_count = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(8500, allocated_budget)
  assert_eq(1500, remaining_budget)
  assert_eq(3, small_projects_count)
}

test "data_analysis_statistics" {
  // 数据分析统计场景
  let data_points = [15, 22, 18, 30, 25, 20, 28, 16]
  let data_count = 8
  
  // 计算总和
  let sum = add(add(add(add(add(add(add(15, 22), 18), 30), 25), 20), 28), 16)
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(sum, data_count)
  
  // 计算中位数（排序后的中间值）
  let median = divide_with_ceil(add(20, 22), 2)
  
  assert_eq(174, sum)
  assert_eq(22, average)
  assert_eq(21, median)
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合
  let first_name = "张"
  let last_name = "三"
  let title = "工程师"
  
  // 构建完整名称
  let full_name = first_name + last_name
  
  // 构建带头衔的名称
  let name_with_title = full_name + " (" + title + ")"
  
  // 生成问候
  let greeting = greet(name_with_title)
  
  assert_eq_string("Hello, 张三 (工程师)!", greeting)
  
  // 测试特殊字符组合
  let special_chars = "αβγ + 123 = 🚀"
  let special_greeting = greet(special_chars)
  assert_eq_string("Hello, αβγ + 123 = 🚀!", special_greeting)
}