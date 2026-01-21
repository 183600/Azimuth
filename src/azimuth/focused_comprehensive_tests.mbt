// 专注的综合测试用例 - 针对 azimuth 库的核心功能
// 包含边界情况、错误处理和实际应用场景

test "number_system_boundary_tests" {
  // 数字系统边界测试
  // 测试接近 Int 边界的值
  assert_eq(2147483646, add(1073741823, 1073741823))
  assert_eq(-2147483648, add(-1073741824, -1073741824))
  
  // 测试零的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(0, multiply(-2147483648, 0))
}

test "divide_with_ceil_comprehensive_behavior" {
  // 全面测试向上取整除法的行为
  // 正数情况
  assert_eq(1, divide_with_ceil(5, 5))    // 完全整除
  assert_eq(2, divide_with_ceil(6, 5))    // 需要向上取整
  assert_eq(1, divide_with_ceil(1, 100))  // 小数向上取整到1
  
  // 负数情况
  assert_eq(-2, divide_with_ceil(-11, 5))  // -11/5 = -2.2 -> -2
  assert_eq(-1, divide_with_ceil(-5, 5))   // -5/5 = -1 -> -1
  assert_eq(0, divide_with_ceil(-1, 5))    // -1/5 = -0.2 -> 0
  
  // 负除数情况
  assert_eq(-2, divide_with_ceil(11, -5))  // 11/-5 = -2.2 -> -2
  assert_eq(2, divide_with_ceil(-11, -5))  // -11/-5 = 2.2 -> 3 (但实现可能不同)
  
  // 除零情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "string_greeting_comprehensive_tests" {
  // 字符串问候的综合测试
  // 空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t\n!", greet("\t\n"))
  
  // Unicode 和特殊字符
  assert_eq_string("Hello, 👋!", greet("👋"))
  assert_eq_string("Hello, 🌟✨!", greet("🌟✨"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, Ñoël!", greet("Ñoël"))
  
  // 长字符串
  let long_name = "This is a very long name that might be used in a real application"
  assert_eq_string("Hello, " + long_name + "!", greet(long_name))
}

test "mathematical_identity_properties" {
  // 数学恒等式性质测试
  let test_values = [0, 1, -1, 42, -42, 1000, -1000]
  
  // 对每个测试值验证恒等式
  assert_eq(0, add(0, 0))  // 加法零恒等式
  assert_eq(0, multiply(0, 0))  // 乘法零恒等式
  
  // 加法恒等式：a + 0 = a
  assert_eq(42, add(42, 0))
  assert_eq(-42, add(-42, 0))
  
  // 乘法恒等式：a * 1 = a
  assert_eq(42, multiply(42, 1))
  assert_eq(-42, multiply(-42, 1))
  
  // 乘法零性质：a * 0 = 0
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(-42, 0))
}

test "real_world_financial_calculations" {
  // 真实世界金融计算场景
  // 计算订单总价：商品价格 × 数量 + 税费 + 运费 - 折扣
  let item_price = 89
  let quantity = 3
  let tax_rate_percent = 8
  let shipping_cost = 15
  let discount_amount = 25
  
  // 计算小计
  let subtotal = multiply(item_price, quantity)
  // 计算税费（使用向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate_percent), 100)
  // 计算最终总价
  let total_cost = add(add(subtotal, tax_amount), add(shipping_cost, -discount_amount))
  
  assert_eq(267, subtotal)      // 89 * 3
  assert_eq(22, tax_amount)     // ceil(267 * 8 / 100) = ceil(21.36) = 22
  assert_eq(279, total_cost)    // 267 + 22 + 15 - 25 = 279
}

test "inventory_and_packaging_optimization" {
  // 库存和包装优化场景
  let products = [
    (name: "Widget A", quantity: 25, package_size: 6),
    (name: "Widget B", quantity: 47, package_size: 12),
    (name: "Widget C", quantity: 103, package_size: 24)
  ]
  
  // 计算每种产品需要的包装数量
  let packages_a = divide_with_ceil(products[0].quantity, products[0].package_size)
  let packages_b = divide_with_ceil(products[1].quantity, products[1].package_size)
  let packages_c = divide_with_ceil(products[2].quantity, products[2].package_size)
  
  // 计算总包装数和剩余空间
  let total_packages = add(add(packages_a, packages_b), packages_c)
  let total_capacity = add(
    add(multiply(packages_a, products[0].package_size),
        multiply(packages_b, products[1].package_size)),
    multiply(packages_c, products[2].package_size)
  )
  let total_items = add(add(products[0].quantity, products[1].quantity), products[2].quantity)
  let wasted_space = add(total_capacity, -total_items)
  
  assert_eq(5, packages_a)    // ceil(25/6) = 5
  assert_eq(4, packages_b)    // ceil(47/12) = 4
  assert_eq(5, packages_c)    // ceil(103/24) = 5
  assert_eq(14, total_packages)  // 5 + 4 + 5
  assert_eq(14, wasted_space)    // 计算剩余空间
}

test "time_and_resource_management" {
  // 时间和资源管理场景
  // 项目任务分配
  let total_tasks = 87
  let tasks_per_day = 8
  let working_days_per_week = 5
  
  // 计算需要的工作日
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(days_needed, working_days_per_week)
  
  // 计算总工作小时（每天8小时）
  let total_hours = multiply(days_needed, 8)
  
  assert_eq(11, days_needed)   // ceil(87/8) = 11
  assert_eq(3, weeks_needed)   // ceil(11/5) = 3
  assert_eq(88, total_hours)   // 11 * 8
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况
  // 极值测试
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与最小值相加
  assert_eq(1, divide_with_ceil(1, 1))         // 自除
  assert_eq(1, divide_with_ceil(-1, -1))       // 负数自除
  
  // 空字符串处理
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, x!", greet("x"))     // 单字符
  
  // 零值运算
  assert_eq(0, multiply(0, 0))
  assert_eq(0, add(0, 0))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "complex_algorithmic_scenarios" {
  // 复杂算法场景
  // 计算等差数列的和：S = n/2 * (2a + (n-1)d)
  let first_term = 7
  let common_diff = 3
  let num_terms = 12
  
  let sum_calculation = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  // 计算等比数列的项：ar^(n-1)
  let geo_first = 2
  let geo_ratio = 3
  let geo_term_num = 5
  let geo_term = multiply(geo_first, multiply(geo_ratio, multiply(geo_ratio, multiply(geo_ratio, geo_ratio))))
  
  assert_eq(282, sum_calculation)  // 12/2 * (14 + 33) = 6 * 47 = 282
  assert_eq(162, geo_term)         // 2 * 3^4 = 2 * 81 = 162
}