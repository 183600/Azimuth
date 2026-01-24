// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "mathematical_identity_properties" {
  // 测试数学单位元性质
  // 加法单位元：a + 0 = a
  assert_eq(42, add(42, 0))
  assert_eq(-17, add(-17, 0))
  assert_eq(0, add(0, 0))
  
  // 乘法单位元：a * 1 = a
  assert_eq(42, multiply(42, 1))
  assert_eq(-17, multiply(-17, 1))
  assert_eq(0, multiply(0, 1))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))  // -11/3 = -3.666... -> -4
  
  // 负数除负数
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(-9, -3))   // -9/-3 = 3 -> 3
  
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, !", greet(""))  // 空字符串测试
}

test "complex_business_calculations" {
  // 复杂业务计算场景：订单总价计算
  let product_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping = 12
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  // 计算税费（简化为固定金额）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_shipping = add(subtotal, tax)
  let total_before_discount = add(total_before_shipping, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)  // 89 * 4
  assert_eq(29, tax)        // ceil(356 * 8 / 100) = ceil(28.48) = 29
  assert_eq(385, total_before_shipping)  // 356 + 29
  assert_eq(397, total_before_discount)  // 385 + 12
  assert_eq(377, final_total)  // 397 - 20
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  let c = 5
  
  // 加法交换律：a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律：a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律：(a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律：(a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律：a * (b + c) = a * b + a * c
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "edge_cases_and_boundary_values" {
  // 边界值和极端情况测试
  // 零值操作
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, -10))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 单位操作
  assert_eq(5, multiply(5, 1))
  assert_eq(5, add(5, 0))
  assert_eq(5, divide_with_ceil(5, 1))
  
  // 相反数操作
  assert_eq(0, add(5, -5))
  assert_eq(-25, multiply(5, -5))
  assert_eq(25, multiply(-5, -5))
}

test "real_world_packaging_calculation" {
  // 实际应用场景：包装计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, items_per_box)
  let empty_space = add(total_capacity, -total_items)
  
  assert_eq(4, boxes_needed)     // ceil(47/12) = 4
  assert_eq(48, total_capacity)  // 4 * 12
  assert_eq(1, empty_space)      // 48 - 47
}

test "time_and_resource_management" {
  // 时间和资源管理计算
  let tasks_per_day = 8
  let total_tasks = 35
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算工作小时数（每天8小时）
  let total_hours = multiply(days_needed, 8)
  
  // 计算剩余时间
  let total_capacity = multiply(days_needed, tasks_per_day)
  let spare_tasks = add(total_capacity, -total_tasks)
  
  assert_eq(5, days_needed)      // ceil(35/8) = 5
  assert_eq(40, total_hours)     // 5 * 8
  assert_eq(40, total_capacity)  // 5 * 8
  assert_eq(5, spare_tasks)      // 40 - 35
}

test "financial_calculation_scenario" {
  // 金融计算场景：预算分配
  let total_budget = 10000
  let marketing_percentage = 25
  let development_percentage = 40
  let operations_percentage = 20
  
  // 计算各部门预算
  let marketing_budget = divide_with_ceil(multiply(total_budget, marketing_percentage), 100)
  let development_budget = divide_with_ceil(multiply(total_budget, development_percentage), 100)
  let operations_budget = divide_with_ceil(multiply(total_budget, operations_percentage), 100)
  
  // 计算总分配预算
  let total_allocated = add(add(marketing_budget, development_budget), operations_budget)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -total_allocated)
  
  assert_eq(2500, marketing_budget)    // ceil(10000 * 25 / 100)
  assert_eq(4000, development_budget)  // ceil(10000 * 40 / 100)
  assert_eq(2000, operations_budget)   // ceil(10000 * 20 / 100)
  assert_eq(8500, total_allocated)     // 2500 + 4000 + 2000
  assert_eq(1500, remaining_budget)    // 10000 - 8500
}

test "comprehensive_function_integration" {
  // 综合功能集成测试
  let base_value = 100
  let multiplier = 3
  let divisor = 7
  let addition = 15
  
  // 复杂数学表达式：(base * multiplier) / divisor + addition
  let multiplied = multiply(base_value, multiplier)
  let divided = divide_with_ceil(multiplied, divisor)
  let final_result = add(divided, addition)
  
  // 字符串处理集成
  let name = "MoonBit"
  let greeting = greet(name)
  
  // 验证结果
  assert_eq(300, multiplied)      // 100 * 3
  assert_eq(43, divided)          // ceil(300 / 7) = ceil(42.857...) = 43
  assert_eq(58, final_result)     // 43 + 15
  assert_eq_string("Hello, MoonBit!", greeting)
}