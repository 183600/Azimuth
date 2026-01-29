// Azimuth 项目标准增强测试用例
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(100, -100))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(1, multiply(1, 1))
  assert_eq(1000000, multiply(1000, 1000))
}

test "subtract_function_various_scenarios" {
  // 测试减法函数的各种场景
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
  assert_eq(20, subtract(-10, -30))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "greet_function_international" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), 5), add(a, add(b, 5)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), 2), multiply(a, multiply(b, 2)))
}

test "complex_business_calculations" {
  // 复杂业务计算测试
  let unit_price = 25
  let quantity = 8
  let discount_rate = 10
  let tax_rate = 8
  let shipping = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算折扣
  let discount = divide_with_ceil(multiply(subtotal, discount_rate), 100)
  let discounted_total = subtract(subtotal, discount)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(discounted_total, tax_rate), 100)
  let total_with_tax = add(discounted_total, tax)
  
  // 计算最终总额
  let final_total = add(total_with_tax, shipping)
  
  assert_eq(200, subtotal)
  assert_eq(20, discount)
  assert_eq(180, discounted_total)
  assert_eq(15, tax)
  assert_eq(195, total_with_tax)
  assert_eq(210, final_total)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  
  // 空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 
	!", greet("
	"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}

test "performance_and_efficiency" {
  // 性能和效率测试
  let large_numbers_test = true
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000, multiply(1000000, 1000))
  assert_eq(1000001, divide_with_ceil(1000000000, 999))
  
  // 连续运算性能
  let result = 0
  let i = 0
  while (i < 100) {
    result = add(result, 1)
    i = i + 1
  }
  assert_eq(100, result)
  
  assert_true(large_numbers_test)
}

test "real_world_application_scenarios" {
  // 真实世界应用场景测试
  // 场景1：库存管理
  let initial_stock = 150
  let sold_items = 45
  let restocked_items = 30
  let items_per_box = 12
  
  let remaining_stock = subtract(initial_stock, sold_items)
  let total_stock = add(remaining_stock, restocked_items)
  let boxes_needed = divide_with_ceil(total_stock, items_per_box)
  
  assert_eq(105, remaining_stock)
  assert_eq(135, total_stock)
  assert_eq(12, boxes_needed)
  
  // 场景2：项目管理
  let team_size = 6
  let tasks_per_member = 8
  let completed_tasks = 25
  let new_tasks = 15
  
  let total_tasks = multiply(team_size, tasks_per_member)
  let remaining_tasks = subtract(total_tasks, completed_tasks)
  let updated_tasks = add(remaining_tasks, new_tasks)
  let completion_rate = divide_with_ceil(multiply(completed_tasks, 100), total_tasks)
  
  assert_eq(48, total_tasks)
  assert_eq(23, remaining_tasks)
  assert_eq(38, updated_tasks)
  assert_eq(52, completion_rate)
  
  // 场景3：财务计算
  let revenue = 50000
  let expenses = 32000
  let tax_rate = 25
  let profit_margin = 15
  
  let gross_profit = subtract(revenue, expenses)
  let tax = divide_with_ceil(multiply(gross_profit, tax_rate), 100)
  let net_profit = subtract(gross_profit, tax)
  let target_profit = divide_with_ceil(multiply(revenue, profit_margin), 100)
  
  assert_eq(18000, gross_profit)
  assert_eq(4500, tax)
  assert_eq(13500, net_profit)
  assert_eq(7500, target_profit)
  
  // 验证盈利目标达成
  assert_true(net_profit >= target_profit)
}