// 综合质量测试用例 - 覆盖数学定律、业务场景和复杂运算

test "mathematical_associative_laws" {
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let a = 15
  let b = 27
  let c = 42
  
  let left = @azimuth.add(@azimuth.add(a, b), c)
  let right = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left, right)
  
  // 测试乘法结合律: (a * b) * c = a * (b * c)
  let m_left = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let m_right = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(m_left, m_right)
}

test "mathematical_distributive_law" {
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "inventory_management_scenario" {
  // 库存管理场景测试
  let initial_stock = 150
  let received = @azimuth.multiply(5, 25)  // 5箱，每箱25个
  let sold = @azimuth.add(30, 45)  // 两天销售量
  let damaged = 12
  let final_stock = @azimuth.add(@azimuth.add(initial_stock, received), @azimuth.add(-sold, -damaged))
  
  @azimuth.assert_eq(143, final_stock)
}

test "pricing_calculations" {
  // 定价计算场景
  let base_price = 100
  let tax_rate = 8  // 8%税率
  let discount = 15  // 15折扣
  let quantity = 3
  
  let subtotal = @azimuth.multiply(base_price, quantity)
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  let discounted_total = @azimuth.add(subtotal, -discount)
  let final_total = @azimuth.add(discounted_total, tax_amount)
  
  @azimuth.assert_eq(289, final_total)
}

test "divide_with_ceil_resource_allocation" {
  // 资源分配场景：将任务分配给可用的工人
  let total_tasks = 47
  let available_workers = 8
  let tasks_per_worker = @azimuth.divide_with_ceil(total_tasks, available_workers)
  
  @azimuth.assert_eq(6, tasks_per_worker)
  
  // 验证分配的合理性
  let total_allocated = @azimuth.multiply(tasks_per_worker, available_workers)
  @azimuth.assert_true(total_allocated >= total_tasks)
}

test "string_processing_multilingual" {
  // 多语言字符串处理测试
  let greetings = ["Hello", "Bonjour", "Hola", "你好", "こんにちは", "안녕하세요"]
  let expected = ["Hello, Hello!", "Hello, Bonjour!", "Hello, Hola!", "Hello, 你好!", "Hello, こんにちは!", "Hello, 안녕하세요!"]
  
  @azimuth.assert_eq_string(expected[0], @azimuth.greet(greetings[0]))
  @azimuth.assert_eq_string(expected[1], @azimuth.greet(greetings[1]))
  @azimuth.assert_eq_string(expected[2], @azimuth.greet(greetings[2]))
  @azimuth.assert_eq_string(expected[3], @azimuth.greet(greetings[3]))
  @azimuth.assert_eq_string(expected[4], @azimuth.greet(greetings[4]))
  @azimuth.assert_eq_string(expected[5], @azimuth.greet(greetings[5]))
}

test "complex_financial_calculation" {
  // 复杂财务计算：贷款分期计算
  let principal = 10000
  let interest_rate = 5  // 5%年利率
  let months = 12
  
  // 简化的月供计算（不包含复利）
  let annual_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, interest_rate), 100)
  let total_repayment = @azimuth.add(principal, annual_interest)
  let monthly_payment = @azimuth.divide_with_ceil(total_repayment, months)
  
  @azimuth.assert_eq(875, monthly_payment)
  
  // 验证总还款额
  let total_paid = @azimuth.multiply(monthly_payment, months)
  @azimuth.assert_true(total_paid >= total_repayment)
}

test "batch_processing_efficiency" {
  // 批处理效率测试
  let items_per_batch = 25
  let total_items = 187
  let batches_needed = @azimuth.divide_with_ceil(total_items, items_per_batch)
  
  @azimuth.assert_eq(8, batches_needed)
  
  // 计算最后一个批次的项目数
  let full_batches = @azimuth.divide_with_ceil(total_items, items_per_batch) - 1
  let items_in_full_batches = @azimuth.multiply(full_batches, items_per_batch)
  let items_in_last_batch = @azimuth.add(total_items, -items_in_full_batches)
  
  @azimuth.assert_true(items_in_last_batch <= items_per_batch)
  @azimuth.assert_true(items_in_last_batch > 0)
}

test "temperature_conversion_simulation" {
  // 温度转换模拟（使用整数运算模拟）
  let celsius = 25
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let fahrenheit = @azimuth.add(@azimuth.divide_with_ceil(@azimuth.multiply(celsius, 9), 5), 32)
  
  @azimuth.assert_eq(77, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9
  let back_to_celsius = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.add(fahrenheit, -32), 5), 9)
  @azimuth.assert_eq(celsius, back_to_celsius)
}

test "error_boundary_comprehensive" {
  // 综合错误边界测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极端值的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试极端值的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试除法的零处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-5, 0))
}