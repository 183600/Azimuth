// Azimuth 项目额外的标准 MoonBit 测试用例
// 包含基本的数学运算和字符串处理功能的测试

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(10, 0))
  @azimuth.assert_eq(-18, @azimuth.multiply(-3, 6))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "greet_function_variations" {
  // 测试问候函数的各种变体
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
}

test "practical_business_calculations" {
  // 实际业务计算测试
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(16, tax)
  @azimuth.assert_eq(231, total)
}

test "mathematical_properties" {
  // 数学性质测试
  let x = 12
  let y = 8
  
  // 交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), 5), @azimuth.add(x, @azimuth.add(y, 5)))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, 3)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3)))
}

test "packaging_and_shipping_calculations" {
  // 包装和运输计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  let weight_per_item = 2
  let total_weight = @azimuth.multiply(total_items, weight_per_item)
  
  let shipping_cost_per_box = 8
  let total_shipping = @azimuth.multiply(boxes_needed, shipping_cost_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)
  @azimuth.assert_eq(94, total_weight)
  @azimuth.assert_eq(32, total_shipping)
}

test "time_and_project_management" {
  // 时间和项目管理计算
  let tasks_per_day = 6
  let total_tasks = 35
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  let hours_per_day = 8
  let total_hours = @azimuth.multiply(days_needed, hours_per_day)
  
  @azimuth.assert_eq(6, days_needed)
  @azimuth.assert_eq(48, total_hours)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况
  // 测试零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  
  // 测试负数运算
  @azimuth.assert_eq(-15, @azimuth.add(-8, -7))
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(-20, @azimuth.multiply(4, -5))
  
  // 测试除零保护
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-50, 0))
}

test "complex_multi_step_calculations" {
  // 复杂多步计算
  // 场景：计算订单总价，包含折扣和税费
  let product_a_price = 45
  let product_a_qty = 3
  let product_b_price = 28
  let product_b_qty = 2
  
  let subtotal_a = @azimuth.multiply(product_a_price, product_a_qty)
  let subtotal_b = @azimuth.multiply(product_b_price, product_b_qty)
  let subtotal = @azimuth.add(subtotal_a, subtotal_b)
  
  let discount = 20
  let after_discount = @azimuth.add(subtotal, -discount)
  
  let tax_rate = 10
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(after_discount, tax_rate), 100)
  
  let shipping = 12
  let final_total = @azimuth.add(@azimuth.add(after_discount, tax), shipping)
  
  @azimuth.assert_eq(135, subtotal_a)
  @azimuth.assert_eq(56, subtotal_b)
  @azimuth.assert_eq(191, subtotal)
  @azimuth.assert_eq(171, after_discount)
  @azimuth.assert_eq(18, tax)
  @azimuth.assert_eq(201, final_total)
}

test "string_and_numeric_integration" {
  // 字符串和数值集成测试
  let customer_id = 12345
  let customer_name = "张三"
  let order_number = "ORD-2024-001"
  
  let greeting1 = @azimuth.greet(customer_name)
  let greeting2 = @azimuth.greet(order_number)
  
  // 计算订单相关数值
  let base_amount = 100
  let multiplier = customer_id % 10 + 1  // 使用客户ID的最后一位数作为倍数
  let order_amount = @azimuth.multiply(base_amount, multiplier)
  
  @azimuth.assert_eq_string("Hello, 张三!", greeting1)
  @azimuth.assert_eq_string("Hello, ORD-2024-001!", greeting2)
  @azimuth.assert_eq(600, order_amount)  // 100 * (5 + 1) = 600
}