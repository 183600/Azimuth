// 专注核心功能测试 - 不超过10个高质量测试用例
// 测试 azimuth 库的核心功能：add, multiply, greet, divide_with_ceil

test "add_function_boundary_cases" {
  // 测试加法函数的边界情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
}

test "multiply_function_zero_cases" {
  // 测试乘法函数的零值情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.multiply(-12345, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
}

test "divide_with_ceil_precision" {
  // 测试向上取整除法的精度
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, áéíóú!", @azimuth.greet("áéíóú"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
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
  // 测试数学性质：交换律和结合律
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律测试
  let c = 3
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "inventory_packaging_scenario" {
  // 库存包装场景：计算需要的箱子数量
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)
  
  // 验证箱子容量足够
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  let is_sufficient = if total_capacity >= total_items { 1 } else { 0 }
  @azimuth.assert_eq(1, is_sufficient)
}

test "financial_compound_interest" {
  // 金融复利计算（简化版）
  let principal = 1000
  let rate_percent = 5
  
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate_percent), 100))
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate_percent), 100))
  
  @azimuth.assert_eq(1050, year1)
  @azimuth.assert_eq(1103, year2)
  @azimuth.assert_eq(1158, year3)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除数为零的安全处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值运算
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
}

test "performance_calculation" {
  // 性能计算：任务完成时间
  let total_tasks = 75
  let tasks_per_hour = 8
  let work_hours_per_day = 8
  
  let hours_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_hour)
  let days_needed = @azimuth.divide_with_ceil(hours_needed, work_hours_per_day)
  
  @azimuth.assert_eq(10, hours_needed)
  @azimuth.assert_eq(2, days_needed)
  
  // 计算总工作小时数
  let total_work_hours = @azimuth.multiply(days_needed, work_hours_per_day)
  @azimuth.assert_eq(16, total_work_hours)
}