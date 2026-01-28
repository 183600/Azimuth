// 高质量 MoonBit 测试用例
// 针对 azimuth 库核心功能的综合测试

test "add_comprehensive_functionality" {
  // 全面测试加法功能
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
  @azimuth.assert_eq(-1, @azimuth.add(2, -3))
}

test "multiply_comprehensive_functionality" {
  // 全面测试乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
  @azimuth.assert_eq(10000, @azimuth.multiply(100, 100))
}

test "greet_multilingual_support" {
  // 测试多语言问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, María!", @azimuth.greet("María"))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "mathematical_laws_verification" {
  // 验证数学定律
  let a = 8
  let b = 12
  
  // 交换律验证
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
}

test "complex_business_calculations" {
  // 复杂业务计算测试
  let product_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping_cost = 20
  
  // 计算小计
  let subtotal = @azimuth.multiply(product_price, quantity)
  // 计算税费
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_cost = @azimuth.add(@azimuth.add(subtotal, tax_amount), shipping_cost)
  
  @azimuth.assert_eq(270, subtotal)
  @azimuth.assert_eq(22, tax_amount)
  @azimuth.assert_eq(312, total_cost)
}

test "resource_allocation_planning" {
  // 资源分配规划测试
  let total_items = 85
  let container_capacity = 12
  let containers_needed = @azimuth.divide_with_ceil(total_items, container_capacity)
  
  let cost_per_container = 15
  let total_cost = @azimuth.multiply(containers_needed, cost_per_container)
  
  @azimuth.assert_eq(8, containers_needed)
  @azimuth.assert_eq(120, total_cost)
}

test "time_management_calculations" {
  // 时间管理计算测试
  let total_tasks = 47
  let tasks_per_day = 6
  let work_days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  let hours_per_day = 8
  let total_work_hours = @azimuth.multiply(work_days_needed, hours_per_day)
  
  @azimuth.assert_eq(8, work_days_needed)
  @azimuth.assert_eq(64, total_work_hours)
}

test "edge_cases_and_boundary_values" {
  // 边界值和特殊情况测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(101, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -1000))
}

test "string_processing_with_special_characters" {
  // 特殊字符字符串处理测试
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, A+B=C!", @azimuth.greet("A+B=C"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, \t!", @azimuth.greet("\t"))
}