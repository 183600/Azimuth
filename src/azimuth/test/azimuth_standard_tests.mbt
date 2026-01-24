// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(10, @azimuth.add(15, -5))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合功能
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))
}

test "greet_function_various_scenarios" {
  // 测试问候函数的各种场景
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  let product_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = @azimuth.multiply(product_price, quantity)
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(16, tax)
  @azimuth.assert_eq(231, total)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律 a * (b + c) = a * b + a * c
  let c = 5
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "edge_cases_and_boundary_values" {
  // 测试边界情况和极值
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -10))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "real_world_inventory_calculation" {
  // 实际库存计算场景
  let items_per_box = 24
  let total_items = 100
  
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  let remaining_space = @azimuth.multiply(boxes_needed, items_per_box) - total_items
  
  @azimuth.assert_eq(5, boxes_needed)
  @azimuth.assert_eq(20, remaining_space)
}

test "time_and_resource_management" {
  // 时间和资源管理测试
  let tasks_per_day = 8
  let total_tasks = 35
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = @azimuth.multiply(days_needed, 8)
  
  @azimuth.assert_eq(5, days_needed)
  @azimuth.assert_eq(40, total_hours)
}