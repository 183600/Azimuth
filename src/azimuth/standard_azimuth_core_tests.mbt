// 标准 azimuth 核心测试用例
// 包含10个高质量的 MoonBit 测试用例，覆盖库的主要功能

test "add_function_comprehensive" {
  // 全面测试加法功能
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(1, @azimuth.add(2147483647, -2147483646))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(-24, @azimuth.multiply(-8, 3))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
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

test "mathematical_properties" {
  // 数学性质验证
  let x = 12
  let y = 8
  
  // 交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  let z = 5
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, z)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z)))
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 边界值测试
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
}

test "real_world_inventory_management" {
  // 现实世界库存管理场景
  let items_per_box = 24
  let total_items = 100
  
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  let remaining_space = @azimuth.multiply(boxes_needed, items_per_box) - total_items
  
  @azimuth.assert_eq(5, boxes_needed)
  @azimuth.assert_eq(20, remaining_space)
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + " " + last_name
  let greeting = @azimuth.greet(full_name)
  
  @azimuth.assert_eq_string("Hello, 张 三!", greeting)
  
  // 测试特殊字符
  let special_chars = "Hello & Goodbye"
  @azimuth.assert_eq_string("Hello, Hello & Goodbye!", @azimuth.greet(special_chars))
}

test "comprehensive_financial_calculation" {
  // 综合金融计算场景
  let principal = 1000
  let rate_percent = 5
  let _years = 3
  
  // 计算复利（简化版）
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate_percent), 100))
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate_percent), 100))
  
  @azimuth.assert_eq(1050, year1)
  @azimuth.assert_eq(1103, year2)
  @azimuth.assert_eq(1158, year3)
}