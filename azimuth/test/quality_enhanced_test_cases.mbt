// 高质量增强测试用例
// 使用标准 MoonBit 测试语法测试 azimuth 库功能

test "add_function_comprehensive" {
  // 测试加法函数的综合情况
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(0, @azimuth.add(50, -50))
  @azimuth.assert_eq(1, @azimuth.add(0, 1))
  @azimuth.assert_eq(-1, @azimuth.add(0, -1))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, &%$#@!", @azimuth.greet("&%$#@"))
  @azimuth.assert_eq_string("Hello, Line1\nLine2!", @azimuth.greet("Line1\nLine2"))
  @azimuth.assert_eq_string("Hello, Tab\tSeparated!", @azimuth.greet("Tab\tSeparated"))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的综合情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(20, 4))
  @azimuth.assert_eq(6, @azimuth.divide_with_ceil(22, 4))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-20, 4))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-15, 4))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "mathematical_distributive_property" {
  // 测试数学分配律 a * (b + c) = a * b + a * c
  let a = 7
  let b = 3
  let c = 5
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "business_calculation_scenario" {
  // 测试商业计算场景
  // 计算订单总成本：商品价格 × 数量 + 运费
  let product_price = 89
  let quantity = 3
  let shipping_cost = 15
  
  let subtotal = @azimuth.multiply(product_price, quantity)
  let total_cost = @azimuth.add(subtotal, shipping_cost)
  @azimuth.assert_eq(282, total_cost)  // 89*3 + 15 = 267 + 15 = 282
}

test "inventory_management_scenario" {
  // 测试库存管理场景
  // 计算需要的货架数量：总库存数 ÷ 每货架容量（向上取整）
  let total_inventory = 157
  let shelf_capacity = 25
  let shelves_needed = @azimuth.divide_with_ceil(total_inventory, shelf_capacity)
  @azimuth.assert_eq(7, shelves_needed)  // ceil(157/25) = 7
}

test "string_processing_validation" {
  // 测试字符串处理验证
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, Charlie!", @azimuth.greet("Charlie"))
  @azimuth.assert_eq_string("Hello, Diana!", @azimuth.greet("Diana"))
  @azimuth.assert_eq_string("Hello, Eve!", @azimuth.greet("Eve"))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算项目预算，考虑人力成本、材料成本和缓冲
  let base_salary = 5000
  let team_size = 4
  let material_cost = 8000
  let buffer_percentage = 20  // 20% 缓冲
  
  // 计算人力成本
  let labor_cost = @azimuth.multiply(base_salary, team_size)
  
  // 计算小计
  let subtotal = @azimuth.add(labor_cost, material_cost)
  
  // 计算缓冲（简化为固定值）
  let buffer = @azimuth.multiply(subtotal, buffer_percentage) / 100
  
  // 计算总预算
  let total_budget = @azimuth.add(subtotal, buffer)
  @azimuth.assert_eq(33600, total_budget)  // (5000*4 + 8000) * 1.2 = 28000 * 1.2 = 33600
}

test "error_handling_and_boundary_values" {
  // 测试错误处理和边界值
  // 测试零值和极值
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}