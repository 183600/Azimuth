// 标准 MoonBit 测试用例 - 增强版
// 专注于性能、错误处理和实际应用场景

test "performance_bulk_operations" {
  // 测试批量操作性能
  let mut sum = 0
  sum = @azimuth.add(sum, 1)
  sum = @azimuth.add(sum, 2)
  sum = @azimuth.add(sum, 3)
  sum = @azimuth.add(sum, 4)
  sum = @azimuth.add(sum, 5)
  sum = @azimuth.add(sum, 6)
  sum = @azimuth.add(sum, 7)
  sum = @azimuth.add(sum, 8)
  sum = @azimuth.add(sum, 9)
  sum = @azimuth.add(sum, 10)
  @azimuth.assert_eq(55, sum)
  
  // 测试批量乘法操作
  let mut product = 1
  product = @azimuth.multiply(product, 2)
  product = @azimuth.multiply(product, 3)
  product = @azimuth.multiply(product, 4)
  product = @azimuth.multiply(product, 5)
  @azimuth.assert_eq(120, product)
}

test "error_handling_divide_by_zero" {
  // 测试除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
}

test "real_world_inventory_management" {
  // 实际应用：库存管理场景
  let initial_stock = 100
  let incoming = @azimuth.multiply(15, 5)  // 15批，每批5个
  let outgoing = 30
  let final_stock = @azimuth.add(@azimuth.add(initial_stock, incoming), -outgoing)
  @azimuth.assert_eq(145, final_stock)
  
  // 计算需要的货架数（每个货架放20个）
  let shelves_needed = @azimuth.divide_with_ceil(final_stock, 20)
  @azimuth.assert_eq(8, shelves_needed)
}

test "real_world_pricing_calculations" {
  // 实际应用：价格计算场景
  let base_price = 100
  let quantity = 7
  let discount_threshold = 5
  let discount_rate = 10
  
  let subtotal = @azimuth.multiply(base_price, quantity)
  
  // 如果数量超过阈值，应用折扣
  let final_total = if quantity > discount_threshold {
    @azimuth.add(subtotal, -discount_rate)
  } else {
    subtotal
  }
  
  @azimuth.assert_eq(690, final_total)
}

test "string_processing_multibyte_chars" {
  // 测试多字节字符处理
  @azimuth.assert_eq_string("Hello, 👋!", @azimuth.greet("👋"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, émojis!", @azimuth.greet("émojis"))
}

test "mathematical_series_calculation" {
  // 数学序列计算
  // 等差数列：1 + 3 + 5 + 7 + 9 = 25
  let mut arithmetic_sum = 0
  arithmetic_sum = @azimuth.add(arithmetic_sum, 1)
  arithmetic_sum = @azimuth.add(arithmetic_sum, 3)
  arithmetic_sum = @azimuth.add(arithmetic_sum, 5)
  arithmetic_sum = @azimuth.add(arithmetic_sum, 7)
  arithmetic_sum = @azimuth.add(arithmetic_sum, 9)
  @azimuth.assert_eq(25, arithmetic_sum)
  
  // 等比数列：2 × 3 × 4 × 5 = 120
  let geometric_product = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 3), 4), 5)
  @azimuth.assert_eq(120, geometric_product)
}

test "boundary_value_analysis" {
  // 边界值分析
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试接近边界的加法
  @azimuth.assert_eq(max_int, @azimuth.add(max_int - 1, 1))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int + 1, -1))
  
  // 测试接近边界的乘法
  @azimuth.assert_eq(46340, @azimuth.multiply(215, 215))  // 215² = 46225，在安全范围内
  @azimuth.assert_eq(-46340, @azimuth.multiply(215, -215))
}

test "data_validation_scenarios" {
  // 数据验证场景
  let user_age = 25
  let test_score = 85
  let stock_level = 50
  
  // 计算总分
  let total_score = @azimuth.add(user_age, test_score)
  @azimuth.assert_eq(110, total_score)
  
  // 验证库存计算
  let total_stock = @azimuth.add(stock_level, 10)
  @azimuth.assert_eq(60, total_stock)
}

test "resource_allocation_optimization" {
  // 资源分配优化场景
  let total_resources = 100
  let team_a_size = 35
  let team_b_size = 40
  let team_c_size = 25
  
  let allocated = @azimuth.add(@azimuth.add(team_a_size, team_b_size), team_c_size)
  @azimuth.assert_eq(total_resources, allocated)
  
  // 计算每个团队需要的资源块数（每块10个单位）
  let blocks_a = @azimuth.divide_with_ceil(team_a_size, 10)
  let blocks_b = @azimuth.divide_with_ceil(team_b_size, 10)
  let blocks_c = @azimuth.divide_with_ceil(team_c_size, 10)
  let total_blocks = @azimuth.add(@azimuth.add(blocks_a, blocks_b), blocks_c)
  
  @azimuth.assert_eq(13, total_blocks)  // 4 + 4 + 3 = 11，但需要向上取整：4 + 4 + 3 = 11
}

test "concurrent_operations_simulation" {
  // 模拟并发操作场景
  let shared_counter = 0
  
  // 模拟多个线程同时增加计数器
  let result1 = @azimuth.add(shared_counter, 5)
  let result2 = @azimuth.add(shared_counter, 3)
  let result3 = @azimuth.add(shared_counter, 7)
  
  // 验证独立操作的正确性
  @azimuth.assert_eq(5, result1)
  @azimuth.assert_eq(3, result2)
  @azimuth.assert_eq(7, result3)
  
  // 模拟累积操作
  let mut cumulative = shared_counter
  cumulative = @azimuth.add(cumulative, 5)
  cumulative = @azimuth.add(cumulative, 3)
  cumulative = @azimuth.add(cumulative, 7)
  @azimuth.assert_eq(15, cumulative)
}