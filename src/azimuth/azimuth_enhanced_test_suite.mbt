// 新增的简洁 MoonBit 测试用例
// 针对 azimuth 库的核心功能测试

test "enhanced_add_comprehensive" {
  // 综合测试加法功能
  assert_eq(100, add(50, 50))
  assert_eq(0, add(1000, -1000))
  assert_eq(-200, add(-100, -100))
  assert_eq(1, add(2147483647, -2147483646))
}

test "enhanced_multiply_edge_cases" {
  // 测试乘法的边界情况
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(1000, -1000))
  assert_eq(1, multiply(-1, -1))
  assert_eq(0, multiply(2147483647, 0))
}

test "enhanced_greet_unicode" {
  // 测试 Unicode 字符串问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
}

test "enhanced_divide_complex" {
  // 复杂的除法向上取整测试
  assert_eq(7, divide_with_ceil(100, 15))
  assert_eq(-6, divide_with_ceil(-100, 15))
  assert_eq(7, divide_with_ceil(-100, -15))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "enhanced_business_calculation" {
  // 业务计算测试
  let unit_price = 25
  let quantity = 8
  let discount = 20
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(180, total)
}

test "enhanced_packaging_optimization" {
  // 包装优化计算
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  let total_boxes = multiply(boxes_needed, 1)
  
  assert_eq(4, boxes_needed)
  assert_eq(4, total_boxes)
}

test "enhanced_performance_metrics" {
  // 性能指标计算
  let baseline = 1000
  let improvement_factor = 3
  let overhead = 150
  
  let improved = multiply(baseline, improvement_factor)
  let net_performance = add(improved, -overhead)
  
  assert_eq(3000, improved)
  assert_eq(2850, net_performance)
}

test "enhanced_resource_allocation" {
  // 资源分配计算
  let total_budget = 5000
  let project_cost = 1200
  let projects_count = divide_with_ceil(total_budget, project_cost)
  
  assert_eq(5, projects_count)
}