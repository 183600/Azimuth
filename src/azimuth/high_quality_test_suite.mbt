// 高质量 MoonBit 测试套件
// 包含 10 个精心设计的测试用例，覆盖各种场景和边界情况

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  // 加法单位元：a + 0 = a
  assert_eq(42, add(42, 0))
  assert_eq(-17, add(-17, 0))
  assert_eq(0, add(0, 0))
  
  // 乘法单位元：a * 1 = a
  assert_eq(42, multiply(42, 1))
  assert_eq(-17, multiply(-17, 1))
  assert_eq(0, multiply(0, 1))
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款计算
  let principal = 100000  // 本金
  let annual_rate = 5     // 年利率5%
  let years = 3           // 贷款年限
  
  // 简化的单利计算
  let yearly_interest = divide_with_ceil(multiply(principal, annual_rate), 100)
  let total_interest = multiply(yearly_interest, years)
  let total_repayment = add(principal, total_interest)
  
  assert_eq(5000, yearly_interest)
  assert_eq(15000, total_interest)
  assert_eq(115000, total_repayment)
}

test "string_processing_internationalization" {
  // 字符串处理国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 北京!", greet("北京"))
}

test "advanced_division_ceil_scenarios" {
  // 高级向上取整除法场景
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))   // 3.666... -> 4
  assert_eq(7, divide_with_ceil(13, 2))   // 6.5 -> 7
  assert_eq(1, divide_with_ceil(1, 100))  // 0.01 -> 1
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))  // -3.666... -> -4
  assert_eq(4, divide_with_ceil(-11, -3))  // 3.666... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为0
  assert_eq(0, divide_with_ceil(0, 5))     // 被除数为0
}

test "algorithmic_sequence_calculation" {
  // 算法序列计算：等差数列求和
  let first_term = 3
  let common_diff = 4
  let num_terms = 6
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  // 手动验证：3 + 7 + 11 + 15 + 19 + 23 = 78
  assert_eq(78, sum)
}

test "real_world_inventory_management" {
  // 现实世界库存管理
  let product_a_qty = 120
  let product_b_qty = 85
  let product_c_qty = 43
  
  let box_capacity = 24
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a_qty, box_capacity)
  let boxes_b = divide_with_ceil(product_b_qty, box_capacity)
  let boxes_c = divide_with_ceil(product_c_qty, box_capacity)
  
  // 计算总箱子数量
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  
  assert_eq(5, boxes_a)   // ceil(120/24) = 5
  assert_eq(4, boxes_b)   // ceil(85/24) = 4
  assert_eq(2, boxes_c)   // ceil(43/24) = 2
  assert_eq(11, total_boxes)  // 5 + 4 + 2 = 11
}

test "mathematical_distributive_property" {
  // 数学分配律验证：a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 7 * (12 + 8) = 7 * 12 + 7 * 8 = 140
}

test "performance_benchmark_simulation" {
  // 性能基准测试模拟
  let base_operations = 1000
  let optimization_factor = 4
  let overhead_percentage = 12
  
  // 计算优化后的操作数
  let optimized_ops = multiply(base_operations, optimization_factor)
  
  // 计算开销
  let overhead = divide_with_ceil(multiply(optimized_ops, overhead_percentage), 100)
  
  // 计算实际性能
  let actual_performance = add(optimized_ops, -overhead)
  
  // 计算性能提升倍数
  let improvement_factor = divide_with_ceil(actual_performance, base_operations)
  
  assert_eq(4000, optimized_ops)
  assert_eq(480, overhead)
  assert_eq(3520, actual_performance)
  assert_eq(4, improvement_factor)
}

test "complex_business_workflow" {
  // 复杂业务工作流：订单处理
  let unit_price = 75
  let quantity = 12
  let tax_rate = 8
  let shipping_per_item = 3
  let bulk_discount_threshold = 800
  let bulk_discount_amount = 50
  
  // 计算商品总价
  let subtotal = multiply(unit_price, quantity)
  
  // 计算运费
  let total_shipping = multiply(shipping_per_item, quantity)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = add(add(subtotal, total_shipping), tax_amount)
  
  // 应用批量折扣（如果满足条件）
  let final_total = if pre_discount_total > bulk_discount_threshold {
    add(pre_discount_total, -bulk_discount_amount)
  } else {
    pre_discount_total
  }
  
  assert_eq(900, subtotal)
  assert_eq(36, total_shipping)
  assert_eq(72, tax_amount)
  assert_eq(1008, pre_discount_total)
  assert_eq(958, final_total)
}

test "edge_case_boundary_values" {
  // 边界值测试
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  
  // 测试零的运算
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, add(12345, 0))
  assert_eq(0, divide_with_ceil(0, 12345))
  assert_eq(0, divide_with_ceil(12345, 0))
  
  // 测试边界除法
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1000, divide_with_ceil(999999, 1000))
}