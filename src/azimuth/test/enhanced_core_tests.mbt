// 增强的核心测试用例
// 专注于测试 azimuth 库的核心功能和边界情况

test "divide_with_ceil_precision_edge_cases" {
  // 测试向上取整除法的精度边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(9999, 100))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
  @azimuth.assert_eq_string("Hello, 🌍🌎🌏!", @azimuth.greet("🌍🌎🌏"))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let x = 10
  let y = 20
  let z = 5
  
  // 测试表达式：(x + y) * z - x / y
  let result = @azimuth.add(@azimuth.multiply(@azimuth.add(x, y), z), -@azimuth.divide_with_ceil(x, y))
  @azimuth.assert_eq(149, result)
  
  // 测试嵌套表达式
  let nested = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.add(x, @azimuth.multiply(y, z)), 2), 3)
  @azimuth.assert_eq(67, nested)
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 测试除数为零的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "extreme_boundary_values" {
  // 测试极端边界值
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
}

test "real_world_financial_calculation" {
  // 测试实际世界的金融计算场景
  let principal = 1000
  let rate_percent = 5
  
  // 简化的复利计算（每年计算一次）
  let amount_after_year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  let amount_after_year2 = @azimuth.add(amount_after_year1, @azimuth.divide_with_ceil(@azimuth.multiply(amount_after_year1, rate_percent), 100))
  let amount_after_year3 = @azimuth.add(amount_after_year2, @azimuth.divide_with_ceil(@azimuth.multiply(amount_after_year2, rate_percent), 100))
  
  @azimuth.assert_eq(1158, amount_after_year3)
}

test "inventory_management_scenario" {
  // 测试库存管理场景
  let items_per_box = 24
  let total_items_small = 100
  let total_items_medium = 250
  let total_items_large = 1000
  
  let boxes_small = @azimuth.divide_with_ceil(total_items_small, items_per_box)
  let boxes_medium = @azimuth.divide_with_ceil(total_items_medium, items_per_box)
  let boxes_large = @azimuth.divide_with_ceil(total_items_large, items_per_box)
  
  let total_boxes = @azimuth.add(@azimuth.add(boxes_small, boxes_medium), boxes_large)
  
  @azimuth.assert_eq(5, boxes_small)
  @azimuth.assert_eq(11, boxes_medium)
  @azimuth.assert_eq(42, boxes_large)
  @azimuth.assert_eq(58, total_boxes)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  let c = 5
  
  // 验证加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 验证乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证分配律 a * (b + c) = a * b + a * c
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "string_processing_with_greet" {
  // 测试字符串处理与问候函数结合
  let first_name = "John"
  let last_name = "Doe"
  let full_name = first_name + " " + last_name
  let greeting = @azimuth.greet(full_name)
  
  @azimuth.assert_eq_string("Hello, John Doe!", greeting)
  
  // 测试特殊字符处理
  let special_name = "Alice & Bob"
  let special_greeting = @azimuth.greet(special_name)
  @azimuth.assert_eq_string("Hello, Alice & Bob!", special_greeting)
}

test "complex_business_workflow" {
  // 测试复杂的业务工作流程
  let product_cost = 120
  let quantity = 15
  let tax_rate = 8
  let shipping_cost_per_item = 5
  let discount_threshold = 1000
  let discount_amount = 50
  
  // 步骤1: 计算商品总价
  let subtotal = @azimuth.multiply(product_cost, quantity)
  
  // 步骤2: 计算运费
  let total_shipping = @azimuth.multiply(shipping_cost_per_item, quantity)
  
  // 步骤3: 计算税费
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 步骤4: 计算折扣前总价
  let pre_discount_total = @azimuth.add(@azimuth.add(subtotal, total_shipping), tax_amount)
  
  // 步骤5: 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    @azimuth.add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  @azimuth.assert_eq(1800, subtotal)
  @azimuth.assert_eq(75, total_shipping)
  @azimuth.assert_eq(144, tax_amount)
  @azimuth.assert_eq(2019, pre_discount_total)
  @azimuth.assert_eq(1969, final_total)
}