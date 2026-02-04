// 综合测试用例 - Azimuth 库的新增测试
// 使用标准 MoonBit 测试语法

test "add_function_comprehensive" {
  // 全面测试加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 10))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 2147483647))
  @azimuth.assert_eq(-2147483647, @azimuth.multiply(-1, 2147483647))
}

test "subtract_function_validation" {
  // 测试减法函数
  @azimuth.assert_eq(5, @azimuth.subtract(8, 3))
  @azimuth.assert_eq(-5, @azimuth.subtract(3, 8))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
  @azimuth.assert_eq(10, @azimuth.subtract(0, -10))
  @azimuth.assert_eq(-10, @azimuth.subtract(-10, 0))
  @azimuth.assert_eq(20, @azimuth.subtract(-5, -25))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(1000000, 3))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, María!", @azimuth.greet("María"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
}

test "complex_business_calculations" {
  // 复杂业务计算测试
  let base_price = 120
  let quantity = 15
  let tax_rate = 8
  let shipping_cost = 25
  let discount = 50
  
  // 计算小计
  let subtotal = @azimuth.multiply(base_price, quantity)
  @azimuth.assert_eq(1800, subtotal)
  
  // 计算税费
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  @azimuth.assert_eq(144, tax)
  
  // 计算总价
  let total_before_discount = @azimuth.add(@azimuth.add(subtotal, tax), shipping_cost)
  let final_total = @azimuth.add(total_before_discount, -discount)
  
  @azimuth.assert_eq(1969, final_total)
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律: a * (b + c) = a * b + a * c
  let c = 5
  let distributive_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distributive_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distributive_left, distributive_right)
  
  // 零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  let initial_stock = 500
  let daily_sales = 25
  let restock_day = 5
  let restock_amount = 200
  let days_to_simulate = 10
  
  // 前5天的销售
  let sold_first_period = @azimuth.multiply(daily_sales, restock_day)
  let stock_after_first_period = @azimuth.subtract(initial_stock, sold_first_period)
  
  // 补货
  let stock_after_restock = @azimuth.add(stock_after_first_period, restock_amount)
  
  // 后5天的销售
  let remaining_days = @azimuth.subtract(days_to_simulate, restock_day)
  let sold_second_period = @azimuth.multiply(daily_sales, remaining_days)
  let final_stock = @azimuth.subtract(stock_after_restock, sold_second_period)
  
  @azimuth.assert_eq(125, sold_first_period)
  @azimuth.assert_eq(375, stock_after_first_period)
  @azimuth.assert_eq(575, stock_after_restock)
  @azimuth.assert_eq(125, sold_second_period)
  @azimuth.assert_eq(450, final_stock)
}

test "packaging_optimization_scenario" {
  // 包装优化场景测试
  let products = [
    ("小件", 120, 8),   // 产品名, 数量, 每箱容量
    ("中件", 85, 6),
    ("大件", 45, 3)
  ]
  
  let total_boxes = 0
  let total_wasted_space = 0
  
  // 计算每种产品需要的箱数
  let small_boxes = @azimuth.divide_with_ceil(120, 8)
  let medium_boxes = @azimuth.divide_with_ceil(85, 6)
  let large_boxes = @azimuth.divide_with_ceil(45, 3)
  
  let total_boxes_needed = @azimuth.add(@azimuth.add(small_boxes, medium_boxes), large_boxes)
  
  // 计算浪费的空间
  let small_waste = @azimuth.subtract(@azimuth.multiply(small_boxes, 8), 120)
  let medium_waste = @azimuth.subtract(@azimuth.multiply(medium_boxes, 6), 85)
  let large_waste = @azimuth.subtract(@azimuth.multiply(large_boxes, 3), 45)
  
  let total_wasted = @azimuth.add(@azimuth.add(small_waste, medium_waste), large_waste)
  
  @azimuth.assert_eq(15, small_boxes)
  @azimuth.assert_eq(15, medium_boxes)
  @azimuth.assert_eq(15, large_boxes)
  @azimuth.assert_eq(45, total_boxes_needed)
  @azimuth.assert_eq(0, small_waste)
  @azimuth.assert_eq(5, medium_waste)
  @azimuth.assert_eq(0, large_waste)
  @azimuth.assert_eq(5, total_wasted)
}