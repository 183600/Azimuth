// Azimuth 标准测试套件
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "add_function_comprehensive" {
  // 加法函数综合测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
  @azimuth.assert_eq(-30, @azimuth.multiply(-6, 5))
  @azimuth.assert_eq(30, @azimuth.multiply(-6, -5))
  @azimuth.assert_eq(10000, @azimuth.multiply(100, 100))
}

test "divide_with_ceil_business_logic" {
  // 向上取整除法业务逻辑测试
  // 场景：计算需要的运输车辆
  let total_weight = 1250
  let vehicle_capacity = 300
  let vehicles_needed = @azimuth.divide_with_ceil(total_weight, vehicle_capacity)
  @azimuth.assert_eq(5, vehicles_needed)
  
  // 验证总运输能力
  let total_capacity = @azimuth.multiply(vehicles_needed, vehicle_capacity)
  @azimuth.assert_eq(1500, total_capacity)
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 中国!", @azimuth.greet("中国"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, école!", @azimuth.greet("école"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目成本估算
  let team_size = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 12
  
  // 计算总工时
  let total_hours = @azimuth.multiply(team_size, @azimuth.multiply(hours_per_day, project_days))
  
  // 计算总成本
  let total_cost = @azimuth.multiply(total_hours, hourly_rate)
  
  // 计算付款期数（每期不超过5000）
  let payment_limit = 5000
  let payment_periods = @azimuth.divide_with_ceil(total_cost, payment_limit)
  
  @azimuth.assert_eq(768, total_hours)  // 8 * 8 * 12 = 768
  @azimuth.assert_eq(57600, total_cost)  // 768 * 75 = 57600
  @azimuth.assert_eq(12, payment_periods)  // ceil(57600/5000) = 12
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 15
  let b = 7
  let c = 3
  
  // 加法交换律和结合律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法交换律和结合律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "negative_number_operations" {
  // 负数运算测试
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(0, @azimuth.add(-10, 10))
  @azimuth.assert_eq(5, @azimuth.add(-8, 13))
  
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(-20, @azimuth.multiply(-4, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(-4, 0))
  
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  let total_products = 487
  let box_capacity = 24
  let warehouse_sections = 8
  let sections_per_box = @azimuth.divide_with_ceil(box_capacity, warehouse_sections)
  
  // 计算需要的箱子数量
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  @azimuth.assert_eq(21, boxes_needed)
  
  // 计算总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(504, total_capacity)
  
  // 计算剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(17, remaining_space)
  
  // 验证每个仓库部分的产品数
  let products_per_section = @azimuth.divide_with_ceil(total_products, @azimuth.multiply(boxes_needed, warehouse_sections))
  @azimuth.assert_eq(3, products_per_section)
}