// Azimuth 增强标准测试用例
// 包含8个高质量的标准 MoonBit 测试用例

// 导入被测试的模块
use azimuth

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 42
  
  // 加法恒等式
  azimuth.assert_eq(x, azimuth.add(x, 0))
  azimuth.assert_eq(x, azimuth.add(0, x))
  
  // 乘法恒等式
  azimuth.assert_eq(x, azimuth.multiply(x, 1))
  azimuth.assert_eq(x, azimuth.multiply(1, x))
  azimuth.assert_eq(0, azimuth.multiply(x, 0))
  
  // 减法恒等式
  azimuth.assert_eq(0, azimuth.subtract(x, x))
  azimuth.assert_eq(x, azimuth.subtract(x, 0))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  // 正数除法
  azimuth.assert_eq(4, azimuth.divide_with_ceil(10, 3))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(9, 3))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 100))
  
  // 负数处理
  azimuth.assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  azimuth.assert_eq(-4, azimuth.divide_with_ceil(10, -3))
  azimuth.assert_eq(4, azimuth.divide_with_ceil(-10, -3))
  
  // 边界情况
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 5))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(10, 0))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 1))
}

test "greet_function_international_support" {
  // 测试问候函数的国际化支持
  azimuth.assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  azimuth.assert_eq_string("Hello, Москва!", azimuth.greet("Москва"))
  azimuth.assert_eq_string("Hello, café!", azimuth.greet("café"))
  azimuth.assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
  azimuth.assert_eq_string("Hello, ñoño!", azimuth.greet("ñoño"))
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
}

test "complex_business_calculation" {
  // 复杂业务计算场景：项目资源规划
  let team_size = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 12
  let budget_per_sprint = 15000
  
  // 计算总工时
  let total_hours = azimuth.multiply(team_size, azimuth.multiply(hours_per_day, project_days))
  
  // 计算总成本
  let total_cost = azimuth.multiply(total_hours, hourly_rate)
  
  // 计算需要的冲刺数
  let sprints_needed = azimuth.divide_with_ceil(total_cost, budget_per_sprint)
  
  // 验证计算结果
  azimuth.assert_eq(768, total_hours)  // 8 * 8 * 12 = 768
  azimuth.assert_eq(57600, total_cost)  // 768 * 75 = 57600
  azimuth.assert_eq(4, sprints_needed)  // ceil(57600/15000) = 4
}

test "arithmetic_commutative_and_associative" {
  // 测试算术交换律和结合律
  let a = 15
  let b = 25
  let c = 10
  
  // 交换律测试
  azimuth.assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  azimuth.assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 结合律测试
  azimuth.assert_eq(azimuth.add(azimuth.add(a, b), c), azimuth.add(a, azimuth.add(b, c)))
  azimuth.assert_eq(azimuth.multiply(azimuth.multiply(a, b), c), azimuth.multiply(a, azimuth.multiply(b, c)))
  
  // 分配律测试
  azimuth.assert_eq(azimuth.multiply(a, azimuth.add(b, c)), azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c)))
}

test "error_handling_and_boundary_values" {
  // 错误处理和边界值测试
  // 除零错误的全面测试
  azimuth.assert_eq(0, azimuth.divide_with_ceil(1, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-1, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(999999, 0))
  
  // 极值测试
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 1))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(-1, -1))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 1))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, -1))
  
  // 大数测试
  azimuth.assert_eq(1000000, azimuth.multiply(1000, 1000))
  azimuth.assert_eq(2000000, azimuth.add(1000000, 1000000))
}

test "real_world_inventory_management" {
  // 真实世界库存管理场景
  let product_a_quantity = 156
  let product_b_quantity = 89
  let product_c_quantity = 234
  let box_capacity = 25
  
  // 计算每种产品需要的箱子数
  let boxes_a = azimuth.divide_with_ceil(product_a_quantity, box_capacity)
  let boxes_b = azimuth.divide_with_ceil(product_b_quantity, box_capacity)
  let boxes_c = azimuth.divide_with_ceil(product_c_quantity, box_capacity)
  
  // 计算总箱子数
  let total_boxes = azimuth.add(azimuth.add(boxes_a, boxes_b), boxes_c)
  
  // 计算总存储容量
  let total_capacity = azimuth.multiply(total_boxes, box_capacity)
  
  // 计算总产品数
  let total_products = azimuth.add(azimuth.add(product_a_quantity, product_b_quantity), product_c_quantity)
  
  // 验证结果
  azimuth.assert_eq(7, boxes_a)    // ceil(156/25) = 7
  azimuth.assert_eq(4, boxes_b)    // ceil(89/25) = 4
  azimuth.assert_eq(10, boxes_c)   // ceil(234/25) = 10
  azimuth.assert_eq(21, total_boxes)
  azimuth.assert_eq(525, total_capacity)  // 21 * 25 = 525
  azimuth.assert_eq(479, total_products)
  
  // 验证容量足够
  azimuth.assert_true(total_capacity >= total_products)
}

test "string_processing_edge_cases" {
  // 字符串处理的边缘情况测试
  // 特殊字符和格式测试
  azimuth.assert_eq_string("Hello, single!", azimuth.greet("single"))
  azimuth.assert_eq_string("Hello, space test!", azimuth.greet("space test"))
  azimuth.assert_eq_string("Hello, 12345!", azimuth.greet("12345"))
  azimuth.assert_eq_string("Hello, !@#$%^!", azimuth.greet("!@#$%^"))
  azimuth.assert_eq_string("Hello, mixedCASE!", azimuth.greet("mixedCASE"))
  azimuth.assert_eq_string("Hello, leading space!", azimuth.greet(" leading space"))
  azimuth.assert_eq_string("Hello, trailing space !", azimuth.greet("trailing space "))
  azimuth.assert_eq_string("Hello, multiple   spaces!", azimuth.greet("multiple   spaces"))
  azimuth.assert_eq_string("Hello, \n\t\r!", azimuth.greet("\n\t\r"))
  azimuth.assert_eq_string("Hello, \"quotes\"!", azimuth.greet("\"quotes\""))
}