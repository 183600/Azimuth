// Azimuth 项目额外测试用例
// 使用标准 MoonBit 测试语法，包含9个高质量测试用例

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  @azimuth.assert_eq(34, @azimuth.divide_with_ceil(100, 3))
  @azimuth.assert_eq(112, @azimuth.divide_with_ceil(1000, 9))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(999, 1000))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1000))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1000))
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
  
  // (a + b) * c = a * c + b * c
  @azimuth.assert_eq(
    @azimuth.multiply(@azimuth.add(a, b), c),
    @azimuth.add(@azimuth.multiply(a, c), @azimuth.multiply(b, c))
  )
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 场景：计算产品制造的成本和时间
  let products_needed = 150
  let machines_available = 8
  let production_per_machine_per_day = 5
  let days_per_week = 5
  let cost_per_machine_per_day = 200
  
  // 计算每日总产量
  let daily_production = @azimuth.multiply(machines_available, production_per_machine_per_day)
  @azimuth.assert_eq(40, daily_production)  // 8 * 5 = 40
  
  // 计算需要的生产天数
  let days_needed = @azimuth.divide_with_ceil(products_needed, daily_production)
  @azimuth.assert_eq(4, days_needed)  // ceil(150/40) = 4
  
  // 计算需要的周数
  let weeks_needed = @azimuth.divide_with_ceil(days_needed, days_per_week)
  @azimuth.assert_eq(1, weeks_needed)  // ceil(4/5) = 1
  
  // 计算总成本
  let total_cost = @azimuth.multiply(days_needed, @azimuth.multiply(machines_available, cost_per_machine_per_day))
  @azimuth.assert_eq(6400, total_cost)  // 4 * 8 * 200 = 6400
}

test "string_edge_cases" {
  // 字符串边缘情况测试
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, \n!", @azimuth.greet("\n"))
  @azimuth.assert_eq_string("Hello, \t!", @azimuth.greet("\t"))
  @azimuth.assert_eq_string("Hello, null!", @azimuth.greet("null"))
  @azimuth.assert_eq_string("Hello, undefined!", @azimuth.greet("undefined"))
  @azimuth.assert_eq_string("Hello, 1234567890!", @azimuth.greet("1234567890"))
}

test "number_system_boundary" {
  // 数字系统边界测试
  // 测试大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(1000000, 3))
  
  // 测试负数边界
  @azimuth.assert_eq(0, @azimuth.add(-1000000, 1000000))
  @azimuth.assert_eq(-100000000, @azimuth.multiply(-10000, 10000))
  @azimuth.assert_eq(-333333, @azimuth.divide_with_ceil(-1000000, 3))
}

test "algorithm_efficiency_simulation" {
  // 算法效率模拟测试
  // 模拟不同算法的时间复杂度
  let input_size = 1000
  
  // O(1) 算法
  let constant_time = 5
  
  // O(log n) 算法（简化模拟）
  let logarithmic_time = @azimuth.divide_with_ceil(input_size, 100)
  
  // O(n) 算法
  let linear_time = @azimuth.multiply(input_size, 2)
  
  // O(n^2) 算法（简化模拟）
  let quadratic_time = @azimuth.multiply(input_size, @azimuth.divide_with_ceil(input_size, 100))
  
  // 验证复杂度特征
  @azimuth.assert_eq(5, constant_time)
  @azimuth.assert_eq(10, logarithmic_time)  // ceil(1000/100) = 10
  @azimuth.assert_eq(2000, linear_time)     // 1000 * 2 = 2000
  @azimuth.assert_eq(10000, quadratic_time) // 1000 * 10 = 10000
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  // 场景：多产品库存优化
  let product_a_stock = 120
  let product_b_stock = 85
  let product_c_stock = 200
  
  let box_capacity_a = 15
  let box_capacity_b = 12
  let box_capacity_c = 25
  
  // 计算各产品需要的箱子数量
  let boxes_a = @azimuth.divide_with_ceil(product_a_stock, box_capacity_a)
  let boxes_b = @azimuth.divide_with_ceil(product_b_stock, box_capacity_b)
  let boxes_c = @azimuth.divide_with_ceil(product_c_stock, box_capacity_c)
  
  // 计算总箱子数
  let total_boxes = @azimuth.add(@azimuth.add(boxes_a, boxes_b), boxes_c)
  
  // 计算各产品的剩余空间
  let space_a = @azimuth.subtract(@azimuth.multiply(boxes_a, box_capacity_a), product_a_stock)
  let space_b = @azimuth.subtract(@azimuth.multiply(boxes_b, box_capacity_b), product_b_stock)
  let space_c = @azimuth.subtract(@azimuth.multiply(boxes_c, box_capacity_c), product_c_stock)
  
  // 计算总剩余空间
  let total_wasted_space = @azimuth.add(@azimuth.add(space_a, space_b), space_c)
  
  // 验证结果
  @azimuth.assert_eq(8, boxes_a)         // ceil(120/15) = 8
  @azimuth.assert_eq(8, boxes_b)         // ceil(85/12) = 8
  @azimuth.assert_eq(8, boxes_c)         // ceil(200/25) = 8
  @azimuth.assert_eq(24, total_boxes)    // 8 + 8 + 8 = 24
  @azimuth.assert_eq(0, space_a)         // 8*15 - 120 = 0
  @azimuth.assert_eq(11, space_b)        // 8*12 - 85 = 11
  @azimuth.assert_eq(0, space_c)         // 8*25 - 200 = 0
  @azimuth.assert_eq(11, total_wasted_space)  // 0 + 11 + 0 = 11
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零错误的全面测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1000000, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1000000, 0))
  
  // 极值运算测试
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(2147483647, @azimuth.subtract(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 1))
}

test "mathematical_series_calculation" {
  // 数学级数计算测试
  // 计算等差数列和
  let first_term = 5
  let common_difference = 3
  let number_of_terms = 10
  
  // 使用公式计算：S = n/2 * (2a + (n-1)d)
  let sum_arithmetic = @azimuth.divide_with_ceil(
    @azimuth.multiply(number_of_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  // 计算等比数列和（简化版本）
  let first_term_geo = 2
  let common_ratio = 3
  let number_of_terms_geo = 5
  
  // 使用迭代方式计算等比数列和
  let term1 = first_term_geo
  let term2 = @azimuth.multiply(term1, common_ratio)
  let term3 = @azimuth.multiply(term2, common_ratio)
  let term4 = @azimuth.multiply(term3, common_ratio)
  let term5 = @azimuth.multiply(term4, common_ratio)
  let sum_geometric = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  
  // 验证结果
  @azimuth.assert_eq(185, sum_arithmetic)  // 10/2 * (2*5 + 9*3) = 5 * (10 + 27) = 5 * 37 = 185
  @azimuth.assert_eq(242, sum_geometric)   // 2 + 6 + 18 + 54 + 162 = 242
}