// Azimuth 标准测试用例集
// 包含10个高质量的标准 MoonBit 测试用例

// 导入被测试的模块
use azimuth

// 测试1: 基本算术运算功能验证
test "basic_arithmetic_operations" {
  // 测试加法运算
  azimuth.assert_eq(15, azimuth.add(7, 8))
  azimuth.assert_eq(0, azimuth.add(0, 0))
  azimuth.assert_eq(-10, azimuth.add(-3, -7))
  
  // 测试乘法运算
  azimuth.assert_eq(42, azimuth.multiply(6, 7))
  azimuth.assert_eq(0, azimuth.multiply(0, 123))
  azimuth.assert_eq(-24, azimuth.multiply(3, -8))
  
  // 测试减法运算
  azimuth.assert_eq(5, azimuth.subtract(12, 7))
  azimuth.assert_eq(-5, azimuth.subtract(7, 12))
}

// 测试2: 边界值和极值处理
test "boundary_and_extreme_values" {
  // 测试零值处理
  azimuth.assert_eq(5, azimuth.add(5, 0))
  azimuth.assert_eq(0, azimuth.multiply(5, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 10))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(5, 0)) // 除零保护
  
  // 测试大数运算
  azimuth.assert_eq(2000000, azimuth.add(1000000, 1000000))
  azimuth.assert_eq(1000000, azimuth.multiply(1000, 1000))
  azimuth.assert_eq(100001, azimuth.divide_with_ceil(1000000, 10))
}

// 测试3: 负数和混合符号运算
test "negative_and_mixed_sign_operations" {
  // 负数加法测试
  azimuth.assert_eq(-15, azimuth.add(-7, -8))
  azimuth.assert_eq(-1, azimuth.add(-8, 7))
  azimuth.assert_eq(1, azimuth.add(-7, 8))
  
  // 负数乘法测试
  azimuth.assert_eq(56, azimuth.multiply(-7, -8))
  azimuth.assert_eq(-56, azimuth.multiply(7, -8))
  azimuth.assert_eq(-56, azimuth.multiply(-7, 8))
  
  // 向上取整除法的负数测试
  azimuth.assert_eq(-2, azimuth.divide_with_ceil(-15, 8))
  azimuth.assert_eq(-2, azimuth.divide_with_ceil(15, -8))
  azimuth.assert_eq(2, azimuth.divide_with_ceil(-15, -8))
}

// 测试4: 字符串处理和国际化
test "string_processing_and_internationalization" {
  // 基本字符串测试
  azimuth.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  
  // Unicode和特殊字符测试
  azimuth.assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  azimuth.assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
  azimuth.assert_eq_string("Hello, café!", azimuth.greet("café"))
  azimuth.assert_eq_string("Hello, Москва!", azimuth.greet("Москва"))
  
  // 数字和符号字符串测试
  azimuth.assert_eq_string("Hello, 2024!", azimuth.greet("2024"))
  azimuth.assert_eq_string("Hello, @#$%!", azimuth.greet("@#$%"))
}

// 测试5: 业务逻辑场景 - 包装和配送
test "business_logic_packaging_and_shipping" {
  // 场景：计算包装箱数量
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = azimuth.divide_with_ceil(total_items, items_per_box)
  azimuth.assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = azimuth.multiply(boxes_needed, items_per_box)
  azimuth.assert_eq(48, total_capacity)
  
  // 计算剩余空间
  let remaining_space = azimuth.subtract(total_capacity, total_items)
  azimuth.assert_eq(1, remaining_space)
  
  // 场景：配送路线规划
  let total_distance = 253
  let distance_per_day = 60
  let days_needed = azimuth.divide_with_ceil(total_distance, distance_per_day)
  azimuth.assert_eq(5, days_needed)
}

// 测试6: 复杂数学计算和验证
test "complex_mathematical_calculations" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  let distributive_result1 = azimuth.multiply(a, azimuth.add(b, c))
  let distributive_result2 = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  azimuth.assert_eq(distributive_result1, distributive_result2)
  
  // 测试数学结合律
  let associative_result1 = azimuth.add(azimuth.add(a, b), c)
  let associative_result2 = azimuth.add(a, azimuth.add(b, c))
  azimuth.assert_eq(associative_result1, associative_result2)
  
  // 复杂计算链测试
  let initial = 10
  let step1 = azimuth.add(initial, azimuth.multiply(3, 4))  // 10 + 12 = 22
  let step2 = azimuth.divide_with_ceil(step1, 5)           // ceil(22/5) = 5
  let step3 = azimuth.multiply(step2, 2)                   // 5 * 2 = 10
  let final_result = azimuth.subtract(step3, 3)            // 10 - 3 = 7
  azimuth.assert_eq(7, final_result)
}

// 测试7: 错误处理和异常情况
test "error_handling_and_exception_cases" {
  // 除零错误的全面测试
  azimuth.assert_eq(0, azimuth.divide_with_ceil(10, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-10, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(2147483647, 0))
  
  // 极值运算测试
  let max_positive = 2147483647
  let min_negative = -2147483648
  
  // 测试极值加法（假设有溢出保护）
  azimuth.assert_eq(max_positive, azimuth.add(max_positive, 1))
  azimuth.assert_eq(min_negative, azimuth.add(min_negative, -1))
  
  // 测试极值乘法（假设有溢出保护）
  azimuth.assert_eq(max_positive, azimuth.multiply(max_positive, 2))
  azimuth.assert_eq(min_negative, azimuth.multiply(max_positive, -2))
}

// 测试8: 真实世界应用场景
test "real_world_application_scenarios" {
  // 场景：活动座位安排
  let participants = 87
  let chairs_per_table = 8
  let tables_needed = azimuth.divide_with_ceil(participants, chairs_per_table)
  azimuth.assert_eq(11, tables_needed)
  
  let total_seats = azimuth.multiply(tables_needed, chairs_per_table)
  azimuth.assert_eq(88, total_seats)
  
  let empty_seats = azimuth.subtract(total_seats, participants)
  azimuth.assert_eq(1, empty_seats)
  
  // 场景：项目管理时间计算
  let team_size = 5
  let tasks_per_person = 8
  let total_tasks = azimuth.multiply(team_size, tasks_per_person)
  azimuth.assert_eq(40, total_tasks)
  
  let tasks_per_sprint = 15
  let sprints_needed = azimuth.divide_with_ceil(total_tasks, tasks_per_sprint)
  azimuth.assert_eq(3, sprints_needed)
}

// 测试9: 算法和计算效率验证
test "algorithm_efficiency_validation" {
  // 场景：不同算法策略的比较
  let data_size = 1000
  
  // 线性算法模拟
  let linear_complexity = azimuth.multiply(data_size, 3)
  azimuth.assert_eq(3000, linear_complexity)
  
  // 对数算法模拟（简化）
  let logarithmic_complexity = azimuth.divide_with_ceil(data_size, 10)
  azimuth.assert_eq(100, logarithmic_complexity)
  
  // 效率比较
  let efficiency_ratio = azimuth.divide_with_ceil(linear_complexity, logarithmic_complexity)
  azimuth.assert_eq(30, efficiency_ratio)
  
  // 场景：资源分配优化
  let total_resources = 500
  let teams = 3
  let resources_per_team = azimuth.divide_with_ceil(total_resources, teams)
  azimuth.assert_eq(167, resources_per_team)
  
  let allocated_total = azimuth.multiply(resources_per_team, teams)
  azimuth.assert_eq(501, allocated_total)
}

// 测试10: 数据完整性和一致性验证
test "data_integrity_and_consistency" {
  // 场景：财务计算验证
  let unit_price = 157
  let quantity = 23
  let subtotal = azimuth.multiply(unit_price, quantity)
  azimuth.assert_eq(3611, subtotal)
  
  let tax_rate = 8  // 8%
  let tax_amount = azimuth.divide_with_ceil(azimuth.multiply(subtotal, tax_rate), 100)
  azimuth.assert_eq(289, tax_amount)
  
  let total = azimuth.add(subtotal, tax_amount)
  azimuth.assert_eq(3900, total)
  
  // 场景：库存管理验证
  let initial_stock = 150
  let sold_items = 47
  let restocked_items = 25
  let current_stock = azimuth.subtract(azimuth.add(initial_stock, restocked_items), sold_items)
  azimuth.assert_eq(128, current_stock)
  
  // 计算库存周转率
  let monthly_sales = 60
  let turnover_rate = azimuth.divide_with_ceil(current_stock, monthly_sales)
  azimuth.assert_eq(3, turnover_rate)
}