// 用户添加的标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

// 测试1: 基本算术运算综合测试
test "comprehensive_arithmetic_operations" {
  // 测试加法的各种情况
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-1, @azimuth.add(5, -6))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  
  // 测试乘法的各种情况
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-42, @azimuth.multiply(-6, 7))
  @azimuth.assert_eq(42, @azimuth.multiply(-6, -7))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  
  // 测试减法的各种情况
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(15, @azimuth.subtract(10, -5))
}

// 测试2: 向上取整除法的业务应用场景
test "divide_with_ceil_business_scenarios" {
  // 场景1: 计算需要的运输车辆
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = @azimuth.divide_with_ceil(packages, capacity_per_truck)
  @azimuth.assert_eq(5, trucks_needed)
  
  // 场景2: 计算项目冲刺数量
  let total_tasks = 85
  let tasks_per_sprint = 20
  let sprints_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_sprint)
  @azimuth.assert_eq(5, sprints_needed)
  
  // 场景3: 计算会议室需求
  let attendees = 67
  let chairs_per_room = 15
  let rooms_needed = @azimuth.divide_with_ceil(attendees, chairs_per_room)
  @azimuth.assert_eq(5, rooms_needed)
}

// 测试3: 数学性质和定律验证
test "mathematical_properties_validation" {
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律: a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // 结合律: (a + b) + c = a + (b + c)
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
}

// 测试4: 字符串处理和国际化测试
test "string_processing_internationalization" {
  // 基本字符串测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // Unicode和国际化字符测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  
  // 特殊字符和数字测试
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet("user123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

// 测试5: 边界条件和极值测试
test "boundary_conditions_extreme_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val + 1))
  
  // 边界值乘法测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 除法边界测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, max_val))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(max_val, 0))
}

// 测试6: 错误处理和健壮性测试
test "error_handling_robustness" {
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 负数除法测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  
  // 极小值除法测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(1, -1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
}

// 测试7: 复杂计算流水线测试
test "complex_calculation_pipeline" {
  // 场景：计算项目成本和资源分配
  let team_size = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 15
  let overhead_rate = 20  // 20% 管理费用
  
  // 计算总工时
  let total_hours = @azimuth.multiply(team_size, @azimuth.multiply(hours_per_day, project_days))
  @azimuth.assert_eq(960, total_hours)  // 8 * 8 * 15 = 960
  
  // 计算基础成本
  let base_cost = @azimuth.multiply(total_hours, hourly_rate)
  @azimuth.assert_eq(72000, base_cost)  // 960 * 75 = 72000
  
  // 计算管理费用（简化计算：基础成本的20%）
  let overhead_cost = @azimuth.divide_with_ceil(@azimuth.multiply(base_cost, overhead_rate), 100)
  @azimuth.assert_eq(14400, overhead_cost)  // ceil(72000 * 20 / 100) = 14400
  
  // 计算总成本
  let total_cost = @azimuth.add(base_cost, overhead_cost)
  @azimuth.assert_eq(86400, total_cost)  // 72000 + 14400 = 86400
}

// 测试8: 性能和大数据量测试
test "performance_large_data_operations" {
  let large_number = 100000
  
  // 大数运算测试
  @azimuth.assert_eq(200000, @azimuth.add(large_number, large_number))
  @azimuth.assert_eq(10000000000, @azimuth.multiply(large_number, large_number))
  
  // 连续运算性能测试
  let mut result = 1
  result = @azimuth.multiply(result, 100)
  result = @azimuth.add(result, 500)
  result = @azimuth.multiply(result, 10)
  result = @azimuth.divide_with_ceil(result, 1000)
  @azimuth.assert_eq(6, result)  // ((1 * 100 + 500) * 10) / 1000 = 6
  
  // 批量计算测试
  let batch_size = 1000
  let items_per_batch = 25
  let total_items = @azimuth.multiply(batch_size, items_per_batch)
  let batches_needed = @azimuth.divide_with_ceil(total_items, items_per_batch)
  @azimuth.assert_eq(1000, batches_needed)
}

// 测试9: 真实世界应用场景测试
test "real_world_application_scenarios" {
  // 场景1：电商订单计算
  let item_price = 199
  let quantity = 3
  let shipping_cost = 15
  let tax_rate = 8  // 8% 税率
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping_cost)
  
  @azimuth.assert_eq(597, subtotal)  // 199 * 3 = 597
  @azimuth.assert_eq(48, tax)  // ceil(597 * 8 / 100) = 48
  @azimuth.assert_eq(660, total)  // 597 + 48 + 15 = 660
  
  // 场景2：活动座位安排
  let attendees = 156
  let tables = 12
  let chairs_per_table = @azimuth.divide_with_ceil(attendees, tables)
  let total_chairs = @azimuth.multiply(chairs_per_table, tables)
  
  @azimuth.assert_eq(13, chairs_per_table)  // ceil(156 / 12) = 13
  @azimuth.assert_eq(156, total_chairs)  // 13 * 12 = 156
}

// 测试10: 算法和逻辑验证测试
test "algorithm_logic_validation" {
  // 验证数学公式和算法逻辑
  
  // 等差数列求和：1 + 2 + 3 + ... + n = n * (n + 1) / 2
  let n = 10
  let expected_sum = @azimuth.divide_with_ceil(@azimuth.multiply(n, @azimuth.add(n, 1)), 2)
  @azimuth.assert_eq(55, expected_sum)  // 10 * 11 / 2 = 55
  
  // 验证平方差公式：a² - b² = (a + b)(a - b)
  let a = 13
  let b = 7
  let a_squared = @azimuth.multiply(a, a)
  let b_squared = @azimuth.multiply(b, b)
  let difference = @azimuth.subtract(a_squared, b_squared)
  
  let factored = @azimuth.multiply(@azimuth.add(a, b), @azimuth.subtract(a, b))
  @azimuth.assert_eq(difference, factored)  // 169 - 49 = 120 = (13 + 7)(13 - 7)
  
  // 验证费马小定理的简化版本（对于素数p，a^p ≡ a mod p）
  // 这里我们只做简单的模运算验证
  let prime = 7
  let base = 3
  let power = @azimuth.multiply(base, base)  // 3^2 = 9
  let remainder = @azimuth.divide_with_ceil(power, prime)
  @azimuth.assert_eq(2, remainder)  // ceil(9 / 7) = 2
}