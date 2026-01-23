// 标准 MoonBit 测试用例
// 覆盖 azimuth 库的核心功能和边界情况

test "add_function_performance_test" {
  // 测试加法函数的性能表现
  let base = 1000
  let result = @azimuth.add(@azimuth.add(@azimuth.add(base, base), base), base)
  @azimuth.assert_eq(4000, result)
  
  // 测试连续加法
  let mut sum = 0
  sum = @azimuth.add(sum, 100)
  sum = @azimuth.add(sum, 200)
  sum = @azimuth.add(sum, 300)
  sum = @azimuth.add(sum, 400)
  @azimuth.assert_eq(1000, sum)
}

test "multiply_function_large_numbers" {
  // 测试乘法函数的大数处理
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(12321, @azimuth.multiply(111, 111))
  @azimuth.assert_eq(998001, @azimuth.multiply(999, 999))
  
  // 测试负数大数
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1000, 1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
}

test "greet_function_multilingual_support" {
  // 测试多语言支持
  @azimuth.assert_eq_string("Hello, English!", @azimuth.greet("English"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 日本語!", @azimuth.greet("日本語"))
  @azimuth.assert_eq_string("Hello, Español!", @azimuth.greet("Español"))
  @azimuth.assert_eq_string("Hello, Français!", @azimuth.greet("Français"))
}

test "divide_with_ceil_business_logic" {
  // 测试业务逻辑场景 - 计算需要的容器数量
  let items = 47
  let container_capacity = 10
  let containers_needed = @azimuth.divide_with_ceil(items, container_capacity)
  @azimuth.assert_eq(5, containers_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(containers_needed, container_capacity)
  @azimuth.assert_eq(50, total_capacity)
  
  // 测试不同场景
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(25, 10))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算项目成本
  let hourly_rate = 50
  let hours_per_day = 8
  let team_size = 5
  let project_days = 10
  
  // 计算总工时
  let total_hours = @azimuth.multiply(hours_per_day, team_size)
  let total_project_hours = @azimuth.multiply(total_hours, project_days)
  
  // 计算总成本
  let total_cost = @azimuth.multiply(total_project_hours, hourly_rate)
  
  @azimuth.assert_eq(40, total_hours)  // 8 * 5 = 40
  @azimuth.assert_eq(400, total_project_hours)  // 40 * 10 = 400
  @azimuth.assert_eq(20000, total_cost)  // 400 * 50 = 20000
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let x = 15
  let y = 25
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  let left = @azimuth.multiply(x, @azimuth.add(y, 5))
  let right = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 5))
  @azimuth.assert_eq(left, right)
  
  // 结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), 5), @azimuth.add(x, @azimuth.add(y, 5)))
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(x, y), 5), @azimuth.multiply(x, @azimuth.multiply(y, 5)))
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  // 除零情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  // 测试各种字符串类型
  @azimuth.assert_eq_string("Hello, single!", @azimuth.greet("single"))
  @azimuth.assert_eq_string("Hello, space test!", @azimuth.greet("space test"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, !@#$%^!", @azimuth.greet("!@#$%^"))
  @azimuth.assert_eq_string("Hello, mixedCASE!", @azimuth.greet("mixedCASE"))
  
  // 测试边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
}

test "real_world_application_scenarios" {
  // 真实世界应用场景测试
  // 场景1：计算购物车总价
  let item1 = 199
  let item2 = 299
  let item3 = 399
  let discount = -50
  let tax = 80
  
  let subtotal = @azimuth.add(@azimuth.add(item1, item2), item3)
  let after_discount = @azimuth.add(subtotal, discount)
  let total = @azimuth.add(after_discount, tax)
  
  @azimuth.assert_eq(897, total)
  
  // 场景2：计算活动安排
  let participants = 87
  let tables = 11
  let chairs_per_table = 8
  let total_chairs = @azimuth.multiply(tables, chairs_per_table)
  
  @azimuth.assert_eq(88, total_chairs)
  // 验证椅子数量大于等于参与者数量
  @azimuth.assert_eq(1, if (total_chairs >= participants) { 1 } else { 0 })
  
  // 场景3：个性化问候
  let vip_customer = "VIP Customer"
  let regular_customer = "Regular Customer"
  
  @azimuth.assert_eq_string("Hello, VIP Customer!", @azimuth.greet(vip_customer))
  @azimuth.assert_eq_string("Hello, Regular Customer!", @azimuth.greet(regular_customer))
}

test "advanced_arithmetic_operations" {
  // 高级算术运算测试
  // 测试连续运算
  let result1 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = @azimuth.multiply(@azimuth.add(10, 5), @azimuth.add(3, 2))
  @azimuth.assert_eq(75, result2)  // (10+5)*(3+2) = 15*5 = 75
  
  // 测试嵌套运算
  let result3 = @azimuth.add(@azimuth.multiply(@azimuth.add(1, 2), @azimuth.add(3, 4)), 5)
  @azimuth.assert_eq(20, result3)  // (1+2)*(3+4) + 5 = 3*7 + 5 = 21 + 5 = 26
  
  // 测试负数运算
  let result4 = @azimuth.add(@azimuth.multiply(-2, 3), @azimuth.multiply(4, -5))
  @azimuth.assert_eq(-26, result4)  // -2*3 + 4*(-5) = -6 + (-20) = -26
}