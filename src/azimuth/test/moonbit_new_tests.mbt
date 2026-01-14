// 新增的 MoonBit 测试用例
// 包含各种实用场景和边界条件测试

test "complex_arithmetic_sequence" {
  // 测试复杂数列计算：计算等差数列前n项和
  // 公式：Sn = n(a1 + an) / 2
  let n = 10
  let first_term = 3
  let last_term = 30
  
  let sum = @azimuth.multiply(n, @azimuth.add(first_term, last_term)) / 2
  @azimuth.assert_eq(165, sum)  // 10 × (3 + 30) / 2 = 165
}

test "string_pattern_validation" {
  // 测试字符串模式验证
  // 测试邮箱格式（简化验证）
  let email_simple = "user@example.com"
  let email_result = @azimuth.greet(email_simple)
  @azimuth.assert_eq_string("Hello, user@example.com!", email_result)
  
  // 测试包含特殊字符的ID
  let user_id = "user_2023@project#1"
  let id_result = @azimuth.greet(user_id)
  @azimuth.assert_eq_string("Hello, user_2023@project#1!", id_result)
  
  // 测试数字字符串
  let numeric_string = "1234567890"
  let numeric_result = @azimuth.greet(numeric_string)
  @azimuth.assert_eq_string("Hello, 1234567890!", numeric_result)
}

test "mathematical_limits_stress" {
  // 测试数学极限压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续运算的稳定性
  let result1 = @azimuth.add(@azimuth.add(max_val, -1000000), 1000000)
  @azimuth.assert_eq(max_val, result1)
  
  let result2 = @azimuth.add(@azimuth.add(min_val, 1000000), -1000000)
  @azimuth.assert_eq(min_val, result2)
  
  // 测试乘法分配律在边界条件下的表现
  let result3 = @azimuth.multiply(@azimuth.add(max_val, min_val), 100)
  @azimuth.assert_eq(-100, result3)  // (-1) × 100 = -100
}

test "business_calculation_scenario" {
  // 测试实际业务计算场景：利润计算
  let revenue = 50000
  let cost = 35000
  let tax_rate = 20  // 20%
  
  // 计算利润
  let profit = @azimuth.add(revenue, -cost)
  
  // 计算税费
  let tax = @azimuth.multiply(profit, tax_rate) / 100
  
  // 计算净利润
  let net_profit = @azimuth.add(profit, -tax)
  
  @azimuth.assert_eq(12000, net_profit)  // (50000 - 35000) × (1 - 0.2) = 12000
}

test "data_structure_simulation" {
  // 测试数据结构模拟：栈操作
  // 使用整数模拟栈的push和pop操作
  let initial_stack = 0
  let push1 = 100
  let push2 = 200
  let push3 = 300
  
  // 模拟栈顶位置
  let stack_top = @azimuth.add(@azimuth.add(@azimuth.add(initial_stack, push1), push2), push3)
  
  // 模拟pop操作（减去最后push的值）
  let after_pop = @azimuth.add(stack_top, -push3)
  
  @azimuth.assert_eq(300, after_pop)  // 100 + 200 = 300
}

test "algorithm_complexity_test" {
  // 测试算法复杂度：模拟O(n²)操作
  let n = 100
  
  // 模拟嵌套循环的总操作次数：n × n
  let total_operations = @azimuth.multiply(n, n)
  
  // 模拟优化后的操作次数：n × log₂(n) ≈ n × 7（对于n=100）
  let optimized_operations = @azimuth.multiply(n, 7)
  
  // 验证优化效果
  let improvement_ratio = total_operations / optimized_operations
  @azimuth.assert_true(improvement_ratio > 10)  // 优化后应该至少快10倍
}

test "scientific_computation" {
  // 测试科学计算场景：物理公式计算
  // 自由落体距离：s = 0.5 × g × t²
  // 简化为整数计算：s = g × t² / 2
  let gravity = 98  // 9.8 m/s² 放大10倍
  let time = 5      // 5秒
  
  let distance = @azimuth.multiply(gravity, @azimuth.multiply(time, time)) / 20  // 除以20因为gravity放大了10倍且公式有0.5
  @azimuth.assert_eq(122, distance)  // 98 × 25 / 20 = 122.5 ≈ 122（整数除法）
}

test "security_validation" {
  // 测试安全性验证：PIN码验证（简化版）
  let correct_pin = 1234
  let user_pin = 1234
  let max_attempts = 3
  
  // 模拟验证逻辑
  let is_valid = if (user_pin == correct_pin) { 1 } else { 0 }
  let attempts_remaining = @azimuth.add(max_attempts, @azimuth.multiply(is_valid, -1))
  
  @azimuth.assert_eq(3, attempts_remaining)  // 验证成功，尝试次数不变
}

test "performance_benchmark" {
  // 性能基准测试：大量数据处理的模拟
  let data_size = 1000
  let operation_cost = 5
  
  // 计算总操作成本
  let total_cost = @azimuth.multiply(data_size, operation_cost)
  
  // 模拟优化后的成本（减少30%）
  let optimization_factor = 70  // 70%的原始成本
  let optimized_cost = @azimuth.multiply(total_cost, optimization_factor) / 100
  
  // 验证优化效果
  let cost_reduction = @azimuth.add(total_cost, -optimized_cost)
  @azimuth.assert_eq(1500, cost_reduction)  // 5000 - 3500 = 1500
}

test "edge_case_comprehensive" {
  // 综合边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  
  // 测试零的运算特性
  @azimuth.assert_eq(zero, @azimuth.multiply(zero, max_val))
  @azimuth.assert_eq(zero, @azimuth.multiply(zero, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(zero, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(zero, min_val))
  
  // 测试极值运算的组合
  let complex_calculation = @azimuth.add(@azimuth.multiply(@azimuth.add(max_val, min_val), 100), @azimuth.multiply(100, @azimuth.add(max_val, min_val)))
  @azimuth.assert_eq(-200, complex_calculation)  // (-1 × 100) + (100 × -1) = -200
}