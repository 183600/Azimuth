// Azimuth 简洁测试用例 - 标准MoonBit测试语法
test "add_function_identity" {
  // 测试加法恒等式
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
}

test "multiply_function_identity" {
  // 测试乘法恒等式
  @azimuth.assert_eq(25, @azimuth.multiply(25, 1))
  @azimuth.assert_eq(25, @azimuth.multiply(1, 25))
  @azimuth.assert_eq(0, @azimuth.multiply(25, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 25))
  @azimuth.assert_eq(-25, @azimuth.multiply(25, -1))
  @azimuth.assert_eq(-25, @azimuth.multiply(-1, 25))
}

test "greet_function_multilingual" {
  // 测试多语言问候功能
  @azimuth.assert_eq_string("Hello, English!", @azimuth.greet("English"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 日本語!", @azimuth.greet("日本語"))
  @azimuth.assert_eq_string("Hello, Español!", @azimuth.greet("Español"))
}

test "divide_with_ceil_practical" {
  // 测试实际应用场景的向上取整除法
  // 计算需要的车辆数量
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(25, 10))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(47, 10))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 10))
  
  // 除零保护
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
}

test "complex_business_calculation" {
  // 测试复杂业务计算
  // 场景：计算项目总成本
  let labor_cost = @azimuth.multiply(8, 50)  // 8人 * 50元/小时
  let material_cost = 1200
  let total_cost = @azimuth.add(labor_cost, material_cost)
  @azimuth.assert_eq(1600, total_cost)
  
  // 计算需要的支付期数（每期不超过500元）
  let payment_periods = @azimuth.divide_with_ceil(total_cost, 500)
  @azimuth.assert_eq(4, payment_periods)
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 15
  let b = 27
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(4, @azimuth.add(-10, 14))
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-21, @azimuth.multiply(3, -7))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "sequential_operations" {
  // 测试连续运算
  let initial = 10
  let step1 = @azimuth.add(initial, 5)        // 15
  let step2 = @azimuth.multiply(step1, 2)     // 30
  let step3 = @azimuth.divide_with_ceil(step2, 8)  // 4
  let step4 = @azimuth.add(step3, 3)          // 7
  @azimuth.assert_eq(7, step4)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "resource_allocation_scenario" {
  // 测试资源分配场景
  let total_resources = 100
  let teams = 3
  let resources_per_team = @azimuth.divide_with_ceil(total_resources, teams)
  @azimuth.assert_eq(34, resources_per_team)  // ceil(100/3) = 34
  
  // 验证总分配量
  let total_allocated = @azimuth.multiply(resources_per_team, teams)
  @azimuth.assert_eq(102, total_allocated)
}