// 标准 MoonBit 质量测试用例
// 为 azimuth 库提供全面的功能测试覆盖

test "add_function_comprehensive" {
  // 全面测试加法函数的各种情况
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  @azimuth.assert_eq(4, @azimuth.add(-7, 11))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-5, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 12345))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 12345))
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边缘情况
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(10, @azimuth.subtract(5, -5))
  @azimuth.assert_eq(-10, @azimuth.subtract(-5, 5))
  @azimuth.assert_eq(0, @azimuth.subtract(0, 0))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "greet_function_international" {
  // 测试问候函数的国际字符支持
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_commutative_laws" {
  // 验证数学交换律
  let a = 15
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算场景
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = @azimuth.divide_with_ceil(items, capacity_per_box)
  
  @azimuth.assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, capacity_per_box)
  @azimuth.assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, items)
  @azimuth.assert_eq(1, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 10
  let multiplier = 3
  let divisor = 4
  let addition = 5
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = @azimuth.multiply(base_value, multiplier)
  let step2 = @azimuth.divide_with_ceil(step1, divisor)
  let result = @azimuth.add(step2, addition)
  
  @azimuth.assert_eq(13, result)  // ceil(10*3/4) + 5 = 8 + 5 = 13
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let team_members = 6
  
  // 使用向上取整确保覆盖所有情况
  let max_share = @azimuth.divide_with_ceil(total_budget, team_members)
  @azimuth.assert_eq(1667, max_share)  // ceil(10000/6) = 1667
  
  // 验证总分配
  let total_allocated = @azimuth.multiply(max_share, team_members)
  @azimuth.assert_eq(10002, total_allocated)
}

test "error_handling_and_boundaries" {
  // 错误处理和边界测试
  // 除零情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
}