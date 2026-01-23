// 标准 Azimuth 测试用例
// 包含 8 个高质量测试用例，覆盖核心功能

test "add_function_basic_properties" {
  // 测试加法函数的基本性质
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(5, @azimuth.add(-2, 7))
}

test "multiply_function_identity_elements" {
  // 测试乘法函数的单位元和零元
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-42, @azimuth.multiply(-1, 42))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_practical_scenarios" {
  // 测试向上取整除法的实际应用场景
  // 计算需要的箱子数量：25个物品，每箱6个
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(25, 6))
  
  // 计算需要的车辆：47个包裹，每车10个
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(47, 10))
  
  // 处理除零情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 17
  let b = 29
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算项目成本
  let base_cost = 1000
  let multiplier = 3
  let discount = -200
  
  let total = @azimuth.add(@azimuth.multiply(base_cost, multiplier), discount)
  @azimuth.assert_eq(2800, total)
}

test "edge_case_boundary_values" {
  // 测试边界值情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  // 场景：服务器资源分配
  let total_memory = 16384  // GB
  let servers = 5
  
  // 计算每台服务器分配的内存（向上取整）
  let memory_per_server = @azimuth.divide_with_ceil(total_memory, servers)
  @azimuth.assert_eq(3277, memory_per_server)  // ceil(16384/5) = 3277
  
  // 验证总分配内存
  let total_allocated = @azimuth.multiply(memory_per_server, servers)
  @azimuth.assert_eq(16385, total_allocated)
}