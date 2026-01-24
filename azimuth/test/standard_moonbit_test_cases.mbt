// 标准 MoonBit 测试用例
// 为 azimuth 库添加的高质量测试用例

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(1, @azimuth.add(-2, 3))
}

test "multiply_function_identity_properties" {
  // 测试乘法函数的恒等性质
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "greet_function_string_operations" {
  // 测试问候函数的字符串操作
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "divide_with_ceil_ceil_behavior" {
  // 测试向上取整除法的取整行为
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 7
  let b = 13
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let base = 10
  let doubled = @azimuth.multiply(base, 2)
  let incremented = @azimuth.add(doubled, 5)
  let divided = @azimuth.divide_with_ceil(incremented, 3)
  @azimuth.assert_eq(9, divided)  // ceil((10*2+5)/3) = ceil(25/3) = 9
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "resource_allocation_scenario" {
  // 测试资源分配场景
  let total_items = 47
  let container_capacity = 12
  let containers_needed = @azimuth.divide_with_ceil(total_items, container_capacity)
  @azimuth.assert_eq(4, containers_needed)
  
  let total_capacity = @azimuth.multiply(containers_needed, container_capacity)
  @azimuth.assert_eq(48, total_capacity)
}

test "string_formatting_edge_cases" {
  // 测试字符串格式化边缘情况
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, space test!", @azimuth.greet("space test"))
  @azimuth.assert_eq_string("Hello, !@#$!", @azimuth.greet("!@#$"))
}

test "mathematical_boundary_conditions" {
  // 测试数学边界条件
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}