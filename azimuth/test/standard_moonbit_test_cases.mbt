// 标准 MoonBit 测试用例
// 为 azimuth 库添加的高质量测试覆盖

test "arithmetic_commutative_law" {
  // 测试算术交换律
  let a = 15
  let b = 27
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "division_ceil_precision" {
  // 测试向上取整除法的精度
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(19, 4))
  @azimuth.assert_eq(10, @azimuth.divide_with_ceil(91, 10))
}

test "greet_function_multilingual" {
  // 测试问候函数的多语言支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：资源分配
  let total_resources = 100
  let teams = 3
  let base_allocation = @azimuth.divide_with_ceil(total_resources, teams)
  let remaining = @azimuth.add(total_resources, -@azimuth.multiply(base_allocation, teams))
  
  @azimuth.assert_eq(34, base_allocation)
  @azimuth.assert_eq(-2, remaining)  // 可能的负值表示超额分配
}

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 42
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
}

test "error_handling_division" {
  // 测试除法错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(0, 1))
}

test "string_boundary_cases" {
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, ABC123!", @azimuth.greet("ABC123"))
}

test "business_logic_packaging" {
  // 测试业务逻辑：包装计算
  let items = 47
  let box_capacity = 12
  let boxes_needed = @azimuth.divide_with_ceil(items, box_capacity)
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  let unused_space = @azimuth.add(total_capacity, -items)
  
  @azimuth.assert_eq(4, boxes_needed)
  @azimuth.assert_eq(48, total_capacity)
  @azimuth.assert_eq(1, unused_space)
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(0, @azimuth.add(-10, 10))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 4))
}

test "performance_large_numbers" {
  // 测试大数性能计算
  let large_num = 100000
  @azimuth.assert_eq(200000, @azimuth.add(large_num, large_num))
  @azimuth.assert_eq(100000000, @azimuth.multiply(large_num, large_num))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(large_num, large_num))
  @azimuth.assert_eq(100001, @azimuth.divide_with_ceil(@azimuth.add(large_num, 1), large_num))
}