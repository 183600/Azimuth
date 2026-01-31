// Azimuth 项目标准测试套件
// 包含10个核心功能测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_functions" {
  // 基本算术函数测试
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-2, @azimuth.subtract(3, 5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(7, 4))
}

test "mathematical_commutative_laws" {
  // 数学交换律验证
  let a = 12
  let b = 8
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(5, -3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-5, -3))
}

test "greet_function_variations" {
  // 问候函数变体测试
  @azimuth.assert_eq_string("Hello, Azimuth!", @azimuth.greet("Azimuth"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
}

test "identity_elements" {
  // 数学恒等元素测试
  let x = 42
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
  @azimuth.assert_eq(0, @azimuth.subtract(x, x))
}

test "boundary_value_analysis" {
  // 边界值分析测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000001, @azimuth.divide_with_ceil(1000001, 1))
}

test "negative_number_operations" {
  // 负数运算测试
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(8, @azimuth.subtract(3, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 4))
}

test "resource_allocation_scenario" {
  // 资源分配场景测试
  let total_items = 35
  let container_capacity = 8
  let containers_needed = @azimuth.divide_with_ceil(total_items, container_capacity)
  
  @azimuth.assert_eq(5, containers_needed)
  
  let total_capacity = @azimuth.multiply(containers_needed, container_capacity)
  @azimuth.assert_eq(40, total_capacity)
  
  let unused_capacity = @azimuth.subtract(total_capacity, total_items)
  @azimuth.assert_eq(5, unused_capacity)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  let base = 10
  let step1 = @azimuth.multiply(base, 3)
  let step2 = @azimuth.add(step1, 15)
  let step3 = @azimuth.divide_with_ceil(step2, 7)
  let final_result = @azimuth.subtract(step3, 2)
  
  @azimuth.assert_eq(3, final_result)  // ceil((10*3+15)/7)-2 = ceil(45/7)-2 = 7-2 = 5
}

test "error_handling_and_edge_cases" {
  // 错误处理和边缘情况测试
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 空字符串和特殊字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  
  // 极值验证
  @azimuth.assert_true(0 <= @azimuth.divide_with_ceil(100, 3))
  @azimuth.assert_true(@azimuth.divide_with_ceil(10, 3) >= 0)
}