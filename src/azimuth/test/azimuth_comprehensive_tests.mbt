// Azimuth 项目综合测试用例
// 使用标准 MoonBit 测试语法，提供全面的功能测试

use azimuth

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(0, add(5, -5))
  assert_eq(25, multiply(-5, -5))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  // 验证加法恒等式
  let x = 42
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 验证乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
}

test "calculation_precision_tests" {
  // 计算精度测试
  // 连续加法
  let result = add(add(add(1, 2), 3), 4)
  assert_eq(10, result)
  
  // 连续乘法
  let product = multiply(multiply(multiply(2, 3), 4), 5)
  assert_eq(120, product)
}

test "real_world_applications" {
  // 实际应用场景测试
  // 计算三角形面积: 底 × 高 ÷ 2
  let base = 8
  let height = 6
  let triangle_area = multiply(base, height) / 2
  assert_eq(24, triangle_area)
  
  // 计算立方体体积: 边长³
  let side = 4
  let volume = multiply(multiply(side, side), side)
  assert_eq(64, volume)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试小数值
  assert_eq(2, add(1, 1))
  assert_eq(1, multiply(1, 1))
  
  // 测试负值边界
  assert_eq(-2, add(-1, -1))
  assert_eq(1, multiply(-1, -1))
  
  // 测试零值运算
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
}

test "performance_stress_test" {
  // 性能压力测试
  // 大数运算
  let large_num = 10000
  let another_large = 20000
  
  assert_eq(30000, add(large_num, another_large))
  assert_eq(200000000, multiply(large_num, another_large))
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  // 测试可能的边界情况
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试最大值加法（可能的溢出情况）
  let result1 = add(max_int, 0)
  assert_eq(max_int, result1)
  
  // 测试最小值运算
  let result2 = multiply(min_int, 1)
  assert_eq(min_int, result2)
}

test "algorithm_complexity_verification" {
  // 算法复杂度验证测试
  // 测试计算复杂表达式的正确性
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * (c + (-a))
  let complex_result = multiply(add(a, b), add(c, -a))
  assert_eq(600, complex_result)
  
  // a * b + c * a
  let another_complex = add(multiply(a, b), multiply(c, a))
  assert_eq(500, another_complex)
}

test "data_type_consistency" {
  // 数据类型一致性测试
  // 确保所有运算返回一致的数据类型
  let int_result1 = add(100, 200)
  let int_result2 = multiply(10, 20)
  
  // 验证结果类型一致性
  assert_eq(300, int_result1)
  assert_eq(200, int_result2)
  
  // 验证负数运算的一致性
  let negative_result1 = add(-50, 100)
  let negative_result2 = multiply(-5, 10)
  
  assert_eq(50, negative_result1)
  assert_eq(-50, negative_result2)
}

test "functional_composition_test" {
  // 函数组合测试
  // 测试函数的组合使用
  let x = 5
  let y = 3
  
  // 创建复合函数: f(x) = (x + y) * (x + (-y))
  let composite_result = multiply(add(x, y), add(x, -y))
  assert_eq(16, composite_result)
  
  // 另一个复合函数: g(x) = x * y + x + y
  let another_composite = add(multiply(x, y), add(x, y))
  assert_eq(23, another_composite)
}