// 标准 MoonBit 增强测试用例
// 包含9个高质量测试用例，覆盖基本功能和边界情况

test "add_function_standard_cases" {
  // 加法函数标准测试用例
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_comprehensive" {
  // 乘法函数综合测试
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
}

test "divide_with_ceil_precision" {
  // 向上取整除法精度测试
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "greet_function_variations" {
  // 问候函数变化测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_commutative_law" {
  // 数学交换律验证
  let a = 7
  let b = 11
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  let packages = 29
  let capacity = 6
  let needed = @azimuth.divide_with_ceil(packages, capacity)
  @azimuth.assert_eq(5, needed)
  
  let total_capacity = @azimuth.multiply(needed, capacity)
  @azimuth.assert_eq(30, total_capacity)
}

test "negative_number_operations" {
  // 负数运算测试
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-8, -3))
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let base = 10
  let step1 = @azimuth.add(base, 5)        // 15
  let step2 = @azimuth.multiply(step1, 2)  // 30
  let step3 = @azimuth.divide_with_ceil(step2, 8)  // 4
  let step4 = @azimuth.add(step3, 1)        // 5
  @azimuth.assert_eq(5, step4)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
}