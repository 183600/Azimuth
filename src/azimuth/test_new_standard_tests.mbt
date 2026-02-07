// Azimuth 项目标准单元测试
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_addition_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(2, @azimuth.add(1, 1))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.add(-1, 1))
  @azimuth.assert_eq(300, @azimuth.add(100, 200))
  @azimuth.assert_eq(-80, @azimuth.add(-50, -30))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "divide_with_ceil_basic_functionality" {
  // 测试向上取整除法基本功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(5, 1))
}

test "subtract_functionality" {
  // 测试减法功能
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(0, 0))
  @azimuth.assert_eq(5, @azimuth.subtract(-5, -10))
  @azimuth.assert_eq(50, @azimuth.subtract(100, 50))
}

test "greet_function_basic_cases" {
  // 测试问候函数基本情况
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 3
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法单位元
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  
  // 乘法单位元
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
}

test "edge_cases_and_boundary_values" {
  // 测试边界值和特殊情况
  // 零值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  
  // 除零处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 单位值测试
  @azimuth.assert_eq(1, @azimuth.add(1, 0))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(42, @azimuth.divide_with_ceil(42, 1))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 复合运算：(5 + 3) * 2 - 4 / 2 (向上取整)
  let step1 = @azimuth.add(5, 3)        // 8
  let step2 = @azimuth.multiply(step1, 2) // 16
  let step3 = @azimuth.divide_with_ceil(4, 2) // 2
  let result = @azimuth.subtract(step2, step3) // 14
  
  @azimuth.assert_eq(8, step1)
  @azimuth.assert_eq(16, step2)
  @azimuth.assert_eq(2, step3)
  @azimuth.assert_eq(14, result)
  
  // 负数混合运算
  let neg_result = @azimuth.add(@azimuth.multiply(-2, 3), @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(-2, neg_result)  // -6 + 4 = -2
}