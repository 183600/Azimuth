// Azimuth 综合测试套件
// 使用标准的 MoonBit 测试语法，全面测试项目功能

test "add_function_basic_tests" {
  // 基本加法功能测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(-3, @azimuth.add(5, -8))
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
}

test "add_function_overflow_tests" {
  // 加法溢出处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 负数溢出测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 混合符号不溢出测试
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_function_basic_tests" {
  // 基本乘法功能测试
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(1, 100))
  @azimuth.assert_eq(-100, @azimuth.multiply(-1, 100))
}

test "multiply_function_overflow_tests" {
  // 乘法溢出处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大数乘法溢出测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值测试
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 3))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46340, 46341))
}

test "greet_function_tests" {
  // 字符串问候函数测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
}

test "assert_helper_functions_tests" {
  // 断言辅助函数测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "edge_cases_combination_tests" {
  // 边界情况组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法和乘法组合
  let sum = @azimuth.add(1000, 2000)
  @azimuth.assert_eq(3000, sum)
  let product = @azimuth.multiply(sum, 2)
  @azimuth.assert_eq(6000, product)
  
  // 边界值运算
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

test "negative_number_operations_tests" {
  // 负数运算测试
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(0, @azimuth.add(-100, 100))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
  @azimuth.assert_eq(1000, @azimuth.multiply(-100, -10))
}

test "special_value_operations_tests" {
  // 特殊值运算测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值和最小值的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 1和-1的特殊处理
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculation_tests" {
  // 复杂计算测试
  // 测试多个函数的组合使用
  let a = 100
  let b = 200
  let c = -50
  
  let sum_ab = @azimuth.add(a, b)
  @azimuth.assert_eq(300, sum_ab)
  
  let sum_abc = @azimuth.add(sum_ab, c)
  @azimuth.assert_eq(250, sum_abc)
  
  let product_ab = @azimuth.multiply(a, b)
  @azimuth.assert_eq(20000, product_ab)
  
  let product_abc = @azimuth.multiply(product_ab, 2)
  @azimuth.assert_eq(40000, product_abc)
}