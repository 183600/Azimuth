// Azimuth 项目综合测试套件
// 使用标准的 MoonBit 测试语法，覆盖项目核心功能

test "add_function_basic_tests" {
  // 测试 add 函数的基本功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(0, 100))
}

test "add_function_overflow_tests" {
  // 测试 add 函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val/2, max_val/2 + 1))
  
  // 负数溢出测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val/2, min_val/2 - 1))
}

test "multiply_function_basic_tests" {
  // 测试 multiply 函数的基本功能
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
}

test "multiply_function_overflow_tests" {
  // 测试 multiply 函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  
  // 负数溢出测试
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  
  // 最小值特殊测试
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 与1和-1的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_function_tests" {
  // 测试 greet 函数的功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "complex_arithmetic_expressions" {
  // 测试复杂的算术表达式
  // (a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(22, result)
}

test "mathematical_sequence_test" {
  // 测试数学序列计算
  // 计算等差数列前n项和：n/2 * (2a1 + (n-1)d)
  let n = 10
  let a1 = 1
  let d = 2
  
  let term1 = @azimuth.multiply(2, a1)
  let term2 = @azimuth.multiply(n - 1, d)
  let sum_formula = @azimuth.add(term1, term2)
  let series_sum = @azimuth.multiply(n, sum_formula) / 2
  
  @azimuth.assert_eq(100, series_sum)
}

test "divide_with_ceil_function_tests" {
  // 测试向上取整除法函数
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(8, 4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 4))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 4))
}

test "assertion_functions_test" {
  // 测试断言函数本身
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("MoonBit", "MoonBit")
  
  // 测试布尔表达式
  let x = 10
  let y = 20
  @azimuth.assert_true(x < y)
  @azimuth.assert_false(x > y)
}