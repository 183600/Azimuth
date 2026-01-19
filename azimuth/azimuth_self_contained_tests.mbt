// Azimuth 项目自包含标准测试用例
// 使用标准 MoonBit 测试语法，自包含的测试

// 导入所需的函数
use azimuth.*

test "add_function_basic_tests" {
  // 测试 add 函数的基本功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(0, 100))
  assert_eq(-50, add(-50, 0))
}

test "add_function_overflow_tests" {
  // 测试 add 函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  
  // 负数溢出测试
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  
  // 边界值测试
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic_tests" {
  // 测试 multiply 函数的基本功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(0, multiply(10, 0))
  assert_eq(7, multiply(7, 1))
  assert_eq(-7, multiply(7, -1))
}

test "multiply_function_overflow_tests" {
  // 测试 multiply 函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  assert_eq(max_val, multiply(46341, 46341))
  
  // 负数溢出测试
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 最小值特殊处理测试
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_tests" {
  // 测试 greet 函数
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_function_tests" {
  // 测试 divide_with_ceil 函数
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(-11, 3))
  assert_eq(-4, divide_with_ceil(-10, -3))
}

test "complex_calculation_tests" {
  // 测试复合计算
  let a = 12
  let b = 8
  let c = -5
  
  // 计算 (a + b) * c
  let sum = add(a, b)
  let result1 = multiply(sum, c)
  assert_eq(-100, result1)
  
  // 计算 a * b + c
  let product = multiply(a, b)
  let result2 = add(product, c)
  assert_eq(91, result2)
  
  // 计算 (a - b) * (b + c)
  let diff = add(a, -b)
  let sum2 = add(b, c)
  let result3 = multiply(diff, sum2)
  assert_eq(12, result3)
}

test "mathematical_formula_tests" {
  // 测试数学公式计算
  // 计算二次方程 ax² + bx + c，当 x=2 时的值
  let a = 3
  let b = 4
  let c = 5
  let x = 2
  
  // 计算 x²
  let x_squared = multiply(x, x)
  // 计算 ax²
  let ax_squared = multiply(a, x_squared)
  // 计算 bx
  let bx = multiply(b, x)
  // 计算 ax² + bx + c
  let result = add(add(ax_squared, bx), c)
  
  assert_eq(25, result)
}

test "assertion_functions_tests" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq(-42, -42)
  assert_eq_string("test", "test")
  assert_eq_string("", "")
  assert_eq_string("Hello, 世界!", "Hello, 世界!")
}