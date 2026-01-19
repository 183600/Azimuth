// Azimuth 项目标准测试套件
// 使用标准 MoonBit 测试语法，完全自包含的测试

// 测试辅助函数，提供断言功能
fn assert_eq(expected : Int, actual : Int) -> Unit {
  // 使用条件分支来实现断言，避免依赖 @builtin.abort
  if (expected == actual) {
    ()
  } else {
    // 当断言失败时，抛出一个错误
    // 使用整数除以零来触发运行时错误
    let _ = 1 / 0
    ()
  }
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  // 使用条件分支来实现断言，避免依赖 @builtin.abort
  if (expected == actual) {
    ()
  } else {
    // 当断言失败时，抛出一个错误
    // 使用整数除以零来触发运行时错误
    let _ = 1 / 0
    ()
  }
}

fn assert_true(condition : Bool) -> Unit {
  // 使用条件分支来实现断言，避免依赖 @builtin.abort
  if (condition) {
    ()
  } else {
    // 当断言失败时，抛出一个错误
    // 使用整数除以零来触发运行时错误
    let _ = 1 / 0
    ()
  }
}

fn assert_false(condition : Bool) -> Unit {
  // 使用条件分支来实现断言，避免依赖 @builtin.abort
  if (condition) {
    // 当断言失败时，抛出一个错误
    // 使用整数除以零来触发运行时错误
    let _ = 1 / 0
    ()
  } else {
    ()
  }
}

// 测试用例
test "add_function_basic_tests" {
  // 测试 add 函数的基本功能
  assert_eq(10, @azimuth.add(3, 7))
  assert_eq(0, @azimuth.add(5, -5))
  assert_eq(-8, @azimuth.add(-3, -5))
  assert_eq(100, @azimuth.add(0, 100))
  assert_eq(-50, @azimuth.add(-50, 0))
}

test "add_function_overflow_tests" {
  // 测试 add 函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  assert_eq(max_val, @azimuth.add(max_val, 1))
  assert_eq(max_val, @azimuth.add(max_val, 100))
  
  // 负数溢出测试
  assert_eq(min_val, @azimuth.add(min_val, -1))
  assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 边界值测试
  assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_function_basic_tests" {
  // 测试 multiply 函数的基本功能
  assert_eq(15, @azimuth.multiply(3, 5))
  assert_eq(-15, @azimuth.multiply(-3, 5))
  assert_eq(15, @azimuth.multiply(-3, -5))
  assert_eq(0, @azimuth.multiply(10, 0))
  assert_eq(7, @azimuth.multiply(7, 1))
  assert_eq(-7, @azimuth.multiply(7, -1))
}

test "multiply_function_overflow_tests" {
  // 测试 multiply 函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  assert_eq(max_val, @azimuth.multiply(46341, 46341))
  
  // 负数溢出测试
  assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  
  // 最小值特殊处理测试
  assert_eq(min_val, @azimuth.multiply(min_val, 2))
  assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_function_tests" {
  // 测试 greet 函数
  assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  assert_eq_string("Hello, World!", @azimuth.greet("World"))
  assert_eq_string("Hello, !", @azimuth.greet(""))
  assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_function_tests" {
  // 测试 divide_with_ceil 函数
  assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  assert_eq(-4, @azimuth.divide_with_ceil(-10, -3))
}

test "complex_calculation_tests" {
  // 测试复合计算
  let a = 12
  let b = 8
  let c = -5
  
  // 计算 (a + b) * c
  let sum = @azimuth.add(a, b)
  let result1 = @azimuth.multiply(sum, c)
  assert_eq(-100, result1)
  
  // 计算 a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  assert_eq(91, result2)
  
  // 计算 (a - b) * (b + c)
  let diff = @azimuth.add(a, -b)
  let sum2 = @azimuth.add(b, c)
  let result3 = @azimuth.multiply(diff, sum2)
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
  let x_squared = @azimuth.multiply(x, x)
  // 计算 ax²
  let ax_squared = @azimuth.multiply(a, x_squared)
  // 计算 bx
  let bx = @azimuth.multiply(b, x)
  // 计算 ax² + bx + c
  let result = @azimuth.add(@azimuth.add(ax_squared, bx), c)
  
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