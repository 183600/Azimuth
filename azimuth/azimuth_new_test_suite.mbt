// Azimuth 项目新测试套件
// 使用标准 MoonBit 测试语法，全面测试核心功能

test "add_function_basic_cases" {
  // 测试 add 函数的基本情况
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(0, 10))
  assert_eq(-10, add(0, -10))
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  
  // 测试极值相加
  assert_eq(-1, add(max_val, min_val))
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
}

test "multiply_function_basic_cases" {
  // 测试 multiply 函数的基本情况
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(46341, 46341))
  
  // 测试特殊值
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "greet_function_comprehensive" {
  // 测试 greet 函数的各种情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
}

test "mathematical_properties" {
  // 测试数学运算的性质
  // 测试加法交换律
  assert_eq(add(7, 11), add(11, 7))
  assert_eq(add(-5, 15), add(15, -5))
  assert_eq(add(100, -100), add(-100, 100))
  
  // 测试乘法交换律
  assert_eq(multiply(7, 11), multiply(11, 7))
  assert_eq(multiply(2, -3), multiply(-3, 2))
  assert_eq(multiply(0, 5), multiply(5, 0))
  
  // 测试乘法分配律
  assert_eq(multiply(2, add(3, 4)), add(multiply(2, 3), multiply(2, 4)))
  assert_eq(multiply(5, add(10, -3)), add(multiply(5, 10), multiply(5, -3)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item1 = 199
  let item2 = 299
  let item3 = 99
  let discount = 50
  let subtotal = add(add(item1, item2), item3)
  let total = add(subtotal, -discount)
  assert_eq(547, total)
  
  // 温度转换（简化版）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9) / 5, 32)
  assert_eq(77, fahrenheit)
  
  // 面积计算
  let length = 8
  let width = 6
  let area = multiply(length, width)
  assert_eq(48, area)
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算：(2 + 3) × 4 - 5
  let result1 = add(multiply(add(2, 3), 4), -5)
  assert_eq(15, result1)
  
  // 计算：10 × (5 + 3) ÷ 2
  let result2 = multiply(10, add(5, 3)) / 2
  assert_eq(40, result2)
  
  // 计算：平方和 3² + 4²
  let result3 = add(multiply(3, 3), multiply(4, 4))
  assert_eq(25, result3)
}

test "boundary_stress_test" {
  // 边界值压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 连续运算测试
  let result1 = add(add(add(max_val, -100), 50), -25)
  assert_eq(max_val, result1)
  
  let result2 = multiply(multiply(multiply(100, 100), 100), 100)
  assert_eq(100000000, result2)
  
  // 混合边界运算
  let result3 = add(multiply(min_val, 0), max_val)
  assert_eq(max_val, result3)
  
  let result4 = multiply(add(min_val, 1), -1)
  assert_eq(max_val, result4)
}

test "assertion_functions_test" {
  // 测试断言函数本身
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq(-42, -42)
  assert_eq(0, 0)
  assert_eq_string("Hello", "Hello")
  assert_eq_string("", "")
  assert_eq_string("123", "123")
  
  // 测试逻辑表达式
  let x = 5
  let y = 10
  assert_true(x < y)
  assert_false(x > y)
  assert_true(add(x, y) == 15)
  assert_true(multiply(x, y) == 50)
}