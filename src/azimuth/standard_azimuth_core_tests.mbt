// 标准 Azimuth 核心测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_basic" {
  // 测试 add 函数的基本功能
  assert_eq(add(2, 3), 5)
  assert_eq(add(0, 0), 0)
  assert_eq(add(-1, 1), 0)
  assert_eq(add(100, 200), 300)
}

test "multiply_function_basic" {
  // 测试 multiply 函数的基本功能
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(0, 5), 0)
  assert_eq(multiply(-2, 3), -6)
  assert_eq(multiply(-2, -3), 6)
}

test "subtract_function_basic" {
  // 测试 subtract 函数的基本功能
  assert_eq(subtract(10, 3), 7)
  assert_eq(subtract(5, 5), 0)
  assert_eq(subtract(3, 10), -7)
  assert_eq(subtract(-5, -3), -2)
}

test "divide_with_ceil_function_positive" {
  // 测试 divide_with_ceil 函数的正数情况
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 5), 1)
  assert_eq(divide_with_ceil(0, 10), 0)
}

test "divide_with_ceil_function_edge_cases" {
  // 测试 divide_with_ceil 函数的边界情况
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
}

test "greet_function_basic" {
  // 测试 greet 函数的基本功能
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let a = 10
  let b = 3
  
  // (a + b) * (a - b)
  let sum = add(a, b)
  let diff = subtract(a, b)
  let result = multiply(sum, diff)
  
  assert_eq(sum, 13)
  assert_eq(diff, 7)
  assert_eq(result, 91)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
}