// 标准 MoonBit 测试用例
// 包含10个高质量的测试用例，使用标准 MoonBit 测试语法

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(add(2, 3), 5)
  assert_eq(add(0, 0), 0)
  assert_eq(add(-5, 5), 0)
  assert_eq(add(100, 200), 300)
  assert_eq(add(-10, -20), -30)
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(0, 10), 0)
  assert_eq(multiply(-2, 5), -10)
  assert_eq(multiply(-3, -3), 9)
  assert_eq(multiply(100, 100), 10000)
}

test "basic_subtraction_functionality" {
  // 测试基本减法功能
  assert_eq(subtract(10, 3), 7)
  assert_eq(subtract(5, 5), 0)
  assert_eq(subtract(0, 10), -10)
  assert_eq(subtract(-5, -10), 5)
  assert_eq(subtract(100, 50), 50)
}

test "divide_with_ceil_basic_cases" {
  // 测试向上取整除法的基本情况
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(100, 10), 10)
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet("Alice"), "Hello, Alice!")
  assert_eq_string(greet("Bob"), "Hello, Bob!")
  assert_eq_string(greet("123"), "Hello, 123!")
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet(" "), "Hello,  !")
  assert_eq_string(greet("!@#"), "Hello, !@#!")
  assert_eq_string(greet("中文"), "Hello, 中文!")
  assert_eq_string(greet("a"), "Hello, a!")
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(result1, 16)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(result2, 17)
  
  // (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(result3, 1)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 3
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
  
  // 减法的逆运算
  assert_eq(add(subtract(x, y), y), x)
}

test "boundary_value_testing" {
  // 测试边界值
  // 零值测试
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 5), 0)
  assert_eq(subtract(0, 0), 0)
  assert_eq(divide_with_ceil(0, 10), 0)
  
  // 单位值测试
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 5), 5)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(5, 1), 5)
  
  // 负一测试
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 5), -5)
  assert_eq(subtract(0, -1), 1)
}