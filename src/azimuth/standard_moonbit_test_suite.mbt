// 标准 MoonBit 测试套件
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_basic_cases" {
  // 测试 add 函数的基本情况
  assert_eq(add(2, 3), 5)
  assert_eq(add(0, 0), 0)
  assert_eq(add(-2, 3), 1)
  assert_eq(add(10, -5), 5)
  assert_eq(add(-3, -4), -7)
}

test "multiply_function_comprehensive" {
  // 测试 multiply 函数的综合情况
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(0, 100), 0)
  assert_eq(multiply(-2, 5), -10)
  assert_eq(multiply(2, -5), -10)
  assert_eq(multiply(-2, -5), 10)
  assert_eq(multiply(1, 50), 50)
}

test "subtract_function_edge_cases" {
  // 测试 subtract 函数的边界情况
  assert_eq(subtract(10, 3), 7)
  assert_eq(subtract(3, 10), -7)
  assert_eq(subtract(0, 0), 0)
  assert_eq(subtract(5, -3), 8)
  assert_eq(subtract(-5, 3), -8)
  assert_eq(subtract(-5, -3), -2)
}

test "divide_with_ceil_function_behavior" {
  // 测试 divide_with_ceil 函数的行为
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 5), 1)
  assert_eq(divide_with_ceil(0, 10), 0)
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
  assert_eq_string(greet("!@#"), "Hello, !@#!")
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
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

test "mathematical_properties_validation" {
  // 测试数学性质验证
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

test "boundary_value_analysis" {
  // 测试边界值分析
  // 测试零值
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 5), 0)
  assert_eq(subtract(0, 0), 0)
  
  // 测试单位值
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 5), 5)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(5, 1), 5)
  
  // 测试负一
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 5), -5)
  assert_eq(subtract(0, -1), 1)
}