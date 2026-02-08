// 标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  assert_eq(add(2, 3), 5)
  assert_eq(add(0, 0), 0)
  assert_eq(add(-5, 5), 0)
  assert_eq(add(100, 200), 300)
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(0, 100), 0)
  assert_eq(multiply(1, 50), 50)
  assert_eq(multiply(-2, 3), -6)
  assert_eq(multiply(-2, -3), 6)
}

test "subtract_function_comprehensive" {
  // 测试减法函数的综合情况
  assert_eq(subtract(10, 5), 5)
  assert_eq(subtract(5, 10), -5)
  assert_eq(subtract(0, 0), 0)
  assert_eq(subtract(100, 0), 100)
  assert_eq(subtract(0, 100), -100)
}

test "divide_with_ceil_behavior" {
  // 测试向上取整除法的行为
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(-10, 3), -3)
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let a = 10
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(result1, 26)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(result2, 32)
  
  // (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(result3, 4)
}

test "mathematical_properties_validation" {
  // 验证数学性质
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
  
  // 减法的逆运算
  assert_eq(add(subtract(x, y), y), x)
}

test "edge_cases_and_boundary_values" {
  // 测试边界值和特殊情况
  // 零值测试
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 5), 0)
  assert_eq(subtract(0, 0), 0)
  assert_eq(divide_with_ceil(0, 10), 0)
  
  // 单位值测试
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 10), 10)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(10, 1), 10)
  
  // 负数测试
  assert_eq(add(-5, 3), -2)
  assert_eq(multiply(-2, 3), -6)
  assert_eq(subtract(-10, 5), -15)
  assert_eq(divide_with_ceil(-5, 2), -2)
}