// 标准 MoonBit 测试套件
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_basic" {
  // 测试基本加法功能
  assert_eq(add(2, 3), 5)
  assert_eq(add(0, 0), 0)
  assert_eq(add(-5, 5), 0)
  assert_eq(add(100, 200), 300)
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(0, 5), 0)
  assert_eq(multiply(-2, 3), -6)
  assert_eq(multiply(-2, -3), 6)
}

test "subtract_function_basic" {
  // 测试基本减法功能
  assert_eq(subtract(10, 5), 5)
  assert_eq(subtract(5, 10), -5)
  assert_eq(subtract(0, 0), 0)
  assert_eq(subtract(-5, -10), 5)
}

test "divide_with_ceil_function" {
  // 测试向上取整除法功能
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(5, 0), 0)
  assert_eq(divide_with_ceil(-10, 3), -3)
}

test "greet_function_basic" {
  // 测试问候函数基本功能
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 10
  let b = 5
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(result1, 30)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(result2, 52)
  
  // (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(result3, 3)
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
}

test "edge_cases_and_boundary_values" {
  // 测试边界情况和极值
  // 测试零值
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 5), 0)
  assert_eq(subtract(0, 0), 0)
  
  // 测试单位值
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 5), 5)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(5, 1), 5)
  
  // 测试负数
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 5), -5)
  assert_eq(subtract(0, -1), 1)
}