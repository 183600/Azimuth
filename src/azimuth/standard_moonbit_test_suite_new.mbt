// 标准 MoonBit 测试套件
// 包含10个高质量的测试用例，覆盖核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(5, add(2, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(7, subtract(10, 3))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, User!", greet("User"))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-8, subtract(-5, 3))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "string_operations_edge_cases" {
  // 测试字符串操作的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, space!", greet("space"))
}

test "complex_calculations" {
  // 测试复合计算
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(16, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(17, result2)
  
  // (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(1, result3)
}

test "boundary_value_analysis" {
  // 测试边界值分析
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, subtract(0, 0))
  
  // 单位值测试
  assert_eq(1, add(1, 0))
  assert_eq(5, multiply(1, 5))
  assert_eq(1, subtract(1, 0))
  assert_eq(5, divide_with_ceil(5, 1))
  
  // 负一测试
  assert_eq(0, add(-1, 1))
  assert_eq(-5, multiply(-1, 5))
  assert_eq(1, subtract(0, -1))
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

test "large_number_operations" {
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(500000, subtract(1000000, 500000))
  assert_eq(333334, divide_with_ceil(1000000, 3))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
}