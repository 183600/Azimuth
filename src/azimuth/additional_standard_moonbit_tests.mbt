// 额外的标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "add_basic_functionality" {
  // 基本加法功能测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-3, -7))
  assert_eq(100, add(50, 50))
}

test "add_boundary_values" {
  // 边界值加法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(-1, add(max_val, min_val))
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
}

test "multiply_basic_functionality" {
  // 基本乘法功能测试
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(0, multiply(5, 0))
  assert_eq(100, multiply(10, 10))
}

test "multiply_special_cases" {
  // 乘法特殊情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
  assert_eq(0, multiply(0, max_val))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(1, max_val))
}

test "greet_functionality" {
  // greet 函数功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_arithmetic_operations" {
  // 复合算术运算测试
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(900, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(230, result2)
  
  // (a + b + c) * 2
  let result3 = multiply(add(add(a, b), c), 2)
  assert_eq(120, result3)
}

test "mathematical_properties" {
  // 数学性质测试
  let x = 15
  let y = 25
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 5)), add(multiply(x, y), multiply(x, 5)))
  
  // 结合律
  assert_eq(add(add(x, y), 5), add(x, add(y, 5)))
}

test "real_world_calculations" {
  // 实际应用计算测试
  // 计算矩形面积和周长
  let length = 8
  let width = 6
  
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(48, area)
  assert_eq(28, perimeter)
  
  // 计算平均值
  let score1 = 85
  let score2 = 90
  let score3 = 95
  
  let average = add(add(score1, score2), score3) / 3
  assert_eq(90, average)
}

test "string_operations_edge_cases" {
  // 字符串操作边界情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, ABCDEFGHIJKLMNOPQRSTUVWXYZ!", greet("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  assert_eq_string("Hello, 1234567890!", greet("1234567890"))
  assert_eq_string("Hello, Special@#$%!", greet("Special@#$%"))
}

test "overflow_protection" {
  // 溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_eq(max_val, add(max_val, 100))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(min_val, add(min_val, min_val))
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(0, multiply(max_val, 0))
}