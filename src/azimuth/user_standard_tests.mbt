// 用户请求的标准 MoonBit 测试用例
// 包含10个测试用例，使用标准 MoonBit 测试语法

test "add_function_basic" {
  // 测试 add 函数的基本功能
  let result1 = add(10, 20)
  assert_eq(result1, 30)
  
  let result2 = add(-5, 15)
  assert_eq(result2, 10)
}

test "multiply_function_basic" {
  // 测试 multiply 函数的基本功能
  let result1 = multiply(7, 8)
  assert_eq(result1, 56)
  
  let result2 = multiply(-4, 6)
  assert_eq(result2, -24)
}

test "greet_function_basic" {
  // 测试 greet 函数的基本功能
  let result1 = greet("MoonBit")
  assert_eq(result1, "Hello, MoonBit!")
  
  let result2 = greet("")
  assert_eq(result2, "Hello, !")
}

test "math_properties_commutative" {
  // 测试数学交换律
  let a = 15
  let b = 25
  
  // 加法交换律
  let result1 = add(a, b)
  let result2 = add(b, a)
  assert_eq(result1, result2)
  
  // 乘法交换律
  let result3 = multiply(a, b)
  let result4 = multiply(b, a)
  assert_eq(result3, result4)
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值与0的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  // 计算：(5 + 3) * 4 - 6
  let step1 = add(5, 3)  // 8
  let step2 = multiply(step1, 4)  // 32
  let result = add(step2, -6)  // 26
  
  assert_eq(result, 26)
}

test "string_with_special_characters" {
  // 测试包含特殊字符的字符串
  let result1 = greet("test@example.com")
  assert_eq(result1, "Hello, test@example.com!")
  
  let result2 = greet("user_123")
  assert_eq(result2, "Hello, user_123!")
}

test "negative_number_operations" {
  // 测试负数运算
  let result1 = add(-10, -20)
  assert_eq(result1, -30)
  
  let result2 = multiply(-5, -7)
  assert_eq(result2, 35)
  
  let result3 = multiply(-8, 3)
  assert_eq(result3, -24)
}

test "zero_value_operations" {
  // 测试零值运算
  let result1 = add(0, 42)
  assert_eq(result1, 42)
  
  let result2 = multiply(0, 100)
  assert_eq(result2, 0)
  
  let result3 = multiply(123, 0)
  assert_eq(result3, 0)
}

test "real_world_calculation" {
  // 测试实际应用场景：计算矩形面积和周长
  let length = 12
  let width = 8
  
  // 计算面积
  let area = multiply(length, width)
  assert_eq(area, 96)
  
  // 计算周长
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(perimeter, 40)
}