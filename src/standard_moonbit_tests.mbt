// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法和项目中定义的函数

test "basic_addition" {
  // 测试基本加法运算
  assert_eq(add(2, 3), 5)
  assert_eq(add(0, 0), 0)
  assert_eq(add(-1, 1), 0)
  assert_eq(add(100, 200), 300)
}

test "basic_subtraction" {
  // 测试基本减法运算
  assert_eq(subtract(5, 3), 2)
  assert_eq(subtract(0, 0), 0)
  assert_eq(subtract(1, 1), 0)
  assert_eq(subtract(200, 100), 100)
}

test "basic_multiplication" {
  // 测试基本乘法运算
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(0, 5), 0)
  assert_eq(multiply(1, 10), 10)
  assert_eq(multiply(-2, 3), -6)
}

test "basic_division" {
  // 测试基本除法运算（使用向上取整除法）
  assert_eq(divide_with_ceil(8, 2), 4)
  assert_eq(divide_with_ceil(0, 1), 0)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(7, 2), 4)  // 向上取整
}

test "string_operations" {
  // 测试字符串操作
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
}

test "zero_division_cases" {
  // 测试零除法情况
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(add(-5, 3), -2)
  assert_eq(add(5, -3), 2)
  assert_eq(multiply(-3, -2), 6)
  assert_eq(multiply(-2, 3), -6)
  assert_eq(subtract(-10, 5), -15)
  assert_eq(subtract(10, -5), 15)
}

test "division_edge_cases" {
  // 测试除法边界情况
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(100, 99), 2)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
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

test "complex_calculations" {
  // 测试复合计算
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