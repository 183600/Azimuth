// 标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(add(2, 3), 5)
  assert_eq(add(-2, 3), 1)
  assert_eq(add(2, -3), -1)
  assert_eq(add(-2, -3), -5)
  assert_eq(add(0, 0), 0)
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(multiply(2, 3), 6)
  assert_eq(multiply(-2, 3), -6)
  assert_eq(multiply(2, -3), -6)
  assert_eq(multiply(-2, -3), 6)
  assert_eq(multiply(0, 5), 0)
}

test "basic_arithmetic_subtraction" {
  // 基本减法运算测试
  assert_eq(subtract(5, 3), 2)
  assert_eq(subtract(3, 5), -2)
  assert_eq(subtract(-3, 5), -8)
  assert_eq(subtract(3, -5), 8)
  assert_eq(subtract(-3, -5), 2)
}

test "divide_with_ceil_basic" {
  // 向上取整除法基本测试
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 5), 1)
  assert_eq(divide_with_ceil(0, 10), 0)
  assert_eq(divide_with_ceil(10, 0), 0)
}

test "divide_with_ceil_negative" {
  // 向上取整除法负数测试
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
  assert_eq(divide_with_ceil(-1, 2), -1)
  assert_eq(divide_with_ceil(-1, -2), 1)
}

test "greet_function_test" {
  // 问候函数测试
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
}

test "complex_calculation_test" {
  // 复合计算测试
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(result1, 16)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(result2, 17)
}

test "mathematical_properties" {
  // 数学性质测试
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

test "boundary_values_test" {
  // 边界值测试
  // 零值测试
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 5), 0)
  assert_eq(subtract(0, 0), 0)
  
  // 单位值测试
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 5), 5)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(5, 1), 5)
}

test "large_numbers_test" {
  // 大数运算测试
  assert_eq(add(1000000, 1000000), 2000000)
  assert_eq(multiply(10000, 10000), 100000000)
  assert_eq(subtract(1000000, 500000), 500000)
  assert_eq(divide_with_ceil(1000000, 3), 333334)
}