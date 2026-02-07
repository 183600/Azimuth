// Azimuth 简单 MoonBit 测试用例
// 包含10个标准测试用例，使用标准 MoonBit 测试语法

test "basic_addition" {
  // 基本加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "basic_multiplication" {
  // 基本乘法测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "basic_subtraction" {
  // 基本减法测试
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(2, 7))
  assert_eq(0, subtract(5, 5))
}

test "division_with_ceil" {
  // 向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function" {
  // 问候函数测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "negative_operations" {
  // 负数运算测试
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-8, subtract(-5, 3))
  assert_eq(-1, divide_with_ceil(-5, 3))
}

test "math_properties" {
  // 数学性质测试
  let a = 7
  let b = 3
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法单位元
  assert_eq(add(a, 0), a)
  
  // 乘法单位元
  assert_eq(multiply(a, 1), a)
}

test "complex_calculation" {
  // 复杂计算测试
  let step1 = multiply(10, 5)
  let step2 = add(step1, 15)
  let step3 = divide_with_ceil(step2, 7)
  
  assert_eq(50, step1)
  assert_eq(65, step2)
  assert_eq(10, step3)
}

test "fibonacci_sequence" {
  // 斐波那契数列测试
  let f0 = 0
  let f1 = 1
  let f2 = add(f1, f0)
  let f3 = add(f2, f1)
  let f4 = add(f3, f2)
  let f5 = add(f4, f3)
  
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
}

test "temperature_conversion" {
  // 温度转换测试
  let celsius = 20
  
  // 摄氏度转华氏度
  let fahrenheit_step1 = multiply(celsius, 9)
  let fahrenheit_step2 = divide_with_ceil(fahrenheit_step1, 5)
  let fahrenheit = add(fahrenheit_step2, 32)
  
  assert_eq(68, fahrenheit)
}