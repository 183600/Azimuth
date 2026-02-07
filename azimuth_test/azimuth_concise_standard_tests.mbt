// Azimuth 项目简洁标准测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_addition_functionality" {
  // 基本加法功能测试
  assert_eq(add(2, 3), 5)
  assert_eq(add(0, 0), 0)
  assert_eq(add(-5, 10), 5)
  assert_eq(add(1000, 2000), 3000)
}

test "basic_multiplication_functionality" {
  // 基本乘法功能测试
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(0, 100), 0)
  assert_eq(multiply(-2, 5), -10)
  assert_eq(multiply(-3, -3), 9)
}

test "basic_subtraction_functionality" {
  // 基本减法功能测试
  assert_eq(subtract(10, 3), 7)
  assert_eq(subtract(5, 5), 0)
  assert_eq(subtract(0, 10), -10)
  assert_eq(subtract(-5, -3), -2)
}

test "divide_with_ceil_basic_cases" {
  // 向上取整除法基本测试
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 4), 1)
  assert_eq(divide_with_ceil(0, 5), 0)
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界测试
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
}

test "greet_function_standard_cases" {
  // 问候函数标准测试
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  let base = 100
  let multiplier = 3
  let offset = 25
  let divisor = 4
  
  let step1 = multiply(base, multiplier)
  let step2 = add(step1, offset)
  let result = divide_with_ceil(step2, divisor)
  
  assert_eq(step1, 300)
  assert_eq(step2, 325)
  assert_eq(result, 82)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 15
  let y = 7
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
}