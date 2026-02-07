// 标准 Azimuth 核心测试用例
// 使用标准 MoonBit 测试语法，包含 8 个高质量测试用例

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(add(2, 3), 5)
  assert_eq(add(-2, 3), 1)
  assert_eq(add(0, 0), 0)
  assert_eq(add(100, 200), 300)
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(-2, 5), -10)
  assert_eq(multiply(0, 100), 0)
  assert_eq(multiply(-3, -3), 9)
}

test "subtraction_operation_tests" {
  // 测试减法运算
  assert_eq(subtract(10, 3), 7)
  assert_eq(subtract(3, 10), -7)
  assert_eq(subtract(5, 5), 0)
  assert_eq(subtract(-5, -3), -2)
}

test "divide_with_ceil_basic_tests" {
  // 测试向上取整除法基本功能
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(0, 5), 0)
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法边界情况
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
}

test "greet_function_tests" {
  // 测试问候函数
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("中文"), "Hello, 中文!")
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let base = 10
  let multiplier = 3
  let offset = 5
  let divisor = 4
  
  // (base * multiplier + offset) / divisor
  let step1 = multiply(base, multiplier)
  let step2 = add(step1, offset)
  let result = divide_with_ceil(step2, divisor)
  
  assert_eq(step1, 30)
  assert_eq(step2, 35)
  assert_eq(result, 9)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法单位元
  assert_eq(add(a, 0), a)
  
  // 乘法单位元
  assert_eq(multiply(a, 1), a)
}