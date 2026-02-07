// Azimuth 核心功能测试
// 标准 MoonBit 测试用例 - 8个高质量测试

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(add(5, 3), 8)
  assert_eq(multiply(4, 6), 24)
  assert_eq(subtract(10, 7), 3)
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(add(-5, 3), -2)
  assert_eq(add(5, -3), 2)
  assert_eq(multiply(-4, 6), -24)
  assert_eq(multiply(4, -6), -24)
  assert_eq(multiply(-4, -6), 24)
  assert_eq(subtract(-10, 7), -17)
  assert_eq(subtract(10, -7), 17)
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(0, 1), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
}

test "greet_function_validation" {
  // 问候函数验证测试
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
}

test "mathematical_properties" {
  // 数学性质验证测试
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

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试零值
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 5), 0)
  assert_eq(subtract(0, 0), 0)
  
  // 测试单位值
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 5), 5)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(5, 1), 5)
  
  // 测试负一
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 5), -5)
  assert_eq(subtract(0, -1), 1)
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  let base = 100
  let factor = 3
  let offset = 15
  let divisor = 4
  
  // 计算流水线
  let step1 = multiply(base, factor)  // 100 * 3 = 300
  let step2 = add(step1, offset)      // 300 + 15 = 315
  let step3 = subtract(step2, factor) // 315 - 3 = 312
  let result = divide_with_ceil(step3, divisor)  // ceil(312/4) = 78
  
  assert_eq(step1, 300)
  assert_eq(step2, 315)
  assert_eq(step3, 312)
  assert_eq(result, 78)
}

test "assertion_functions_test" {
  // 断言函数测试
  // 这些测试验证断言函数本身的行为
  let test_value1 = 42
  let test_value2 = 42
  let test_string1 = "Hello"
  let test_string2 = "Hello"
  
  // 使用 assert_eq 进行验证
  assert_eq(test_value1, test_value2)
  
  // 使用 assert_eq_string 进行验证
  assert_eq_string(test_string1, test_string2)
  
  // 测试算术结果
  assert_eq(add(10, 20), 30)
  assert_eq(multiply(6, 7), 42)
  assert_eq_string(greet("Test"), "Hello, Test!")
}