// 标准 MoonBit 测试套件
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(add(10, 5), 15)
  assert_eq(multiply(4, 7), 28)
  assert_eq(subtract(20, 8), 12)
  assert_eq(divide_with_ceil(15, 4), 4)
  assert_eq(divide_with_ceil(16, 4), 4)
}

test "division_edge_cases" {
  // 除法边界情况测试
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("测试"), "Hello, 测试!")
}

test "negative_number_calculations" {
  // 负数计算测试
  assert_eq(add(-5, 3), -2)
  assert_eq(add(5, -3), 2)
  assert_eq(multiply(-4, 5), -20)
  assert_eq(multiply(4, -5), -20)
  assert_eq(multiply(-4, -5), 20)
  assert_eq(subtract(-10, 5), -15)
  assert_eq(subtract(10, -5), 15)
}

test "large_number_operations" {
  // 大数运算测试
  assert_eq(add(100000, 200000), 300000)
  assert_eq(multiply(1000, 1000), 1000000)
  assert_eq(subtract(500000, 250000), 250000)
  assert_eq(divide_with_ceil(1000000, 3), 333334)
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(add(12, 8), 20)
  assert_eq(multiply(12, 8), 96)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 零值测试
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 100), 0)
  assert_eq(subtract(0, 0), 0)
  
  // 单位值测试
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 100), 100)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(100, 1), 100)
  
  // 负一测试
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 100), -100)
  assert_eq(subtract(0, -1), 1)
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  let base = 100
  let multiplier = 3
  let increment = 25
  let divisor = 5
  
  // 复杂计算链
  let step1 = multiply(base, multiplier)      // 100 * 3 = 300
  let step2 = add(step1, increment)           // 300 + 25 = 325
  let step3 = subtract(step2, divisor)        // 325 - 5 = 320
  let result = divide_with_ceil(step3, divisor) // ceil(320/5) = 64
  
  assert_eq(step1, 300)
  assert_eq(step2, 325)
  assert_eq(step3, 320)
  assert_eq(result, 64)
}