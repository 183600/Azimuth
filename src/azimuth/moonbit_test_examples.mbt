// MoonBit 测试用例示例
// 使用标准的 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 8
  let b = 6
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 2)), add(multiply(a, b), multiply(a, 2)))
}

test "boundary_value_handling" {
  // 边界值处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
}

test "real_world_calculations" {
  // 实际应用场景计算测试
  // 面积计算：矩形面积 = 长 × 宽
  let length = 12
  let width = 8
  let area = multiply(length, width)
  assert_eq(96, area)
  
  // 周长计算：矩形周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(40, perimeter)
}

test "conditional_logic_operations" {
  // 条件逻辑操作测试
  let x = 25
  let y = 30
  
  // 如果 x < y，则计算 x + y，否则计算 x * y
  let condition = x < y
  let result = if condition { add(x, y) } else { multiply(x, y) }
  assert_eq(55, result)
  
  // 反向条件测试
  let condition2 = x > y
  let result2 = if condition2 { add(x, y) } else { multiply(x, y) }
  assert_eq(750, result2)
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 计算 (a + b) * c - d
  let a = 10
  let b = 15
  let c = 4
  let d = 20
  
  let sum_ab = add(a, b)
  let product = multiply(sum_ab, c)
  let result = add(product, -d)
  
  assert_eq(80, result)
}

test "data_processing_patterns" {
  // 数据处理模式测试
  // 计算平均值：(a + b + c) / 3
  let value1 = 85
  let value2 = 90
  let value3 = 95
  
  let total = add(add(value1, value2), value3)
  let average = total / 3
  
  assert_eq(90, average)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
  
  // 测试正常情况
  assert_true(add(10, 20) == 30)
  assert_true(multiply(6, 7) == 42)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 测试特殊字符
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  
  // 测试数字字符串
  assert_eq_string("Hello, 12345!", greet("12345"))
  
  // 测试混合字符串
  assert_eq_string("Hello, test_123!", greet("test_123"))
}