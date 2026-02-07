// Azimuth 项目标准质量测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(8, 6))
  assert_eq(6, divide_with_ceil(17, 3))
}

test "boundary_value_testing" {
  // 边界值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(-100, subtract(0, 100))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-14, subtract(-8, 6))
  assert_eq(-5, divide_with_ceil(-14, 3))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "complex_mathematical_expressions" {
  // 复杂数学表达式测试
  let step1 = add(10, 20)
  let step2 = multiply(step1, 3)
  let step3 = subtract(step2, 15)
  let result = divide_with_ceil(step3, 5)
  assert_eq(21, result)
}

test "edge_case_division" {
  // 除法边界情况测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(1, divide_with_ceil(3, 2))
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(2, divide_with_ceil(5, 2))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(-1, divide_with_ceil(1, -2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "practical_calculation_scenario" {
  // 实际计算场景测试 - 模拟购物车计算
  let item1_price = 25
  let item2_price = 30
  let item3_price = 45
  let discount = 10
  
  let subtotal = add(add(item1_price, item2_price), item3_price)
  let final_total = subtract(subtotal, discount)
  let average_price_per_item = divide_with_ceil(final_total, 3)
  
  assert_eq(100, subtotal)
  assert_eq(90, final_total)
  assert_eq(30, average_price_per_item)
}

test "multi_step_computation" {
  // 多步骤计算测试
  let initial_value = 100
  let increment1 = 25
  let increment2 = multiply(2, 15)
  let decrement = 20
  
  let step1 = add(initial_value, increment1)
  let step2 = add(step1, increment2)
  let step3 = subtract(step2, decrement)
  let final_result = divide_with_ceil(step3, 3)
  
  assert_eq(125, step1)
  assert_eq(155, step2)
  assert_eq(135, step3)
  assert_eq(45, final_result)
}