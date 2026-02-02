// 基本的 MoonBit 测试用例
// 测试基本的 MoonBit 功能

test "integer_arithmetic" {
  // 测试整数算术运算
  assert_eq(1 + 2, 3)
  assert_eq(10 - 3, 7)
  assert_eq(4 * 5, 20)
  assert_eq(15 / 3, 5)
  assert_eq(17 % 5, 2)
}

test "negative_numbers" {
  // 测试负数运算
  assert_eq(-5 + 8, 3)
  assert_eq(-10 * -2, 20)
  assert_eq(-15 / 3, -5)
  assert_eq(-17 % 5, -2)
}

test "boolean_logic" {
  // 测试布尔逻辑
  assert_eq(true && true, true)
  assert_eq(true && false, false)
  assert_eq(true || false, true)
  assert_eq(false || false, false)
  assert_eq(!true, false)
  assert_eq(!false, true)
}

test "string_operations" {
  // 测试字符串操作
  assert_eq_string("Hello", "Hello")
  assert_eq_string("", "")
  assert_eq_string("Hello, World!", "Hello, World!")
}

test "comparison_operations" {
  // 测试比较操作
  assert_eq(1 < 2, true)
  assert_eq(2 > 1, true)
  assert_eq(1 <= 1, true)
  assert_eq(1 >= 1, true)
  assert_eq(1 == 1, true)
  assert_eq(1 != 2, true)
}

test "mathematical_expressions" {
  // 测试数学表达式
  assert_eq((2 + 3) * 4, 20)
  assert_eq(10 * (5 + 3), 80)
  assert_eq(2 * 3 + 4 * 5, 26)
  assert_eq(100 / (5 * 4), 5)
}

test "complex_calculations" {
  // 测试复杂计算
  let a = 3
  let b = 4
  let c = 5
  
  // 计算二次方程 ax² + bx + c
  let x = 2
  let result = a * x * x + b * x + c
  
  assert_eq(result, 25)  // 3*4 + 4*2 + 5 = 12 + 8 + 5 = 25
}

test "sequential_operations" {
  // 测试连续运算
  let initial = 10
  let step1 = initial + 5        // 15
  let step2 = step1 * 2          // 30
  let step3 = step2 / 6          // 5
  let step4 = step3 + 1          // 6
  
  assert_eq(step4, 6)
}

test "boundary_values" {
  // 测试边界值
  assert_eq(0 + 0, 0)
  assert_eq(0 * 5, 0)
  assert_eq(5 * 0, 0)
  assert_eq(0 / 1, 0)
  assert_eq(1 - 1, 0)
}

test "character_operations" {
  // 测试字符操作
  let char_a = 'a'
  let char_A = 'A'
  
  assert_eq(char_a.to_string(), "a")
  assert_eq(char_A.to_string(), "A")
  
  // 测试字符比较
  assert_eq('a' < 'b', true)
  assert_eq('A' < 'a', true)
}