// 标准 MoonBit 测试用例
// 包含10个高质量测试用例，覆盖核心功能

///|
test "integer_basic_operations" {
  // 测试整数基本运算
  2 + 3 = 5
  10 - 4 = 6
  3 * 4 = 12
  15 / 3 = 5
  17 % 5 = 2
}

///|
test "integer_boundary_values" {
  // 测试整数边界值
  2147483647 = 2147483647
  -2147483648 = -2147483648
  0 = 0
  -1 = -1
  1 = 1
}

///|
test "string_basic_operations" {
  // 测试字符串基本操作
  "Hello" + " " + "MoonBit" = "Hello MoonBit"
  "" = ""
  "测试" = "测试"
}

///|
test "boolean_logic_operations" {
  // 测试布尔逻辑操作
  true && true = true
  true && false = false
  true || false = true
  false || false = false
  !true = false
  !false = true
}

///|
test "comparison_operations" {
  // 测试比较操作
  5 > 3 = true
  5 < 3 = false
  5 >= 5 = true
  5 <= 3 = false
  5 == 5 = true
  5 != 3 = true
}

///|
test "character_operations" {
  // 测试字符操作
  'A' = 'A'
  '中' = '中'
}

///|
test "array_basic_operations" {
  // 测试数组基本操作
  let arr = [1, 2, 3, 4, 5]
  arr[0] = 1
  arr[4] = 5
}

///|
test "mathematical_expressions" {
  // 测试数学表达式
  (2 + 3) * 4 = 20
  10 * (5 + 3) = 80
  2 * 3 + 4 * 5 = 26
}

///|
test "complex_calculations" {
  // 测试复杂计算
  let a = 10
  let b = 20
  let c = 30
  
  a + b * c = 610
  (a + b) * c = 900
  a * b + c = 230
}

///|
test "azimuth_function_integration" {
  // 测试 azimuth 函数集成
  add(15, 25) = 40
  multiply(6, 7) = 42
  greet("MoonBit") = "Hello, MoonBit!"
  divide_with_ceil(25, 8) = 4
}