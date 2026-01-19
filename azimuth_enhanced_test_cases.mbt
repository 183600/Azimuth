// Azimuth 项目增强测试用例
// 使用标准 MoonBit 测试语法，包含8个测试用例

test "string_concatenation_tests" {
  // 测试字符串连接功能
  assert_eq_string("Hello World", "Hello" + " " + "World")
  assert_eq_string("123456", "123" + "456")
  assert_eq_string("", "" + "")
  assert_eq_string("abc", "a" + "b" + "c")
  assert_eq_string("MoonBit", "Moon" + "Bit")
}

test "arithmetic_operator_precedence" {
  // 测试算术运算符优先级
  assert_eq(14, 2 + 3 * 4)  // 乘法优先于加法
  assert_eq(20, (2 + 3) * 4)  // 括号改变优先级
  assert_eq(10, 20 - 5 * 2)  // 减法和乘法混合
  assert_eq(30, 5 * 3 + 15)  // 乘法和加法混合
  assert_eq(8, 16 / 2 + 0)  // 除法和加法混合
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-5, -2 + -3)
  assert_eq(6, -2 * -3)
  assert_eq(-10, -5 * 2)
  assert_eq(1, -5 + 6)
  assert_eq(-8, -10 + 2)
}

test "zero_operations" {
  // 测试零的运算
  assert_eq(0, 0 + 0)
  assert_eq(0, 0 * 5)
  assert_eq(0, 5 * 0)
  assert_eq(5, 0 + 5)
  assert_eq(5, 5 + 0)
}

test "comparison_operations" {
  // 测试比较操作
  assert_true(5 > 3)
  assert_false(3 > 5)
  assert_true(5 >= 5)
  assert_true(3 < 5)
  assert_false(5 < 3)
  assert_true(3 <= 3)
  assert_true(5 == 5)
  assert_false(5 == 3)
  assert_true(5 != 3)
  assert_false(5 != 5)
}

test "logical_operations" {
  // 测试逻辑运算
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && true)
  assert_false(false && false)
  assert_true(true || true)
  assert_true(true || false)
  assert_true(false || true)
  assert_false(false || false)
  assert_false(!true)
  assert_true(!false)
}

test "complex_expressions" {
  // 测试复杂表达式
  let x = 10
  let y = 5
  let z = 2
  
  // 复杂算术表达式
  assert_eq(25, x * y / 2 + z * 5)
  assert_eq(100, (x + y) * (x - y))
  assert_eq(20, x * y / (z + z))
  
  // 嵌套表达式
  assert_eq(15, x - y + z * 5)
  assert_eq(50, x * (y + z))
}

test "type_coercion_tests" {
  // 测试类型转换和兼容性
  let int_val = 42
  let float_val = 42.0
  
  // 整数运算
  assert_eq(84, int_val + int_val)
  assert_eq(1764, int_val * int_val)
  
  // 混合运算（如果支持）
  // assert_eq(84.0, int_val + float_val)  // 如果支持整数和浮点数混合运算
  // assert_eq(1764.0, int_val * float_val)  // 如果支持整数和浮点数混合运算
}