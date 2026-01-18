// Azimuth 项目增强测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

// 导入必要的模块
use azimuth

test "array_operations" {
  // 数组操作测试
  let numbers = [1, 2, 3, 4, 5]
  let first = numbers[0]
  let last = numbers[4]
  let length = numbers.length()
  
  @assertion.assert_eq(1, first)?
  @assertion.assert_eq(5, last)?
  @assertion.assert_eq(5, length)?
}

test "boolean_logic" {
  // 布尔逻辑测试
  let true_val = true
  let false_val = false
  
  // 测试基本逻辑运算
  let and_result = true_val && false_val
  let or_result = true_val || false_val
  let not_result = !false_val
  
  @assertion.assert_eq(false, and_result)?
  @assertion.assert_eq(true, or_result)?
  @assertion.assert_eq(true, not_result)?
}

test "conditional_expressions" {
  // 条件表达式测试
  let x = 10
  let y = 20
  
  // 测试三元条件表达式
  let max_value = if (x > y) { x } else { y }
  let min_value = if (x < y) { x } else { y }
  let equal_check = if (x == y) { true } else { false }
  
  @assertion.assert_eq(20, max_value)?
  @assertion.assert_eq(10, min_value)?
  @assertion.assert_eq(false, equal_check)?
}

test "string_length_operations" {
  // 字符串长度操作测试
  let short_string = "Hi"
  let medium_string = "Hello"
  let long_string = "Hello, World!"
  let unicode_string = "你好世界"
  
  // 测试字符串长度
  @assertion.assert_eq(2, short_string.length())?
  @assertion.assert_eq(5, medium_string.length())?
  @assertion.assert_eq(13, long_string.length())?
  @assertion.assert_eq(4, unicode_string.length())?
}

test "modulo_operations" {
  // 取模运算测试
  let dividend1 = 10
  let divisor1 = 3
  let dividend2 = 20
  let divisor2 = 5
  let dividend3 = 15
  let divisor3 = 4
  
  // 测试取模运算
  let remainder1 = dividend1 % divisor1
  let remainder2 = dividend2 % divisor2
  let remainder3 = dividend3 % divisor3
  
  @assertion.assert_eq(1, remainder1)?
  @assertion.assert_eq(0, remainder2)?
  @assertion.assert_eq(3, remainder3)?
}

test "function_composition" {
  // 函数组合测试
  let base_value = 5
  
  // 嵌套函数调用
  let step1 = azimuth.multiply(base_value, 3)  // 5 * 3 = 15
  let step2 = azimuth.add(step1, 10)           // 15 + 10 = 25
  let step3 = azimuth.multiply(step2, 2)       // 25 * 2 = 50
  let final_result = azimuth.add(step3, -5)    // 50 - 5 = 45
  
  @assertion.assert_eq(45, final_result)?
}

test "error_handling" {
  // 错误处理测试
  let valid_input = 42
  let invalid_input = -1
  
  // 测试有效输入
  let valid_result = azimuth.process(valid_input)
  @assertion.assert_eq(true, valid_result.is_ok())?
  
  // 测试无效输入
  let invalid_result = azimuth.process(invalid_input)
  @assertion.assert_eq(true, invalid_result.is_error())?
}

test "option_type_handling" {
  // Option 类型处理测试
  let some_value = some(42)
  let none_value = none
  
  // 测试 some 值
  @assertion.assert_eq(true, some_value.is_some())?
  @assertion.assert_eq(false, some_value.is_none())?
  
  // 测试 none 值
  @assertion.assert_eq(false, none_value.is_some())?
  @assertion.assert_eq(true, none_value.is_none())?
}

test "list_operations" {
  // 列表操作测试
  let empty_list = []
  let single_item = [42]
  let multiple_items = [1, 2, 3, 4, 5]
  
  // 测试列表长度
  @assertion.assert_eq(0, empty_list.length())?
  @assertion.assert_eq(1, single_item.length())?
  @assertion.assert_eq(5, multiple_items.length())?
  
  // 测试列表头部和尾部
  @assertion.assert_eq(42, single_item.head())?
  @assertion.assert_eq(1, multiple_items.head())?
}

test "type_conversion" {
  // 类型转换测试
  let int_value = 42
  let string_value = "42"
  let float_value = 42.0
  
  // 测试整数到字符串转换
  let int_to_string = int_value.to_string()
  @assertion.assert_eq_string("42", int_to_string)?
  
  // 测试字符串到整数转换
  let string_to_int = string_value.to_int()
  @assertion.assert_eq(42, string_to_int)?
  
  // 测试整数到浮点数转换
  let int_to_float = int_value.to_float()
  @assertion.assert_eq(42.0, int_to_float)?
}