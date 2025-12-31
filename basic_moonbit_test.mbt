// 简单的MoonBit测试用例，确保语法正确

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  
  let a = 10
  let b = 5
  
  // 测试加法
  assert_eq(a + b, 15)
  
  // 测试减法
  assert_eq(a - b, 5)
  
  // 测试乘法
  assert_eq(a * b, 50)
  
  // 测试除法
  assert_eq(a / b, 2)
  
  // 测试比较运算
  assert_eq(a > b, true)
  assert_eq(a < b, false)
  assert_eq(a >= b, true)
  assert_eq(a <= b, false)
  assert_eq(a == b, false)
  assert_eq(a != b, true)
}

test "string_operations" {
  // 测试字符串操作
  
  let str1 = "hello"
  let str2 = "world"
  let str3 = "hello"
  
  // 测试字符串连接
  let combined = str1 + " " + str2
  assert_eq(combined, "hello world")
  
  // 测试字符串长度
  assert_eq(str1.length(), 5)
  assert_eq(combined.length(), 11)
  
  // 测试字符串比较
  assert_eq(str1 == str3, true)
  assert_eq(str1 == str2, false)
  
  // 测试字符串前缀和后缀
  assert_eq(combined.has_prefix("hello"), true)
  assert_eq(combined.has_suffix("world"), true)
  assert_eq(combined.has_prefix("world"), false)
  
  // 测试字符串包含
  assert_eq(combined.contains("lo wo"), true)
  assert_eq(combined.contains("xyz"), false)
}

test "array_operations" {
  // 测试数组操作
  
  let numbers = [1, 2, 3, 4, 5]
  let strings = ["apple", "banana", "cherry"]
  
  // 测试数组长度
  assert_eq(numbers.length(), 5)
  assert_eq(strings.length(), 3)
  
  // 测试数组索引
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
  assert_eq(strings[1], "banana")
  
  // 测试数组元素访问
  let first_num = numbers[0]
  let last_str = strings[2]
  assert_eq(first_num, 1)
  assert_eq(last_str, "cherry")
  
  // 测试数组创建
  let empty_array : Array[Int] = []
  assert_eq(empty_array.length(), 0)
}

test "boolean_logic" {
  // 测试布尔逻辑
  
  let true_val = true
  let false_val = false
  
  // 测试基本值
  assert_eq(true_val, true)
  assert_eq(false_val, false)
  
  // 测试逻辑运算
  assert_eq(true_val && false_val, false)
  assert_eq(true_val || false_val, true)
  assert_eq(!true_val, false)
  assert_eq(!false_val, true)
  
  // 测试复杂逻辑表达式
  assert_eq((true_val && true_val) || false_val, true)
  assert_eq((false_val || false_val) && true_val, false)
}

test "type_conversions" {
  // 测试类型转换
  
  let int_val = 42
  let double_val = 3.14
  let string_val = "123"
  
  // 测试整数到字符串转换
  let int_to_string = int_val.to_string()
  assert_eq(int_to_string, "42")
  
  // 测试浮点数到字符串转换
  let double_to_string = double_val.to_string()
  assert_eq(double_to_string.contains("3.14"), true)
  
  // 测试字符串到整数转换
  let string_to_int = string_val.to_int()
  assert_eq(string_to_int, 123)
  
  // 测试整数到浮点数转换
  let int_to_double = int_val.to_double()
  assert_eq(int_to_double, 42.0)
}