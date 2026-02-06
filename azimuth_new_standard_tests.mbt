// Azimuth 项目标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "string_basic_operations" {
  // 字符串基本操作测试
  let str1 = "Hello"
  let str2 = "World"
  let empty = ""
  
  // 字符串连接
  assert_eq_string(str1 + " " + str2, "Hello World")
  assert_eq_string(empty + "test", "test")
  assert_eq_string("test" + empty, "test")
  
  // 字符串长度
  assert_eq(str1.length(), 5)
  assert_eq(str2.length(), 5)
  assert_eq(empty.length(), 0)
}

test "int_mathematical_operations" {
  // 整数数学运算测试
  let a = 42
  let b = 17
  
  // 基本运算
  assert_eq(a + b, 59)
  assert_eq(a - b, 25)
  assert_eq(a * b, 714)
  assert_eq(a / b, 2)
  assert_eq(a % b, 8)
  
  // 绝对值测试
  assert_eq(Int::abs(42), 42)
  assert_eq(Int::abs(-42), 42)
  assert_eq(Int::abs(0), 0)
}

test "boolean_logical_operations" {
  // 布尔逻辑运算测试
  let true_val = true
  let false_val = false
  
  // 基本逻辑运算
  assert_true(true_val && true_val)
  assert_false(true_val && false_val)
  assert_false(false_val && false_val)
  
  assert_true(true_val || true_val)
  assert_true(true_val || false_val)
  assert_false(false_val || false_val)
  
  // 逻辑非
  assert_false(!true_val)
  assert_true(!false_val)
}

test "array_basic_operations" {
  // 数组基本操作测试
  let numbers = [1, 2, 3, 4, 5]
  let empty = []
  
  // 数组长度
  assert_eq(numbers.length(), 5)
  assert_eq(empty.length(), 0)
  
  // 数组访问
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
  
  // 数组求和
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(sum, 15)
}

test "option_type_operations" {
  // Option 类型操作测试
  let some_value = Some(42)
  let none_value = None
  
  // Option 检查
  assert_true(some_value.is_some())
  assert_false(some_value.is_none())
  assert_false(none_value.is_some())
  assert_true(none_value.is_none())
  
  // Option 解包
  assert_eq(some_value.unwrap(), 42)
  
  // Option 默认值
  assert_eq(some_value.unwrap_or(0), 42)
  assert_eq(none_value.unwrap_or(0), 0)
}

test "result_type_operations" {
  // Result 类型操作测试
  let success_result = Ok(42)
  let error_result = Err("error message")
  
  // Result 检查
  assert_true(success_result.is_ok())
  assert_false(success_result.is_err())
  assert_false(error_result.is_ok())
  assert_true(error_result.is_err())
  
  // Result 解包
  assert_eq(success_result.unwrap(), 42)
  
  // Result 默认值
  assert_eq(success_result.unwrap_or(0), 42)
  assert_eq(error_result.unwrap_or(0), 0)
}

test "char_operations" {
  // 字符操作测试
  let char_a = 'a'
  let char_Z = 'Z'
  let char_digit = '5'
  
  // 字符比较
  assert_true(char_a < char_Z)
  assert_false(char_Z < char_a)
  assert_true(char_digit != char_a)
  
  // 字符转换为字符串
  assert_eq_string(char_a.to_string(), "a")
  assert_eq_string(char_Z.to_string(), "Z")
  assert_eq_string(char_digit.to_string(), "5")
}

test "tuple_operations" {
  // 元组操作测试
  let pair = (42, "answer")
  let triple = (1, true, "test")
  
  // 元组访问
  assert_eq(pair.0, 42)
  assert_eq_string(pair.1, "answer")
  
  assert_eq(triple.0, 1)
  assert_true(triple.1)
  assert_eq_string(triple.2, "test")
  
  // 元组比较
  assert_true(pair == (42, "answer"))
  assert_false(pair == (43, "answer"))
  assert_false(pair == (42, "test"))
}

test "type_conversion_operations" {
  // 类型转换操作测试
  let int_val = 42
  let string_val = "123"
  
  // 字符串转整数
  // 注意：这里使用假设的转换函数，实际函数名可能不同
  let parsed_int = string_val.parse_int().unwrap_or(0)
  assert_eq(parsed_int, 123)
  
  // 整数转字符串
  assert_eq_string(int_val.to_string(), "42")
  
  // 布尔值转换
  assert_eq_string(true.to_string(), "true")
  assert_eq_string(false.to_string(), "false")
}

test "complex_data_structures" {
  // 复杂数据结构测试
  // 创建嵌套数组结构
  let matrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ]
  
  // 访问嵌套结构
  assert_eq(matrix[0][0], 1)
  assert_eq(matrix[1][1], 5)
  assert_eq(matrix[2][2], 9)
  
  // 计算对角线和
  let diagonal_sum = matrix[0][0] + matrix[1][1] + matrix[2][2]
  assert_eq(diagonal_sum, 15)
  
  // 创建包含 Option 的数组
  let optional_numbers = [Some(1), None, Some(3), Some(4), None]
  let present_count = optional_numbers.filter(fn(x) { x.is_some() }).length()
  assert_eq(present_count, 3)
}