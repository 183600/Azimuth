// MoonBit 标准测试用例
// 覆盖核心功能模块

///|
test "integer_basic_operations" {
  // 测试整数基本操作
  let a = 42
  let b = 17
  
  // 加法
  let sum = a + b
  inspect(sum, content="59")
  
  // 减法
  let diff = a - b
  inspect(diff, content="25")
  
  // 乘法
  let product = a * b
  inspect(product, content="714")
  
  // 除法
  let quotient = a / b
  inspect(quotient, content="2")
}

///|
test "string_basic_operations" {
  // 测试字符串基本操作
  let greeting = "Hello"
  let name = "MoonBit"
  
  // 字符串连接
  let message = greeting + ", " + name + "!"
  inspect(message, content="Hello, MoonBit!")
  
  // 字符串长度
  let len = message.length()
  inspect(len, content="15")
  
  // 空字符串
  let empty = ""
  inspect(empty.length(), content="0")
}

///|
test "boolean_logic_operations" {
  // 测试布尔逻辑操作
  let true_val = true
  let false_val = false
  
  // 基本逻辑运算
  inspect(true_val && false_val, content="false")
  inspect(true_val || false_val, content="true")
  inspect(!true_val, content="false")
  inspect(!false_val, content="true")
  
  // 复合逻辑
  let complex = (true_val && false_val) || (true_val || false_val)
  inspect(complex, content="true")
}

///|
test "comparison_operations" {
  // 测试比较操作
  let a = 10
  let b = 20
  let c = 10
  
  // 基本比较
  inspect(a < b, content="true")
  inspect(a > b, content="false")
  inspect(a <= b, content="true")
  inspect(a >= b, content="false")
  inspect(a == c, content="true")
  inspect(a != b, content="true")
  
  // 字符串比较
  let str1 = "apple"
  let str2 = "banana"
  inspect(str1 < str2, content="true")
}

///|
test "mathematical_functions" {
  // 测试数学函数
  let x = -5
  let y = 3.14
  
  // 绝对值
  let abs_x = Int::abs(x)
  inspect(abs_x, content="5")
  
  // 基本数学运算
  let power = 2 * 2 * 2
  inspect(power, content="8")
  
  // 模运算
  let remainder = 17 % 5
  inspect(remainder, content="2")
}

///|
test "type_conversions" {
  // 测试类型转换
  let int_val = 42
  let byte_val = int_val.to_byte()
  
  // 字符转换
  let char_val = 'A'
  let char_as_int = char_val.to_int()
  inspect(char_as_int, content="65")
  
  // 数值转换
  let small_int = 100
  let as_byte = small_int.to_byte()
  inspect(as_byte.to_int(), content="100")
}

///|
test "array_operations" {
  // 测试数组操作
  let numbers = [1, 2, 3, 4, 5]
  
  // 数组长度
  inspect(numbers.length(), content="5")
  
  // 数组访问
  inspect(numbers[0], content="1")
  inspect(numbers[4], content="5")
  
  // 数组操作
  let first = numbers[0]
  let last = numbers[numbers.length() - 1]
  inspect(first + last, content="6")
}

///|
test "option_type_handling" {
  // 测试Option类型处理
  let some_value = Some(42)
  let none_value = None
  
  // Option检查
  inspect(some_value.is_some(), content="true")
  inspect(none_value.is_some(), content="false")
  inspect(some_value.is_none(), content="false")
  inspect(none_value.is_none(), content="true")
  
  // Option值提取
  let extracted = match some_value {
    Some(x) => x * 2
    None => 0
  }
  inspect(extracted, content="84")
}

///|
test "result_type_operations" {
  // 测试Result类型操作
  let success_result = Ok(42)
  let error_result = Err("Error message")
  
  // Result检查
  inspect(success_result.is_ok(), content="true")
  inspect(error_result.is_ok(), content="false")
  inspect(success_result.is_err(), content="false")
  inspect(error_result.is_err(), content="true")
  
  // Result值处理
  let processed = match success_result {
    Ok(value) => value * 3
    Err(_) => 0
  }
  inspect(processed, content="126")
}

///|
test "edge_cases_and_boundaries" {
  // 测试边界情况和特殊情况
  let max_int = Int::max_value
  let min_int = Int::min_value
  
  // 边界值检查
  inspect(max_int, content="2147483647")
  inspect(min_int, content="-2147483648")
  
  // 零值操作
  let zero = 0
  inspect(zero + 1, content="1")
  inspect(zero * 100, content="0")
  
  // 特殊字符串
  let special_chars = "!@#$%^&*()"
  inspect(special_chars.length(), content="10")
  
  // 空值处理
  let empty_string = ""
  inspect(empty_string.length(), content="0")
  inspect(empty_string == "", content="true")
}