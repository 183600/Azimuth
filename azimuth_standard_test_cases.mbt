// 标准 MoonBit 测试用例集合
// 包含10个核心功能测试，使用标准的 MoonBit 测试语法

test "int_boundary_values_and_operations" {
  // 测试整数边界值和基本运算
  let max_int = 2147483647
  let min_int = -2147483648
  
  assert_eq(max_int, 2147483647)
  assert_eq(min_int, -2147483648)
  
  // 测试基本算术运算
  assert_eq(100 + 200, 300)
  assert_eq(500 - 150, 350)
  assert_eq(25 * 4, 100)
  assert_eq(81 / 9, 9)
  assert_eq(17 % 5, 2)
  
  // 测试负数运算
  assert_eq(-50 + 25, -25)
  assert_eq(-10 * -3, 30)
  assert_eq(-20 / 4, -5)
}

test "string_operations_and_comparisons" {
  // 测试字符串基本操作
  let hello = "Hello"
  let world = "World"
  let moonbit = "MoonBit"
  
  // 测试字符串连接
  let greeting = hello + " " + world + "!"
  assert_eq(greeting, "Hello World!")
  
  // 测试字符串长度
  assert_eq(hello.length(), 5)
  assert_eq(world.length(), 5)
  assert_eq(moonbit.length(), 7)
  assert_eq(greeting.length(), 12)
  
  // 测试字符串比较
  assert_eq(hello < world, true)
  assert_eq(moonbit > hello, true)
  assert_eq(hello == hello, true)
  assert_eq(hello != world, true)
}

test "array_operations_and_indexing" {
  // 测试数组操作和索引
  let int_array = [10, 20, 30, 40, 50]
  let string_array = ["apple", "banana", "cherry"]
  
  // 测试数组长度
  assert_eq(int_array.length(), 5)
  assert_eq(string_array.length(), 3)
  
  // 测试数组索引访问
  assert_eq(int_array[0], 10)
  assert_eq(int_array[2], 30)
  assert_eq(int_array[4], 50)
  
  assert_eq(string_array[1], "banana")
  assert_eq(string_array[2], "cherry")
  
  // 测试数组元素运算
  let first_plus_last = int_array[0] + int_array[4]
  assert_eq(first_plus_last, 60)
  
  let middle_sum = int_array[1] + int_array[2] + int_array[3]
  assert_eq(middle_sum, 90)
}

test "option_type_pattern_matching" {
  // 测试 Option 类型的模式匹配
  let some_value : Option[Int] = Some(100)
  let none_value : Option[Int] = None
  let some_string : Option[String] = Some("MoonBit")
  let none_string : Option[String] = None
  
  // 测试 Some 值提取
  let extracted_int = match some_value {
    Some(x) => x
    None => 0
  }
  assert_eq(extracted_int, 100)
  
  // 测试 None 默认值
  let default_int = match none_value {
    Some(x) => x
    None => -1
  }
  assert_eq(default_int, -1)
  
  // 测试字符串 Option
  let extracted_string = match some_string {
    Some(s) => s
    None => "default"
  }
  assert_eq(extracted_string, "MoonBit")
  
  // 测试字符串 None
  let default_string = match none_string {
    Some(s) => s
    None => "empty"
  }
  assert_eq(default_string, "empty")
}

test "boolean_logic_operations" {
  // 测试布尔逻辑操作
  let true_val = true
  let false_val = false
  
  // 测试基本逻辑运算
  assert_eq(true_val && true_val, true)
  assert_eq(true_val && false_val, false)
  assert_eq(false_val && true_val, false)
  assert_eq(false_val && false_val, false)
  
  assert_eq(true_val || true_val, true)
  assert_eq(true_val || false_val, true)
  assert_eq(false_val || true_val, true)
  assert_eq(false_val || false_val, false)
  
  // 测试逻辑非
  assert_eq(!true_val, false)
  assert_eq(!false_val, true)
  
  // 测试比较操作
  assert_eq(true_val == true_val, true)
  assert_eq(true_val != false_val, true)
  assert_eq(true_val == false_val, false)
  
  // 测试数值比较的布尔结果
  assert_eq(5 > 3, true)
  assert_eq(2 < 1, false)
  assert_eq(10 >= 10, true)
  assert_eq(8 <= 5, false)
}

test "character_operations_and_comparisons" {
  // 测试字符操作和比较
  let char_a = 'a'
  let char_z = 'z'
  let char_A = 'A'
  let char_Z = 'Z'
  let char_0 = '0'
  let char_9 = '9'
  
  // 测试字符转换
  assert_eq(char_a.to_string(), "a")
  assert_eq(char_A.to_string(), "A")
  assert_eq(char_0.to_string(), "0")
  
  // 测试字符比较
  assert_eq(char_a < char_z, true)
  assert_eq(char_A < char_Z, true)
  assert_eq(char_0 < char_9, true)
  
  // 测试大小写比较
  assert_eq(char_A < char_a, true)
  assert_eq(char_Z < char_z, true)
  
  // 测试数字字符和字母字符比较
  assert_eq(char_0 < char_A, true)
  assert_eq(char_9 < char_a, true)
  
  // 测试字符相等性
  assert_eq(char_a == 'a', true)
  assert_eq(char_A != char_a, true)
  assert_eq(char_0 == '0', true)
}

test "result_type_error_handling" {
  // 测试 Result 类型的错误处理
  let success_int : Result[Int, String] = Ok(42)
  let error_int : Result[Int, String] = Err("Invalid input")
  let success_string : Result[String, Int] = Ok("Success")
  let error_string : Result[String, Int] = Err(404)
  
  // 测试成功结果处理
  let success_value = match success_int {
    Ok(value) => value
    Err(_) => 0
  }
  assert_eq(success_value, 42)
  
  // 测试错误结果处理
  let error_message = match error_int {
    Ok(_) => "No error"
    Err(msg) => msg
  }
  assert_eq(error_message, "Invalid input")
  
  // 测试字符串结果处理
  let success_msg = match success_string {
    Ok(msg) => msg
    Err(_) => "Error occurred"
  }
  assert_eq(success_msg, "Success")
  
  // 测试错误代码处理
  let error_code = match error_string {
    Ok(_) => 0
    Err(code) => code
  }
  assert_eq(error_code, 404)
}

test "complex_pattern_matching" {
  // 测试复杂的模式匹配
  let tuple1 = (1, "one", true)
  let tuple2 = (42, "answer", false)
  let nested_option : Option[Option[Int]] = Some(Some(100))
  let nested_none : Option[Option[Int]] = Some(None)
  
  // 测试元组模式匹配
  let first_result = match tuple1 {
    (x, _, _) => x + 1
  }
  assert_eq(first_result, 2)
  
  let second_result = match tuple2 {
    (x, _, z) => if z then x else x * 2
  }
  assert_eq(second_result, 84)
  
  // 测试嵌套 Option 模式匹配
  let nested_value = match nested_option {
    Some(Some(x)) => x
    Some(None) => 0
    None => -1
  }
  assert_eq(nested_value, 100)
  
  let nested_default = match nested_none {
    Some(Some(x)) => x
    Some(None) => 0
    None => -1
  }
  assert_eq(nested_default, 0)
  
  // 测试条件模式匹配
  let conditional_result = match tuple1 {
    (x, _, true) => x * 10
    (x, _, false) => x + 10
  }
  assert_eq(conditional_result, 10)
}

test "type_conversions_and_casting" {
  // 测试类型转换和强制转换
  let int_value = 123
  let bool_value = true
  
  // 测试整数到字符串转换
  let int_to_string = int_value.to_string()
  assert_eq(int_to_string, "123")
  
  // 测试布尔到字符串转换
  let bool_to_string = bool_value.to_string()
  assert_eq(bool_to_string, "true")
  
  // 测试字符到字符串转换
  let char_to_string = 'X'.to_string()
  assert_eq(char_to_string, "X")
  
  // 测试数值比较和类型一致性
  assert_eq(int_value > 100, true)
  assert_eq(int_value < 200, true)
  assert_eq(int_value == 123, true)
  assert_eq(int_value != 456, true)
  
  // 测试布尔逻辑
  assert_eq(bool_value && true, true)
  assert_eq(bool_value || false, true)
  assert_eq(!bool_value, false)
  
  // 测试字符串到数值的间接验证
  let string_num = "42"
  let string_length = string_num.length()
  assert_eq(string_length, 2)
}

test "list_basic_operations" {
  // 测试列表基本操作
  let empty_list : Array[Int] = []
  let single_element = [42]
  let multiple_elements = [1, 2, 3, 4, 5]
  
  // 测试列表长度
  assert_eq(empty_list.length(), 0)
  assert_eq(single_element.length(), 1)
  assert_eq(multiple_elements.length(), 5)
  
  // 测试列表访问
  assert_eq(multiple_elements[0], 1)
  assert_eq(multiple_elements[2], 3)
  assert_eq(multiple_elements[4], 5)
  
  // 测试列表操作
  let new_list = [0] + multiple_elements
  assert_eq(new_list.length(), 6)
  assert_eq(new_list[0], 0)
  
  // 测试列表求和
  let sum = multiple_elements[0] + multiple_elements[1] + multiple_elements[2] + multiple_elements[3] + multiple_elements[4]
  assert_eq(sum, 15)
}