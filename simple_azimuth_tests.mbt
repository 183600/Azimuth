// 简单的 MoonBit 测试用例
// 包含10个核心功能测试

test "int_basic_operations" {
  assert_eq(1 + 1, 2)
  assert_eq(10 - 5, 5)
  assert_eq(3 * 4, 12)
  assert_eq(8 / 2, 4)
}

test "string_basic_operations" {
  let hello = "Hello"
  let world = "World"
  let greeting = hello + " " + world
  assert_eq(greeting, "Hello World")
  assert_eq(hello.length(), 5)
}

test "array_basic_operations" {
  let arr = [1, 2, 3, 4, 5]
  assert_eq(arr.length(), 5)
  assert_eq(arr[0], 1)
  assert_eq(arr[4], 5)
}

test "option_type_handling" {
  let some_value : Option[Int] = Some(42)
  let none_value : Option[Int] = None
  
  let extracted = match some_value {
    Some(x) => x
    None => 0
  }
  assert_eq(extracted, 42)
  
  let default = match none_value {
    Some(x) => x
    None => -1
  }
  assert_eq(default, -1)
}

test "boolean_logic" {
  assert_eq(true && true, true)
  assert_eq(true && false, false)
  assert_eq(true || false, true)
  assert_eq(false || false, false)
  assert_eq(!true, false)
  assert_eq(!false, true)
}

test "character_operations" {
  let char_a = 'a'
  let char_A = 'A'
  assert_eq(char_a.to_string(), "a")
  assert_eq(char_A.to_string(), "A")
  assert_eq(char_a < char_A, false)
  assert_eq(char_A < char_a, true)
}

test "result_type_handling" {
  let success : Result[Int, String] = Ok(100)
  let error : Result[Int, String] = Err("Error")
  
  let success_value = match success {
    Ok(x) => x
    Err(_) => 0
  }
  assert_eq(success_value, 100)
  
  let error_message = match error {
    Ok(_) => "No error"
    Err(msg) => msg
  }
  assert_eq(error_message, "Error")
}

test "pattern_matching" {
  let tuple = (42, "answer")
  let first = match tuple {
    (x, _) => x
  }
  assert_eq(first, 42)
  
  let second = match tuple {
    (_, y) => y
  }
  assert_eq(second, "answer")
}

test "type_conversions" {
  let int_val = 123
  let bool_val = true
  
  assert_eq(int_val.to_string(), "123")
  assert_eq(bool_val.to_string(), "true")
  assert_eq('X'.to_string(), "X")
}

test "comparisons" {
  assert_eq(5 > 3, true)
  assert_eq(2 < 1, false)
  assert_eq(10 >= 10, true)
  assert_eq(8 <= 5, false)
  assert_eq("apple" == "apple", true)
  assert_eq("apple" != "banana", true)
}