// 标准 MoonBit 核心模块测试用例
// 测试核心功能模块的基本操作

// 导入必要的函数
using @builtin { inspect }

test "int_basic_operations" {
  // 测试整数基本操作
  let a = 2147483647
  let b = -2147483648
  inspect(a, content="2147483647")
  inspect(b, content="-2147483648")
  inspect(a + 1, content="-2147483648")
  inspect(b - 1, content="2147483647")
}

test "string_basic_operations" {
  // 测试字符串基本操作
  let s1 = "Hello"
  let s2 = "World"
  let combined = s1 + " " + s2
  inspect(combined, content="Hello World")
  inspect(combined.length(), content="11")
  inspect(s1.length(), content="5")
  inspect(s2.length(), content="5")
}

test "array_basic_operations" {
  // 测试数组基本操作
  let arr = [1, 2, 3, 4, 5]
  inspect(arr.length(), content="5")
  inspect(arr[0], content="1")
  inspect(arr[4], content="5")
  inspect(arr.length() > 0, content="true")
}

test "array_operations" {
  // 测试数组操作
  let arr1 = [1, 2, 3]
  let arr2 = [4, 5, 6]
  let combined = arr1 + arr2
  inspect(combined, content="[1, 2, 3, 4, 5, 6]")
  inspect(combined.length(), content="6")
}

test "option_type_operations" {
  // 测试 Option 类型操作
  let some_value : Option[Int] = Some(42)
  let none_value : Option[Int] = None
  
  let result1 = match some_value {
    Some(v) => v
    None => 0
  }
  
  let result2 = match none_value {
    Some(v) => v
    None => 0
  }
  
  inspect(result1, content="42")
  inspect(result2, content="0")
}

test "option_pattern_matching" {
  // 测试 Option 模式匹配
  let maybe_value = Some(10)
  let doubled = match maybe_value {
    Some(x) => x * 2
    None => 0
  }
  inspect(doubled, content="20")
  
  let empty_value = None
  let empty_result = match empty_value {
    Some(x) => x * 2
    None => 0
  }
  inspect(empty_result, content="0")
}

test "result_type_operations" {
  // 测试 Result 类型操作
  let success : Result[String, String] = Ok("success")
  let failure : Result[String, String] = Err("error")
  
  let success_result = match success {
    Ok(v) => v
    Err(e) => "default"
  }
  
  let failure_result = match failure {
    Ok(v) => v
    Err(e) => "default"
  }
  
  inspect(success_result, content="success")
  inspect(failure_result, content="default")
}

test "char_operations" {
  // 测试字符操作
  let upper_char = 'A'
  let lower_char = 'a'
  let digit_char = '5'
  
  inspect(upper_char.to_int(), content="65")
  inspect(lower_char.to_int(), content="97")
  inspect(digit_char.to_int(), content="53")
}