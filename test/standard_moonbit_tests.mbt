// 标准 MoonBit 测试用例
// 包含基础数据类型和操作的测试

///|
test "整数基础运算" {
  inspect(10 + 5, content="15")
  inspect(10 - 3, content="7")
  inspect(6 * 7, content="42")
  inspect(20 / 4, content="5")
  inspect(15 % 4, content="3")
}

///|
test "布尔逻辑运算" {
  inspect(true && false, content="false")
  inspect(true || false, content="true")
  inspect(!true, content="false")
  inspect(!false, content="true")
  inspect(true == true, content="true")
  inspect(true != false, content="true")
}

///|
test "字符串操作" {
  let s1 = "Hello"
  let s2 = "World"
  let s3 = s1 + " " + s2
  inspect(s3, content="\"Hello World\"")
  inspect(s3.length(), content="11")
  inspect(s1, content="\"Hello\"")
  inspect(s1 != s2, content="true")
}

///|
test "数组基础操作" {
  let arr = [1, 2, 3, 4, 5]
  inspect(arr.length(), content="5")
  inspect(arr[0], content="1")
  inspect(arr[4], content="5")
}

///|
test "Option 类型处理" {
  let some_value = Some(42)
  let none_value : Option[Int] = None
  inspect(some_value, content="Some(42)")
  inspect(none_value, content="None")
  inspect(some_value.unwrap(), content="42")
}

///|
test "Result 类型处理" {
  let ok_result = Ok("success")
  let err_result = Err("error")
  inspect(ok_result, content="Ok(\"success\")")
  inspect(err_result, content="Err(\"error\")")
  inspect(ok_result.unwrap(), content="\"success\"")
}

///|
test "比较操作符" {
  inspect(10 > 5, content="true")
  inspect(5 < 10, content="true")
  inspect(10 >= 10, content="true")
  inspect(5 <= 10, content="true")
  inspect(10 == 10, content="true")
  inspect(5 != 10, content="true")
}

///|
test "位运算操作" {
  inspect(5 & 3, content="1")
  inspect(5 | 3, content="7")
  inspect(5 ^ 3, content="6")
  inspect(~5, content="-6")
  inspect(5 << 1, content="10")
  inspect(10 >> 1, content="5")
}

///|
test "类型转换" {
  inspect(42.to_string(), content="\"42\"")
  inspect(true.to_string(), content="\"true\"")
  inspect(3.14.to_int(), content="3")
  inspect(42.to_double(), content="42.0")
}

///|
test "条件表达式" {
  let x = 10
  let result = if x > 5 { "greater" } else { "less or equal" }
  
  let y = 3
  let result2 = if y > 5 { "greater" } else { "less or equal" }
  
  inspect(result, content="\"greater\"")
  inspect(result2, content="\"less or equal\"")
}