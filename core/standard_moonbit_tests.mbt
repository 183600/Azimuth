// 标准 MoonBit 测试用例
// 测试核心功能模块

test "int_absolute_value" {
  // 测试整数绝对值功能
  assert_eq(5, Int::abs(5))
  assert_eq(5, Int::abs(-5))
  assert_eq(0, Int::abs(0))
  assert_eq(2147483647, Int::abs(2147483647))
  assert_eq(-2147483648, Int::abs(-2147483648))
}

test "string_basic_operations" {
  // 测试字符串基本操作
  let empty = ""
  let hello = "Hello"
  let world = "World"
  
  assert_eq(0, empty.length())
  assert_eq(5, hello.length())
  assert_eq(5, world.length())
  
  assert_eq(true, (hello.empty()).not())
  assert_eq(true, empty.empty())
}

test "list_basic_operations" {
  // 测试列表基本操作
  let list1 = @list.from_array([1, 2, 3])
  let list2 = @list.from_array([4, 5, 6])
  
  // Test length
  assert_eq(list1.length(), 3)
  assert_eq(list2.length(), 3)
  
  // Test empty
  let empty_list = @list.from_array([])
  assert_eq(true, empty_list.is_empty())
  assert_eq(false, list1.is_empty())
}

test "option_type_operations" {
  // 测试Option类型操作
  let some_value = @option.Some(42)
  let none_value = @option.None
  
  assert_eq(true, some_value.is_some())
  assert_eq(false, some_value.is_none())
  assert_eq(false, none_value.is_some())
  assert_eq(true, none_value.is_none())
  
  assert_eq(42, some_value.unwrap_or(0))
  assert_eq(0, none_value.unwrap_or(0))
}

test "result_type_operations" {
  // 测试Result类型操作
  let ok_value = @result.Ok(100)
  let err_value = @result.Err("error")
  
  assert_eq(true, ok_value.is_ok())
  assert_eq(false, ok_value.is_err())
  assert_eq(false, err_value.is_ok())
  assert_eq(true, err_value.is_err())
  
  assert_eq(100, ok_value.unwrap_or(0))
  assert_eq(0, err_value.unwrap_or(0))
}

test "char_type_operations" {
  // 测试字符类型操作
  let digit_char = '5'
  let letter_char = 'A'
  let space_char = ' '
  
  assert_eq(true, digit_char.is_digit())
  assert_eq(false, letter_char.is_digit())
  assert_eq(false, space_char.is_digit())
  
  assert_eq(false, digit_char.is_alpha())
  assert_eq(true, letter_char.is_alpha())
  assert_eq(false, space_char.is_alpha())
  
  assert_eq(false, digit_char.is_whitespace())
  assert_eq(false, letter_char.is_whitespace())
  assert_eq(true, space_char.is_whitespace())
}

test "array_basic_operations" {
  // 测试数组基本操作
  let empty_array : Array[Int] = []
  let num_array = [1, 2, 3, 4, 5]
  
  assert_eq(0, empty_array.length())
  assert_eq(5, num_array.length())
  
  assert_eq(true, empty_array.is_empty())
  assert_eq(false, num_array.is_empty())
  
  assert_eq(1, num_array[0])
  assert_eq(5, num_array[4])
}

test "int_boundary_values" {
  // 测试整数边界值
  assert_eq(Int::max_value, 2147483647)
  assert_eq(Int::min_value, -2147483648)
  
  let max_val = Int::max_value
  let min_val = Int::min_value
  
  assert_eq(max_val + 1, -2147483648)
  assert_eq(min_val - 1, 2147483647)
}

test "math_basic_operations" {
  // 测试基本数学运算
  let x = 10
  let y = 3
  
  assert_eq(x + y, 13)
  assert_eq(x - y, 7)
  assert_eq(x * y, 30)
  assert_eq(x / y, 3)
  assert_eq(x % y, 1)
}

test "string_concatenation" {
  // 测试字符串连接
  let str1 = "Hello"
  let str2 = "World"
  let combined = str1 + ", " + str2 + "!"
  
  assert_eq("Hello, World!", combined)
  
  let empty = ""
  let with_empty = empty + "test"
  assert_eq("test", with_empty)
}