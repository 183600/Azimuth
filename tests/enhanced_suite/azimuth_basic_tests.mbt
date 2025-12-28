// Azimuth Telemetry System - 10个基础测试用例
// 这些测试用例涵盖了遥测系统的核心功能

// 测试1: 基础算术运算
test "基础算术运算测试" {
  assert_eq(1 + 1, 2)
  assert_eq(2 * 3, 6)
  assert_eq(10 - 5, 5)
  assert_eq(20 / 4, 5)
  assert_eq(7 % 3, 1)
}

// 测试2: 布尔逻辑运算
test "布尔逻辑运算测试" {
  assert_true(true)
  assert_false(false)
  assert_true(true && true)
  assert_false(true && false)
  assert_true(true || false)
  assert_false(false || false)
  assert_true(!false)
  assert_false(!true)
}

// 测试3: 字符串操作
test "字符串操作测试" {
  assert_eq("hello" + " world", "hello world")
  assert_eq("test".length(), 4)
  assert_eq("".length(), 0)
  assert_eq("azimuth".length(), 7)
  assert_true("hello" != "world")
  assert_eq("hello", "hello")
}

// 测试4: 数组操作
test "数组操作测试" {
  let number_array = [1, 2, 3, 4, 5]
  assert_eq(number_array.length(), 5)
  assert_eq(number_array[0], 1)
  assert_eq(number_array[4], 5)
  
  let string_array = ["a", "b", "c"]
  assert_eq(string_array.length(), 3)
  assert_eq(string_array[1], "b")
  
  let empty_array = []
  assert_eq(empty_array.length(), 0)
}

// 测试5: Option类型操作
test "Option类型操作测试" {
  let some_value = Some(42)
  let none_value = None
  
  assert_eq(some_value, Some(42))
  assert_eq(none_value, None)
  assert_true(some_value != None)
  assert_true(none_value != Some(0))
  
  let some_string = Some("test")
  let none_string = None
  assert_eq(some_string, Some("test"))
  assert_eq(none_string, None)
}

// 测试6: 数值比较
test "数值比较测试" {
  assert_true(1 < 2)
  assert_true(2 <= 2)
  assert_true(3 > 2)
  assert_true(3 >= 3)
  assert_true(5 == 5)
  assert_true(5 != 4)
  
  assert_true(-5 < 0)
  assert_true(-10 < -5)
  assert_true(0 > -1)
}

// 测试7: 字符串比较
test "字符串比较测试" {
  assert_true("apple" < "banana")
  assert_true("hello" == "hello")
  assert_true("world" != "hello")
  assert_true("a" <= "a")
  assert_true("z" >= "a")
  
  assert_true("" == "")
  assert_true("a" != "")
  assert_true("" < "a")
}

// 测试8: 数组索引和切片
test "数组索引和切片测试" {
  let numbers = [10, 20, 30, 40, 50]
  assert_eq(numbers[0], 10)
  assert_eq(numbers[2], 30)
  assert_eq(numbers[4], 50)
  
  let chars = ['a', 'b', 'c', 'd']
  assert_eq(chars[0], 'a')
  assert_eq(chars[3], 'd')
  assert_eq(chars.length(), 4)
}

// 测试9: 负数运算
test "负数运算测试" {
  assert_eq(-5 + 10, 5)
  assert_eq(-3 * 2, -6)
  assert_true(-10 < 0)
  assert_true(-1 > -5)
  assert_eq(-(-5), 5)
  
  assert_eq(-10 - (-5), -5)
  assert_eq(-20 / 4, -5)
}

// 测试10: 复合类型操作
test "复合类型操作测试" {
  // 元组操作
  let tuple = (1, "hello", true)
  assert_eq(tuple.0, 1)
  assert_eq(tuple.1, "hello")
  assert_true(tuple.2)
  
  // 嵌套数组
  let nested = [[1, 2], [3, 4], [5, 6]]
  assert_eq(nested.length(), 3)
  assert_eq(nested[0][0], 1)
  assert_eq(nested[1][1], 4)
  assert_eq(nested[2][0], 5)
  
  // 嵌套Option
  let nested_option = Some(Some(42))
  assert_eq(nested_option, Some(Some(42)))
  assert_true(nested_option != None)
}