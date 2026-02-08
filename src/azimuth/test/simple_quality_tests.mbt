// 简化的高质量 MoonBit 核心库测试用例
// 使用标准的 MoonBit 测试语法

test "basic_arithmetic" {
  // 测试基本算术运算
  assert_eq(1 + 1, 2)
  assert_eq(5 - 3, 2)
  assert_eq(3 * 4, 12)
  assert_eq(10 / 2, 5)
}

test "boolean_operations" {
  // 测试布尔运算
  assert_eq(true && false, false)
  assert_eq(true || false, true)
  assert_eq(!true, false)
  assert_eq(!false, true)
}

test "string_operations" {
  // 测试字符串操作
  let hello = "Hello"
  let world = " World"
  let combined = hello + world
  assert_eq(combined, "Hello World")
  assert_eq(combined.length(), 11)
}

test "array_operations" {
  // 测试数组操作
  let numbers = [1, 2, 3, 4, 5]
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
}

test "comparison_operations" {
  // 测试比较运算
  assert_eq(5 > 3, true)
  assert_eq(5 < 3, false)
  assert_eq(5 == 5, true)
  assert_eq(5 != 3, true)
  assert_eq(5 >= 5, true)
  assert_eq(3 <= 5, true)
}

test "int_modulo" {
  // 测试取模运算
  assert_eq(10 % 3, 1)
  assert_eq(9 % 3, 0)
  assert_eq(7 % 4, 3)
}

test "string_methods" {
  // 测试字符串方法
  let text = "Hello World"
  assert_eq(text.length(), 11)
  assert_eq(text[0], 'H')
  assert_eq(text[10], 'd')
}

test "array_methods" {
  // 测试数组方法
  let numbers = [1, 2, 3, 4, 5]
  let sum = numbers.fold(init=0, (acc, x) => acc + x)
  assert_eq(sum, 15)
  
  let doubled = numbers.map(x => x * 2)
  assert_eq(doubled.length(), 5)
  assert_eq(doubled[0], 2)
  assert_eq(doubled[4], 10)
}

test "option_operations" {
  // 测试 Option 类型操作
  let some_value = Some(42)
  let none_value : Int? = None
  
  assert_eq(some_value.is_some(), true)
  assert_eq(none_value.is_none(), true)
  
  let unwrapped = match some_value {
    Some(x) => x
    None => 0
  }
  assert_eq(unwrapped, 42)
}

test "type_conversion" {
  // 测试类型转换
  let num = 42
  let str = num.to_string()
  assert_eq(str, "42")
  
  let char = 'A'
  assert_eq(char.to_string(), "A")
}