// MoonBit 标准库核心模块测试用例
// 包含对 int、list、string、option 等核心模块的测试

test "int_basic_operations" {
  // 测试基本整数操作
  assert_eq(2 + 3, 5)
  assert_eq(10 - 4, 6)
  assert_eq(3 * 4, 12)
  assert_eq(15 / 3, 5)
  
  // 测试比较
  assert_true(5 > 3)
  assert_true(3 < 5)
  assert_true(5 == 5)
  assert_true(5 != 3)
}

test "string_basic_operations" {
  // 测试字符串基本操作
  let hello_str = "Hello"
  let world_str = "World"
  
  assert_eq(hello_str.length(), 5)
  
  // 测试字符串连接
  let combined = hello_str + " " + world_str
  assert_eq(combined, "Hello World")
  
  // 测试字符串比较
  assert_true(hello_str == "Hello")
  assert_true(hello_str != "hello")
}

test "option_basic_operations" {
  // 测试 Option 类型基本操作
  let some_value : Int? = Some(42)
  let none_value : Int? = None
  
  assert_true(some_value is Some(_))
  assert_true(none_value is None)
  
  // 测试 unwrap_or
  assert_eq(some_value.unwrap_or(0), 42)
  assert_eq(none_value.unwrap_or(0), 0)
}

test "array_basic_operations" {
  // 测试数组基本操作
  let num_array = [1, 2, 3, 4, 5]
  
  assert_eq(num_array.length(), 5)
  
  // 测试数组访问
  assert_eq(num_array[0], 1)
  assert_eq(num_array[4], 5)
  
  // 测试数组操作
  let doubled = num_array.map(x => x * 2)
  assert_eq(doubled, [2, 4, 6, 8, 10])
}

test "boolean_basic_operations" {
  // 测试布尔基本操作
  assert_true(true)
  assert_false(false)
  
  // 测试布尔运算
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && false)
  
  assert_true(true || false)
  assert_true(true || true)
  assert_false(false || false)
}

test "tuple_basic_operations" {
  // 测试元组基本操作
  let pair = (1, "hello")
  
  // 获取元组元素
  assert_eq(pair.0, 1)
  assert_eq(pair.1, "hello")
  
  // 测试元组比较
  assert_eq((1, "hello"), (1, "hello"))
  assert_true((1, "hello") != (2, "hello"))
}

test "list_basic_operations" {
  // 测试列表基本操作
  let list1 = [1, 2, 3, 4, 5]
  assert_eq(list1.length(), 5)
  assert_eq(list1[0], 1)
  assert_eq(list1[4], 5)
  
  // 测试列表操作
  let doubled = list1.map(x => x * 2)
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  let evens = list1.filter(x => x % 2 == 0)
  assert_eq(evens, [2, 4])
}

test "combined_operations" {
  // 测试多种类型的组合操作
  // 创建一个数字列表，进行操作，然后转换为字符串
  let numbers = [1, 2, 3, 4, 5]
  
  // 过滤偶数
  let evens = numbers.filter(x => x % 2 == 0)
  assert_eq(evens, [2, 4])
  
  // 映射操作
  let doubled = evens.map(x => x * 2)
  assert_eq(doubled, [4, 8])
}