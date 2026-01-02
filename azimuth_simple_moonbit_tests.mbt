// Azimuth 简单MoonBit测试用例
// 只使用MoonBit的基本功能，不依赖外部类型或函数

// 测试1: 基本算术运算
test "基本算术运算测试" {
  assert_eq(1 + 1, 2)
  assert_eq(5 + 10, 15)
  assert_eq(100 + 200, 300)
  
  assert_eq(10 - 5, 5)
  assert_eq(100 - 25, 75)
  assert_eq(50 - 50, 0)
  
  assert_eq(3 * 4, 12)
  assert_eq(10 * 10, 100)
  assert_eq(7 * 8, 56)
  
  assert_eq(20 / 4, 5)
  assert_eq(100 / 10, 10)
  assert_eq(15 / 3, 5)
  
  assert_eq(10 % 3, 1)
  assert_eq(20 % 6, 2)
  assert_eq(100 % 7, 2)
}

// 测试2: 布尔逻辑运算
test "布尔逻辑运算测试" {
  assert_true(true)
  assert_false(false)
  
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && true)
  assert_false(false && false)
  
  assert_true(true || true)
  assert_true(true || false)
  assert_true(false || true)
  assert_false(false || false)
  
  assert_true(!false)
  assert_false(!true)
  
  assert_true((true && false) || true)
  assert_false((true && false) && true)
  assert_true(true || (false && false))
}

// 测试3: 字符串操作
test "字符串操作测试" {
  let str1 = "hello"
  let str2 = "world"
  let str3 = "azimuth"
  
  assert_eq(str1.length(), 5)
  assert_eq(str2.length(), 5)
  assert_eq(str3.length(), 7)
  
  assert_eq(str1 + " " + str2, "hello world")
  assert_eq(str3 + "-telemetry", "azimuth-telemetry")
  
  assert_true(str1.contains("h"))
  assert_true(str1.contains("ell"))
  assert_false(str1.contains("z"))
  
  assert_eq(str1, "hello")
  assert_not_eq(str1, "world")
}

// 测试4: 数组操作
test "数组操作测试" {
  let numbers = [1, 2, 3, 4, 5]
  let strings = ["a", "b", "c", "d", "e"]
  
  assert_eq(numbers.length(), 5)
  assert_eq(strings.length(), 5)
  
  assert_eq(numbers[0], 1)
  assert_eq(numbers[2], 3)
  assert_eq(numbers[4], 5)
  
  assert_eq(strings[0], "a")
  assert_eq(strings[2], "c")
  assert_eq(strings[4], "e")
  
  // 手动实现map功能
  let mut doubled = []
  for i in 0..numbers.length() {
    doubled = doubled + [numbers[i] * 2]
  }
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  // 手动实现filter功能
  let mut evens = []
  for i in 0..numbers.length() {
    if numbers[i] % 2 == 0 {
      evens = evens + [numbers[i]]
    }
  }
  assert_eq(evens, [2, 4])
  
  // 手动实现reduce功能
  let mut sum = 0
  for i in 0..numbers.length() {
    sum = sum + numbers[i]
  }
  assert_eq(sum, 15)
  
  assert_true(numbers.contains(3))
  assert_false(numbers.contains(10))
  
  // 字符串数组操作
  let mut upper_strings = []
  for i in 0..strings.length() {
    upper_strings = upper_strings + [strings[i]]
  }
  assert_eq(upper_strings, ["a", "b", "c", "d", "e"])
  
  let mut long_strings = []
  for i in 0..strings.length() {
    if strings[i].length() > 0 {
      long_strings = long_strings + [strings[i]]
    }
  }
  assert_eq(long_strings, ["a", "b", "c", "d", "e"])
}

// 测试5: Option类型操作
test "Option类型操作测试" {
  let some_value = Some(42)
  let none_value = None
  let some_string = Some("test")
  let none_string = None
  
  match some_value {
    Some(v) => assert_eq(v, 42)
    None => assert_true(false)
  }
  
  match none_value {
    Some(v) => assert_true(false)
    None => assert_true(true)
  }
  
  match some_string {
    Some(s) => assert_eq(s, "test")
    None => assert_true(false)
  }
  
  match none_string {
    Some(s) => assert_true(false)
    None => assert_true(true)
  }
  
  // 手动实现map功能
  let doubled = match some_value {
    Some(v) => Some(v * 2)
    None => None
  }
  match doubled {
    Some(v) => assert_eq(v, 84)
    None => assert_true(false)
  }
  
  let uppercased = match some_string {
    Some(s) => Some(s) // 简化，不做实际转换
    None => None
  }
  match uppercased {
    Some(s) => assert_eq(s, "test")
    None => assert_true(false)
  }
  
  let none_mapped = match none_value {
    Some(v) => Some(v * 2)
    None => None
  }
  assert_eq(none_mapped, None)
  
  // 手动实现map_or功能
  let default_value = match some_value {
    Some(v) => v
    None => 0
  }
  assert_eq(default_value, 42)
  
  let none_default = match none_value {
    Some(v) => v
    None => 0
  }
  assert_eq(none_default, 0)
}

// 测试6: 元组操作
test "元组操作测试" {
  let pair = (1, "one")
  let triple = (1, "one", true)
  let nested = ((1, 2), ("a", "b"))
  
  assert_eq(pair.0, 1)
  assert_eq(pair.1, "one")
  
  assert_eq(triple.0, 1)
  assert_eq(triple.1, "one")
  assert_eq(triple.2, true)
  
  assert_eq(nested.0, (1, 2))
  assert_eq(nested.1, ("a", "b"))
  
  match pair {
    (x, y) => {
      assert_eq(x, 1)
      assert_eq(y, "one")
    }
  }
  
  match triple {
    (x, y, z) => {
      assert_eq(x, 1)
      assert_eq(y, "one")
      assert_eq(z, true)
    }
  }
}

// 测试7: 比较操作
test "比较操作测试" {
  // 数值比较
  assert_true(1 == 1)
  assert_false(1 == 2)
  assert_true(1 != 2)
  assert_false(1 != 1)
  assert_true(1 < 2)
  assert_false(2 < 1)
  assert_true(1 <= 1)
  assert_true(1 <= 2)
  assert_false(2 <= 1)
  assert_true(2 > 1)
  assert_false(1 > 2)
  assert_true(2 >= 2)
  assert_true(2 >= 1)
  assert_false(1 >= 2)
  
  // 字符串比较
  assert_true("a" == "a")
  assert_false("a" == "b")
  assert_true("a" != "b")
  assert_false("a" != "a")
  assert_true("a" < "b")
  assert_false("b" < "a")
  assert_true("a" <= "a")
  assert_true("a" <= "b")
  assert_false("b" <= "a")
  assert_true("b" > "a")
  assert_false("a" > "b")
  assert_true("b" >= "b")
  assert_true("b" >= "a")
  assert_false("a" >= "b")
}

// 测试8: 条件逻辑
test "条件逻辑测试" {
  let x = 10
  let y = 20
  
  if x < y {
    assert_true(true)
  } else {
    assert_true(false)
  }
  
  let result = if x > 5 { "greater" } else { "less" }
  assert_eq(result, "greater")
  
  // 嵌套条件
  let z = 15
  let comparison_result = if x < y {
    if y < z { "x < y < z" } else { "x < y >= z" }
  } else {
    if x < z { "y <= x < z" } else { "y <= x >= z" }
  }
  assert_eq(comparison_result, "x < y < z")
}

// 测试9: 循环和迭代
test "循环和迭代测试" {
  let mut sum = 0
  for i in 0..=5 {
    sum = sum + i
  }
  assert_eq(sum, 15)
  
  let mut product = 1
  for i in 1..=4 {
    product = product * i
  }
  assert_eq(product, 24)
  
  // 数组迭代
  let numbers = [1, 2, 3, 4, 5]
  let mut array_sum = 0
  for i in 0..numbers.length() {
    array_sum = array_sum + numbers[i]
  }
  assert_eq(array_sum, 15)
  
  // 条件循环
  let mut count = 0
  let mut result = 0
  while count < 5 {
    result = result + count
    count = count + 1
  }
  assert_eq(result, 10) // 0 + 1 + 2 + 3 + 4 = 10
}

// 测试10: 函数定义和使用
test "函数定义和使用测试" {
  // 简单函数
  let add = fn(a, b) { a + b }
  let multiply = fn(a, b) { a * b }
  
  assert_eq(add(2, 3), 5)
  assert_eq(multiply(4, 5), 20)
  
  // 高阶函数
  let apply_twice = fn(f, x) { f(f(x)) }
  
  let add_one = fn(x) { x + 1 }
  let multiply_by_two = fn(x) { x * 2 }
  
  assert_eq(apply_twice(add_one, 1), 3)
  assert_eq(apply_twice(multiply_by_two, 3), 12)
  
  // 函数作为参数
  let calculate = fn(a, b, operation) {
    if operation == "add" { a + b }
    else if operation == "subtract" { a - b }
    else if operation == "multiply" { a * b }
    else if operation == "divide" && b != 0 { a / b }
    else { 0 }
  }
  
  assert_eq(calculate(10, 5, "add"), 15)
  assert_eq(calculate(10, 5, "subtract"), 5)
  assert_eq(calculate(10, 5, "multiply"), 50)
  assert_eq(calculate(10, 5, "divide"), 2)
  assert_eq(calculate(10, 0, "divide"), 0)
  assert_eq(calculate(10, 5, "unknown"), 0)
}