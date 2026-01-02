// 基础MoonBit语言测试
// 专注于测试MoonBit语言的基本特性和操作

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
  
  assert_eq(str1.to_uppercase(), "HELLO")
  assert_eq(str2.to_uppercase(), "WORLD")
  assert_eq(str3.to_uppercase(), "AZIMUTH")
  
  assert_eq(str1.to_lowercase(), "hello")
  assert_eq("HELLO".to_lowercase(), "hello")
  assert_eq("WORLD".to_lowercase(), "world")
}

// 测试4: 数组操作
test "数组操作测试" {
  let empty_array = []
  let numbers = [1, 2, 3, 4, 5]
  let strings = ["a", "b", "c", "d", "e"]
  
  assert_eq(empty_array.length(), 0)
  assert_eq(numbers.length(), 5)
  assert_eq(strings.length(), 5)
  
  assert_eq(numbers[0], 1)
  assert_eq(numbers[2], 3)
  assert_eq(numbers[4], 5)
  
  assert_eq(strings[0], "a")
  assert_eq(strings[2], "c")
  assert_eq(strings[4], "e")
  
  let doubled = numbers.map(fn(x) { x * 2 })
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  let evens = numbers.filter(fn(x) { x % 2 == 0 })
  assert_eq(evens, [2, 4])
  
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(sum, 15)
  
  assert_true(numbers.contains(3))
  assert_false(numbers.contains(10))
  
  let upper_strings = strings.map(fn(s) { s.to_uppercase() })
  assert_eq(upper_strings, ["A", "B", "C", "D", "E"])
  
  let long_strings = strings.filter(fn(s) { s.length() > 0 })
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
  
  let doubled = some_value.map(fn(x) { x * 2 })
  match doubled {
    Some(v) => assert_eq(v, 84)
    None => assert_true(false)
  }
  
  let uppercased = some_string.map(fn(s) { s.to_uppercase() })
  match uppercased {
    Some(s) => assert_eq(s, "TEST")
    None => assert_true(false)
  }
  
  let none_mapped = none_value.map(fn(x) { x * 2 })
  assert_eq(none_mapped, None)
  
  let default_value = some_value.map_or(0, fn(x) { x })
  assert_eq(default_value, 42)
  
  let none_default = none_value.map_or(0, fn(x) { x })
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

// 测试8: 类型转换
test "类型转换测试" {
  // 整数转字符串
  assert_eq(1.to_string(), "1")
  assert_eq(42.to_string(), "42")
  assert_eq(-10.to_string(), "-10")
  
  // 浮点数转字符串
  assert_eq(3.14.to_string(), "3.14")
  assert_eq(0.0.to_string(), "0.0")
  assert_eq(-2.5.to_string(), "-2.5")
  
  // 字符串转整数（模拟）
  let parse_int = fn(s : String) -> Int {
    if s == "1" { 1 }
    else if s == "42" { 42 }
    else if s == "-10" { -10 }
    else { 0 }
  }
  
  assert_eq(parse_int("1"), 1)
  assert_eq(parse_int("42"), 42)
  assert_eq(parse_int("-10"), -10)
  assert_eq(parse_int("unknown"), 0)
  
  // 整数转浮点数
  assert_eq(1.to_float(), 1.0)
  assert_eq(42.to_float(), 42.0)
  assert_eq(-10.to_float(), -10.0)
  
  // 浮点数转整数
  assert_eq(1.0.to_int(), 1)
  assert_eq(42.0.to_int(), 42)
  assert_eq(-10.0.to_int(), -10)
  assert_eq(3.14.to_int(), 3)
  assert_eq(-2.5.to_int(), -2)
}