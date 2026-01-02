// 简单集成测试用例
// 测试基本的MoonBit语言特性

test "字符串操作测试" {
  let str = "azimuth-telemetry"
  assert_eq(str.length(), 17)
  assert_eq(str.to_uppercase(), "AZIMUTH-TELEMETRY")
  assert_eq(str.to_lowercase(), "azimuth-telemetry")
  assert_true(str.contains("telemetry"))
  assert_false(str.contains("metrics"))
  assert_eq(str.substring(0, 7), "azimuth")
}

test "数组操作测试" {
  let numbers = [1, 2, 3, 4, 5]
  let doubled = numbers.map(fn(x) { x * 2 })
  let evens = numbers.filter(fn(x) { x % 2 == 0 })
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  
  assert_eq(doubled, [2, 4, 6, 8, 10])
  assert_eq(evens, [2, 4])
  assert_eq(sum, 15)
  assert_eq(numbers.length(), 5)
  assert_true(numbers.contains(3))
  assert_false(numbers.contains(10))
}

test "Option类型测试" {
  let some_value = Some(42)
  let none_value = None
  
  match some_value {
    Some(v) => assert_eq(v, 42)
    None => assert_true(false)
  }
  
  match none_value {
    Some(v) => assert_true(false)
    None => assert_true(true)
  }
  
  let doubled = some_value.map(fn(x) { x * 2 })
  match doubled {
    Some(v) => assert_eq(v, 84)
    None => assert_true(false)
  }
}

test "Result类型测试" {
  let ok_value = Ok("success")
  let err_value = Err("error")
  
  match ok_value {
    Ok(v) => assert_eq(v, "success")
    Err(e) => assert_true(false)
  }
  
  match err_value {
    Ok(v) => assert_true(false)
    Err(e) => assert_eq(e, "error")
  }
}

test "整数运算测试" {
  let a = 10
  let b = 3
  
  assert_eq(a + b, 13)
  assert_eq(a - b, 7)
  assert_eq(a * b, 30)
  assert_eq(a / b, 3)
  assert_eq(a % b, 1)
  
  assert_true(a > b)
  assert_true(a >= b)
  assert_false(a < b)
  assert_false(a <= b)
  assert_false(a == b)
  assert_true(a != b)
}

test "浮点数运算测试" {
  let a = 10.5
  let b = 3.2
  
  assert_eq(a + b, 13.7)
  assert_eq(a - b, 7.3)
  assert_eq(a * b, 33.6)
  
  assert_true(a > b)
  assert_true(a >= b)
  assert_false(a < b)
  assert_false(a <= b)
  assert_false(a == b)
  assert_true(a != b)
}

test "布尔运算测试" {
  let t = true
  let f = false
  
  assert_true(t && t)
  assert_false(t && f)
  assert_false(f && t)
  assert_false(f && f)
  
  assert_true(t || t)
  assert_true(t || f)
  assert_true(f || t)
  assert_false(f || f)
  
  assert_false(!t)
  assert_true(!f)
}

test "条件表达式测试" {
  let x = 10
  
  let result = if x > 5 { "greater" } else { "less or equal" }
  assert_eq(result, "greater")
  
  let y = 3
  let result2 = if y > 5 { "greater" } else { "less or equal" }
  assert_eq(result2, "less or equal")
}

test "循环结构测试" {
  let mut sum = 0
  
  for i = 1; i <= 5; i = i + 1 {
    sum = sum + i
  }
  
  assert_eq(sum, 15)
  
  let mut product = 1
  let mut i = 1
  
  while i <= 4 {
    product = product * i
    i = i + 1
  }
  
  assert_eq(product, 24)
}

test "函数定义和调用测试" {
  fn add(x : Int, y : Int) -> Int {
    x + y
  }
  
  fn multiply(x : Int, y : Int) -> Int {
    x * y
  }
  
  fn is_even(x : Int) -> Bool {
    x % 2 == 0
  }
  
  assert_eq(add(3, 4), 7)
  assert_eq(multiply(5, 6), 30)
  assert_true(is_even(10))
  assert_false(is_even(7))
}