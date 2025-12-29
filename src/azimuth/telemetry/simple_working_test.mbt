test "simple test" {
  // 简单的数学运算测试
  assert_eq(1 + 1, 2)
  assert_eq(2 * 3, 6)
  assert_eq(10 - 5, 5)
  assert_eq(8 / 2, 4)
  
  // 字符串操作测试
  let hello = "Hello"
  let world = "World"
  let greeting = hello + " " + world
  assert_eq(greeting, "Hello World")
  
  // 布尔值测试
  assert_eq(true && true, true)
  assert_eq(true && false, false)
  assert_eq(true || false, true)
  assert_eq(false || false, false)
  
  // 数组测试
  let numbers = [1, 2, 3, 4, 5]
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
}

test "string operations" {
  let text = "Azimuth Telemetry"
  assert_eq(text.length(), 17)
  assert_eq(text.has_prefix("Azimuth"), true)
  assert_eq(text.has_suffix("Telemetry"), true)
  assert_eq(text.has_prefix("Telemetry"), false)
  assert_eq(text.has_suffix("Azimuth"), false)
}

test "numeric operations" {
  // 整数操作
  let a = 42
  let b = 17
  assert_eq(a + b, 59)
  assert_eq(a - b, 25)
  assert_eq(a * b, 714)
  assert_eq(a / b, 2)
  
  // 浮点数操作
  let x = 3.14
  let y = 2.71
  assert_eq(x + y, 5.85)
  assert_eq(x - y, 0.43)
  assert_eq(x * y, 8.5094)
}

test "conditional logic" {
  let value = 10
  
  if value > 5 {
    assert_eq(true, true)
  } else {
    @test.fail("Should not reach here")
  }
  
  if value < 5 {
    @test.fail("Should not reach here")
  } else {
    assert_eq(true, true)
  }
}