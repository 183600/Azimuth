// Additional Test Suite for MoonBit
// 额外的MoonBit测试套件

// Test 6: 数值边界条件
test "numeric boundary conditions" {
  assert_eq(0 + 0, 0)
  assert_eq(0 * 5, 0)
  assert_eq(5 * 0, 0)
  
  assert_eq(-5 + 5, 0)
  assert_eq(-10 + -5, -15)
  assert_eq(-5 * -5, 25)
}

// Test 7: 字符串比较
test "string comparisons" {
  let str1 = "hello"
  let str2 = "hello"
  let str3 = "world"
  
  assert_true(str1 == str2)
  assert_false(str1 == str3)
  assert_true(str1 != str3)
  assert_false(str1 != str2)
}

// Test 8: 数组操作
test "array advanced operations" {
  let numbers = [1, 2, 3, 4, 5]
  
  // 数组映射
  let doubled = numbers.map(fn(x) { x * 2 })
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  // 数组过滤
  let evens = numbers.filter(fn(x) { x % 2 == 0 })
  assert_eq(evens, [2, 4])
  
  // 数组包含检查
  assert_true(numbers.contains(3))
  assert_false(numbers.contains(6))
}

// Test 9: 元组操作
test "tuple operations" {
  let pair = (1, "one")
  let triple = (1, "one", true)
  
  // 元组访问
  assert_eq(pair.0, 1)
  assert_eq(pair.1, "one")
  
  assert_eq(triple.0, 1)
  assert_eq(triple.1, "one")
  assert_eq(triple.2, true)
  
  // 元组比较
  assert_eq(pair, (1, "one"))
  assert_false(pair == (2, "two"))
}

// Test 10: 条件表达式
test "conditional expressions" {
  let x = 5
  let result = if x > 0 { "positive" } else { "non-positive" }
  assert_eq(result, "positive")
  
  let y = -3
  let result2 = if y > 0 { "positive" } else { "non-positive" }
  assert_eq(result2, "non-positive")
  
  // 嵌套条件
  let z = 0
  let result3 = if z > 0 { "positive" } else if z < 0 { "negative" } else { "zero" }
  assert_eq(result3, "zero")
}