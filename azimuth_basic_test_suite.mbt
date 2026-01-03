// Azimuth Basic Test Suite
// 基础测试套件，包含10个简单的测试用例

// 测试1: 基本算术运算
test "基本算术运算" {
  assert_eq(1 + 1, 2)
  assert_eq(5 + 10, 15)
  assert_eq(10 - 3, 7)
  assert_eq(4 * 5, 20)
  assert_eq(20 / 4, 5)
}

// 测试2: 布尔逻辑
test "布尔逻辑" {
  assert_true(true)
  assert_false(false)
  assert_true(true && true)
  assert_false(true && false)
  assert_true(true || false)
  assert_false(false || false)
}

// 测试3: 字符串操作
test "字符串操作" {
  let str = "azimuth"
  assert_eq(str.length(), 7)
  assert_eq(str + " test", "azimuth test")
  assert_true(str.contains("az"))
}

// 测试4: 数组操作
test "数组操作" {
  let arr = [1, 2, 3, 4, 5]
  assert_eq(arr.length(), 5)
  assert_eq(arr[0], 1)
  assert_eq(arr[4], 5)
  
  let doubled = arr.map(fn(x) { x * 2 })
  assert_eq(doubled, [2, 4, 6, 8, 10])
}

// 测试5: 条件语句
test "条件语句" {
  let x = 10
  let result = if x > 5 { "greater" } else { "less" }
  assert_eq(result, "greater")
  
  let y = 3
  let result2 = if y > 5 { "greater" } else { "less" }
  assert_eq(result2, "less")
}

// 测试6: 函数定义和调用
test "函数定义和调用" {
  let add = fn(a, b) { a + b }
  assert_eq(add(3, 4), 7)
  
  let multiply = fn(a, b) { a * b }
  assert_eq(multiply(5, 6), 30)
  
  let square = fn(x) { x * x }
  assert_eq(square(4), 16)
}

// 测试7: 比较操作
test "比较操作" {
  assert_true(5 > 3)
  assert_true(3 < 5)
  assert_true(5 >= 5)
  assert_true(5 <= 5)
  assert_true(5 == 5)
  assert_true(5 != 3)
}

// 测试8: 模式匹配
test "模式匹配" {
  let option = Some(5)
  let result = match option {
    Some(x) => x * 2
    None => 0
  }
  assert_eq(result, 10)
  
  let none_option = None[Int]
  let result2 = match none_option {
    Some(x) => x * 2
    None => 0
  }
  assert_eq(result2, 0)
}

// 测试9: 元组操作
test "元组操作" {
  let pair = (10, "hello")
  assert_eq(pair.0, 10)
  assert_eq(pair.1, "hello")
  
  let triple = (1, 2, 3)
  assert_eq(triple.0, 1)
  assert_eq(triple.1, 2)
  assert_eq(triple.2, 3)
}

// 测试10: 记录类型
test "记录类型" {
  type Point {
    x : Int
    y : Int
  }
  
  let p = { x: 3, y: 4 }
  assert_eq(p.x, 3)
  assert_eq(p.y, 4)
  
  let p2 = { p | y: 5 }
  assert_eq(p2.x, 3)
  assert_eq(p2.y, 5)
}