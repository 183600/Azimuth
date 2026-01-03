// 简单的MoonBit测试
test "基本算术测试" {
  assert_eq(1 + 1, 2)
  assert_eq(5 - 3, 2)
}

test "布尔逻辑测试" {
  assert_true(true)
  assert_false(false)
  assert_true(true && true)
  assert_false(true && false)
}

test "字符串测试" {
  let str = "hello"
  assert_eq(str.length(), 5)
  assert_eq(str + " world", "hello world")
}

test "数组测试" {
  let arr = [1, 2, 3]
  assert_eq(arr.length(), 3)
  assert_eq(arr[0], 1)
}

test "条件测试" {
  let x = 5
  let result = if x > 3 { "big" } else { "small" }
  assert_eq(result, "big")
}

test "函数测试" {
  let add = fn(a, b) { a + b }
  assert_eq(add(2, 3), 5)
}

test "比较测试" {
  assert_true(5 > 3)
  assert_true(3 < 5)
  assert_true(5 == 5)
  assert_true(5 != 3)
}

test "选项类型测试" {
  let some = Some(5)
  let none = None[Int]
  
  let result1 = match some {
    Some(x) => x * 2
    None => 0
  }
  assert_eq(result1, 10)
  
  let result2 = match none {
    Some(x) => x * 2
    None => 0
  }
  assert_eq(result2, 0)
}

test "元组测试" {
  let pair = (10, "hello")
  assert_eq(pair.0, 10)
  assert_eq(pair.1, "hello")
}

test "记录类型测试" {
  type Point {
    x : Int
    y : Int
  }
  
  let p = { x: 3, y: 4 }
  assert_eq(p.x, 3)
  assert_eq(p.y, 4)
}