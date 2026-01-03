// Azimuth Simple Basic Tests
// 简单基础测试

// 测试1: 基本算术
test "基本算术" {
  assert_eq(1 + 1, 2)
  assert_eq(5 - 3, 2)
  assert_eq(3 * 4, 12)
  assert_eq(10 / 2, 5)
}

// 测试2: 布尔运算
test "布尔运算" {
  assert_true(true)
  assert_false(false)
  assert_true(true && true)
  assert_false(true && false)
  assert_true(true || false)
  assert_false(false || false)
}

// 测试3: 字符串
test "字符串" {
  let str = "hello"
  assert_eq(str.length(), 5)
  assert_eq(str + " world", "hello world")
}

// 测试4: 数组
test "数组" {
  let arr = [1, 2, 3]
  assert_eq(arr.length(), 3)
  assert_eq(arr[0], 1)
  assert_eq(arr[2], 3)
}

// 测试5: 条件
test "条件" {
  let x = 5
  let result = if x > 3 { "big" } else { "small" }
  assert_eq(result, "big")
}

// 测试6: 函数
test "函数" {
  let add = fn(a, b) { a + b }
  assert_eq(add(2, 3), 5)
}

// 测试7: 比较
test "比较" {
  assert_true(5 > 3)
  assert_true(3 < 5)
  assert_true(5 == 5)
  assert_true(5 != 3)
}

// 测试8: 选项类型
test "选项类型" {
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

// 测试9: 元组
test "元组" {
  let pair = (10, "hello")
  assert_eq(pair.0, 10)
  assert_eq(pair.1, "hello")
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
}