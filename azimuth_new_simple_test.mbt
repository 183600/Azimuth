// Azimuth 新增简单测试
// 测试基本功能以确保语法正确

test "简单算术测试" {
  assert_eq(1 + 1, 2)
  assert_eq(5 * 5, 25)
  assert_eq(10 / 2, 5)
}

test "字符串操作测试" {
  let str1 = "hello"
  let str2 = "world"
  assert_eq(str1 + " " + str2, "hello world")
  assert_eq(str1.length(), 5)
}

test "数组操作测试" {
  let numbers = [1, 2, 3, 4, 5]
  assert_eq(numbers.length(), 5)
  assert_eq(numbers[0], 1)
  assert_eq(numbers[4], 5)
}

test "布尔逻辑测试" {
  assert_true(true)
  assert_false(false)
  assert_true(true && false == false)
  assert_true(true || false == true)
}