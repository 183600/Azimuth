// 简单的标准 MoonBit 测试用例
// 只使用基本的算术运算，不依赖库函数

test "basic_addition" {
  // 测试基本加法运算
  assert_eq(2 + 3, 5)
  assert_eq(0 + 0, 0)
  assert_eq(-5 + 10, 5)
}

test "basic_multiplication" {
  // 测试基本乘法运算
  assert_eq(3 * 4, 12)
  assert_eq(0 * 5, 0)
  assert_eq(-2 * 3, -6)
}

test "basic_subtraction" {
  // 测试基本减法运算
  assert_eq(10 - 5, 5)
  assert_eq(5 - 10, -5)
  assert_eq(0 - 0, 0)
}

test "basic_division" {
  // 测试基本除法运算
  assert_eq(10 / 2, 5)
  assert_eq(9 / 3, 3)
  assert_eq(1 / 2, 0)
  assert_eq(0 / 5, 0)
}

test "string_operations" {
  // 测试基本字符串操作
  let str1 = "Hello"
  let str2 = "World"
  let combined = str1 + ", " + str2 + "!"
  
  assert_eq(combined.length(), 13)
  assert_eq(combined, "Hello, World!")
}

test "simple_calculation" {
  // 测试简单计算场景
  let base = 10
  let multiplier = 3
  let increment = 5
  
  // 复合运算：(base * multiplier + increment)
  let step1 = base * multiplier
  let result = step1 + increment
  
  assert_eq(30, step1)
  assert_eq(35, result)
}