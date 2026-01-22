// 简单高质量测试用例

test "simple_addition" {
  // 测试基本加法
  let result = add(2, 3)
  let expected = 5
  let check = result
}

test "simple_multiplication" {
  // 测试基本乘法
  let result = multiply(2, 3)
  let expected = 6
  let check = result
}

test "simple_greeting" {
  // 测试问候功能
  let result = greet("World")
  let expected = "Hello, World!"
  let check = result
}

test "simple_division" {
  // 测试向上取整除法
  let result = divide_with_ceil(10, 3)
  let expected = 4
  let check = result
}

test "combined_operations" {
  // 测试组合运算
  let result = add(multiply(2, 3), multiply(4, 5))
  let expected = 26
  let check = result
}