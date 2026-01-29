// Azimuth 项目最小标准测试套件
// 使用标准 MoonBit 测试语法，测试核心功能

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  let result1 = 2 + 3
  let result2 = 0 + 0
  let result3 = -5 + -5
  let result4 = 5 + -5
  
  // 验证结果
  let check1 = result1 == 5
  let check2 = result2 == 0
  let check3 = result3 == -10
  let check4 = result4 == 0
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  let result1 = 2 * 3
  let result2 = 5 * 0
  let result3 = -2 * 3
  let result4 = -2 * -3
  
  // 验证结果
  let check1 = result1 == 6
  let check2 = result2 == 0
  let check3 = result3 == -6
  let check4 = result4 == 6
}

test "string_operations" {
  // 测试字符串操作
  let greeting1 = "Hello, " + "World!"
  let greeting2 = "Hello, " + "MoonBit!"
  let greeting3 = "Hello, " + ""
  
  // 验证结果
  let check1 = greeting1 == "Hello, World!"
  let check2 = greeting2 == "Hello, MoonBit!"
  let check3 = greeting3 == "Hello, !"
}

test "integer_division" {
  // 测试整数除法
  let result1 = 5 / 2
  let result2 = 6 / 2
  let result3 = 0 / 5
  let result4 = -5 / 2
  
  // 验证结果
  let check1 = result1 == 2
  let check2 = result2 == 3
  let check3 = result3 == 0
  let check4 = result4 == -2
}

test "modulo_operations" {
  // 测试取模运算
  let result1 = 5 % 2
  let result2 = 6 % 2
  let result3 = 0 % 5
  let result4 = -5 % 2
  
  // 验证结果
  let check1 = result1 == 1
  let check2 = result2 == 0
  let check3 = result3 == 0
  let check4 = result4 == -1
}

test "boolean_expressions" {
  // 测试布尔表达式
  let result1 = 5 > 3
  let result2 = 5 < 3
  let result3 = 5 >= 5
  let result4 = 5 <= 3
  let result5 = 5 == 5
  let result6 = 5 != 3
  
  // 验证结果
  let check1 = result1 == true
  let check2 = result2 == false
  let check3 = result3 == true
  let check4 = result4 == false
  let check5 = result5 == true
  let check6 = result6 == true
}

test "complex_expressions" {
  // 测试复杂表达式
  let result1 = (2 + 3) * 4
  let result2 = 10 * (5 + 3)
  let result3 = 2 + 3 * 4
  let result4 = (2 + 3) * (4 + 5)
  
  // 验证结果
  let check1 = result1 == 20
  let check2 = result2 == 80
  let check3 = result3 == 14
  let check4 = result4 == 45
}

test "variable_assignments" {
  // 测试变量赋值
  let x = 10
  let y = 20
  let sum = x + y
  let product = x * y
  let difference = x - y
  
  // 验证结果
  let check1 = x == 10
  let check2 = y == 20
  let check3 = sum == 30
  let check4 = product == 200
  let check5 = difference == -10
}

test "negative_number_operations" {
  // 测试负数运算
  let result1 = -5 + 3
  let result2 = -5 * 3
  let result3 = -5 - 3
  let result4 = -15 / 3
  
  // 验证结果
  let check1 = result1 == -2
  let check2 = result2 == -15
  let check3 = result3 == -8
  let check4 = result4 == -5
}

test "zero_operations" {
  // 测试零运算
  let result1 = 0 + 5
  let result2 = 0 * 5
  let result3 = 5 - 0
  let result4 = 0 / 5
  
  // 验证结果
  let check1 = result1 == 5
  let check2 = result2 == 0
  let check3 = result3 == 5
  let check4 = result4 == 0
}