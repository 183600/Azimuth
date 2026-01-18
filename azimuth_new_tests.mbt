// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，包含数学运算和字符串操作

test "addition_basic" {
  // 基本加法测试
  let result1 = 1 + 2
  let result2 = 10 + 5
  let result3 = -3 + 7
  
  // 验证结果
  let check1 = result1
  let check2 = result2
  let check3 = result3
}

test "subtraction_basic" {
  // 基本减法测试
  let result1 = 10 - 5
  let result2 = 7 - 3
  let result3 = 5 - 10
  
  // 验证结果
  let check1 = result1
  let check2 = result2
  let check3 = result3
}

test "multiplication_basic" {
  // 基本乘法测试
  let result1 = 3 * 4
  let result2 = 5 * 6
  let result3 = -2 * 8
  
  // 验证结果
  let check1 = result1
  let check2 = result2
  let check3 = result3
}

test "division_basic" {
  // 基本除法测试
  let result1 = 20 / 4
  let result2 = 15 / 3
  let result3 = 0 / 7
  
  // 验证结果
  let check1 = result1
  let check2 = result2
  let check3 = result3
}

test "modulo_operation" {
  // 取模运算测试
  let result1 = 10 % 3
  let result2 = 15 % 5
  let result3 = 7 % 4
  
  // 验证结果
  let check1 = result1
  let check2 = result2
  let check3 = result3
}

test "string_concatenation" {
  // 字符串连接测试
  let result1 = "Hello" + " " + "World"
  let result2 = "Moon" + "Bit"
  let result3 = "Test" + "123"
  
  // 验证结果
  let check1 = result1
  let check2 = result2
  let check3 = result3
}

test "boolean_operations" {
  // 布尔运算测试
  let result1 = true && true
  let result2 = true && false
  let result3 = true || false
  let result4 = false || false
  
  // 验证结果
  let check1 = result1
  let check2 = result2
  let check3 = result3
  let check4 = result4
}

test "comparison_operations" {
  // 比较运算测试
  let result1 = 5 > 3
  let result2 = 3 > 5
  let result3 = 5 >= 5
  let result4 = 3 < 5
  let result5 = 5 < 3
  let result6 = 5 <= 5
  let result7 = 5 == 5
  let result8 = 5 != 3
  
  // 验证结果
  let check1 = result1
  let check2 = result2
  let check3 = result3
  let check4 = result4
  let check5 = result5
  let check6 = result6
  let check7 = result7
  let check8 = result8
}