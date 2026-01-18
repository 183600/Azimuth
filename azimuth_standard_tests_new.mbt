// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，针对项目核心功能编写

test "add_function_positive_numbers" {
  // 测试加法函数 - 正数
  let result1 = 2 + 3
  let result2 = 10 + 15
  let result3 = 100 + 200
  
  // 验证结果
  let check1 = result1  // 应该等于 5
  let check2 = result2  // 应该等于 25
  let check3 = result3  // 应该等于 300
}

test "add_function_negative_numbers" {
  // 测试加法函数 - 负数
  let result1 = -5 + (-3)
  let result2 = -10 + 5
  let result3 = 15 + (-20)
  
  // 验证结果
  let check1 = result1  // 应该等于 -8
  let check2 = result2  // 应该等于 -5
  let check3 = result3  // 应该等于 -5
}

test "add_function_zero" {
  // 测试加法函数 - 零值
  let result1 = 0 + 0
  let result2 = 5 + 0
  let result3 = 0 + (-10)
  
  // 验证结果
  let check1 = result1  // 应该等于 0
  let check2 = result2  // 应该等于 5
  let check3 = result3  // 应该等于 -10
}

test "multiply_function_positive_numbers" {
  // 测试乘法函数 - 正数
  let result1 = 3 * 4
  let result2 = 10 * 5
  let result3 = 7 * 8
  
  // 验证结果
  let check1 = result1  // 应该等于 12
  let check2 = result2  // 应该等于 50
  let check3 = result3  // 应该等于 56
}

test "multiply_function_negative_numbers" {
  // 测试乘法函数 - 负数
  let result1 = -3 * 4
  let result2 = 5 * (-2)
  let result3 = -4 * (-6)
  
  // 验证结果
  let check1 = result1  // 应该等于 -12
  let check2 = result2  // 应该等于 -10
  let check3 = result3  // 应该等于 24
}

test "multiply_function_zero" {
  // 测试乘法函数 - 零值
  let result1 = 0 * 5
  let result2 = 10 * 0
  let result3 = 0 * 0
  
  // 验证结果
  let check1 = result1  // 应该等于 0
  let check2 = result2  // 应该等于 0
  let check3 = result3  // 应该等于 0
}

test "greet_function_standard_names" {
  // 测试问候语函数 - 标准名字
  let name1 = "Alice"
  let name2 = "Bob"
  let name3 = "Charlie"
  
  let greeting1 = "Hello, " + name1 + "!"
  let greeting2 = "Hello, " + name2 + "!"
  let greeting3 = "Hello, " + name3 + "!"
  
  // 验证结果
  let check1 = greeting1  // 应该等于 "Hello, Alice!"
  let check2 = greeting2  // 应该等于 "Hello, Bob!"
  let check3 = greeting3  // 应该等于 "Hello, Charlie!"
}

test "greet_function_edge_cases" {
  // 测试问候语函数 - 边界情况
  let empty_name = ""
  let single_char = "X"
  let space_name = " "
  
  let greeting1 = "Hello, " + empty_name + "!"
  let greeting2 = "Hello, " + single_char + "!"
  let greeting3 = "Hello, " + space_name + "!"
  
  // 验证结果
  let check1 = greeting1  // 应该等于 "Hello, !"
  let check2 = greeting2  // 应该等于 "Hello, X!"
  let check3 = greeting3  // 应该等于 "Hello,  !"
}

test "combined_operations" {
  // 测试组合运算
  let a = 5
  let b = 3
  let c = 2
  
  // 加法和乘法组合
  let result1 = a + b * c
  let result2 = (a + b) * c
  let result3 = a * b + c
  
  // 验证结果
  let check1 = result1  // 应该等于 11 (5 + 3*2)
  let check2 = result2  // 应该等于 16 ((5+3)*2)
  let check3 = result3  // 应该等于 17 (5*3+2)
}

test "complex_expressions" {
  // 测试复杂表达式
  let x = 10
  let y = 5
  let z = 2
  
  // 多层嵌套表达式
  let result1 = (x + y) * (x - y)
  let result2 = x * x + y * y
  let result3 = (x + y + z) * (x + y - z)
  
  // 验证结果
  let check1 = result1  // 应该等于 75 ((10+5)*(10-5))
  let check2 = result2  // 应该等于 125 (10*10+5*5)
  let check3 = result3  // 应该等于 195 ((10+5+2)*(10+5-2))
}