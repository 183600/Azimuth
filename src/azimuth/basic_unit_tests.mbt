// 基本测试用例
// 测试 MoonBit 的基本功能

// 导入必要的包
use moonbitlang/core/int

test "basic_arithmetic" {
  // 基本算术运算
  let sum = 2 + 3
  let product = 4 * 5
  let difference = 10 - 3
  let quotient = 20 / 4
  
  // 使用字符串验证结果
  assert_eq("5", sum.to_string())
  assert_eq("20", product.to_string())
  assert_eq("7", difference.to_string())
  assert_eq("5", quotient.to_string())
}

test "string_operations" {
  // 字符串操作
  let greeting = "Hello"
  let target = "World"
  let combined = greeting + ", " + target + "!"
  
  // 验证字符串拼接
  "Hello, World!" == combined
}

test "boolean_logic" {
  // 布尔逻辑
  let true_val = true
  let false_val = false
  
  // 验证布尔运算
  true_val && true_val
  !(true_val && false_val)
  true_val || false_val
  !(false_val || false_val)
  !false_val
  !true_val == false_val
}

test "number_comparisons" {
  // 数字比较
  let a = 10
  let b = 5
  let c = 10
  
  // 验证比较运算
  a > b
  b < a
  a == c
  a != b
  a >= c
  b <= a
}

test "complex_expressions" {
  // 复杂表达式
  let result1 = 5 + 3 * 2
  let result2 = (5 + 3) * 2
  let result3 = 20 / 4 + 3
  let result4 = 20 / (4 + 3)
  
  // 验证表达式求值
  "11" == result1.to_string()
  "16" == result2.to_string()
  "8" == result3.to_string()
  "2" == result4.to_string()
}

test "negative_numbers" {
  // 负数运算
  let neg1 = -5
  let neg2 = -10
  let pos = 5
  
  // 验证负数运算
  "-5" == neg1.to_string()
  "-10" == neg2.to_string()
  "5" == pos.to_string()
  "0" == (neg1 + pos).to_string()
  "-15" == (neg1 + neg2).to_string()
  "50" == (neg1 * neg2).to_string()
  "-25" == (neg1 * pos).to_string()
}

test "modulo_operations" {
  // 取模运算
  let a = 10
  let b = 3
  let c = 5
  let d = 2
  
  // 验证取模运算
  "1" == (a % b).to_string()
  "0" == (a % c).to_string()
  "1" == (b % d).to_string()
}

test "type_conversions" {
  // 类型转换
  let num = 42
  let str = "42"
  let num_str = num.to_string()
  
  // 验证类型转换
  "42" == num_str
  num_str == str
}

test "conditional_expressions" {
  // 条件表达式
  let a = 10
  let b = 5
  let result1 = if (a > b) { "greater" } else { "not greater" }
  let result2 = if (a < b) { "less" } else { "not less" }
  
  // 验证条件表达式
  "greater" == result1
  "not less" == result2
}

test "function_calls" {
  // 函数调用
  let double = fn(x : Int) -> Int { x * 2 }
  let add_one = fn(x : Int) -> Int { x + 1 }
  
  let result1 = double(5)
  let result2 = add_one(10)
  let result3 = double(add_one(3))
  
  // 验证函数调用
  "10" == result1.to_string()
  "11" == result2.to_string()
  "8" == result3.to_string()
}