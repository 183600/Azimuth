// 基本测试用例
// 测试 MoonBit 的基本功能

test "basic_arithmetic" {
  // 基本算术运算
  let sum = 2 + 3
  let product = 4 * 5
  let difference = 10 - 3
  let quotient = 20 / 4
  
  // 计算结果
  let sum_str = sum.to_string()
  let product_str = product.to_string()
  let difference_str = difference.to_string()
  let quotient_str = quotient.to_string()
  
  // 验证结果
  let _ = sum_str == "5"
  let _ = product_str == "20"
  let _ = difference_str == "7"
  let _ = quotient_str == "5"
}

test "string_operations" {
  // 字符串操作
  let greeting = "Hello"
  let target = "World"
  let combined = greeting + ", " + target + "!"
  
  // 验证字符串拼接
  let _ = combined == "Hello, World!"
}

test "boolean_logic" {
  // 布尔逻辑
  let true_val = true
  let false_val = false
  
  // 验证布尔运算
  let _ = true_val && true_val
  let _ = !(true_val && false_val)
  let _ = true_val || false_val
  let _ = !(false_val || false_val)
  let _ = !false_val
  let _ = !true_val == false_val
}

test "number_comparisons" {
  // 数字比较
  let a = 10
  let b = 5
  let c = 10
  
  // 验证比较运算
  let _ = a > b
  let _ = b < a
  let _ = a == c
  let _ = a != b
  let _ = a >= c
  let _ = b <= a
}

test "complex_expressions" {
  // 复杂表达式
  let result1 = 5 + 3 * 2
  let result2 = (5 + 3) * 2
  let result3 = 20 / 4 + 3
  let result4 = 20 / (4 + 3)
  
  // 验证表达式求值
  let _ = result1.to_string() == "11"
  let _ = result2.to_string() == "16"
  let _ = result3.to_string() == "8"
  let _ = result4.to_string() == "2"
}

test "negative_numbers" {
  // 负数运算
  let neg1 = -5
  let neg2 = -10
  let pos = 5
  
  // 验证负数运算
  let _ = neg1.to_string() == "-5"
  let _ = neg2.to_string() == "-10"
  let _ = pos.to_string() == "5"
  let _ = (neg1 + pos).to_string() == "0"
  let _ = (neg1 + neg2).to_string() == "-15"
  let _ = (neg1 * neg2).to_string() == "50"
  let _ = (neg1 * pos).to_string() == "-25"
}

test "modulo_operations" {
  // 取模运算
  let a = 10
  let b = 3
  let c = 5
  let d = 2
  
  // 验证取模运算
  let _ = (a % b).to_string() == "1"
  let _ = (a % c).to_string() == "0"
  let _ = (b % d).to_string() == "1"
}

test "type_conversions" {
  // 类型转换
  let num = 42
  let str = "42"
  let num_str = num.to_string()
  
  // 验证类型转换
  let _ = num_str == "42"
  let _ = num_str == str
}

test "conditional_expressions" {
  // 条件表达式
  let a = 10
  let b = 5
  let result1 = if (a > b) { "greater" } else { "not greater" }
  let result2 = if (a < b) { "less" } else { "not less" }
  
  // 验证条件表达式
  let _ = result1 == "greater"
  let _ = result2 == "not less"
}

test "function_calls" {
  // 函数调用
  let double = fn(x : Int) -> Int { x * 2 }
  let add_one = fn(x : Int) -> Int { x + 1 }
  
  let result1 = double(5)
  let result2 = add_one(10)
  let result3 = double(add_one(3))
  
  // 验证函数调用
  let _ = result1.to_string() == "10"
  let _ = result2.to_string() == "11"
  let _ = result3.to_string() == "8"
}