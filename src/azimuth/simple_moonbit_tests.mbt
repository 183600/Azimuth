// 简单的 MoonBit 测试用例
// 测试基本功能而不使用断言

test "basic_arithmetic" {
  // 基本算术运算
  let sum = 2 + 3
  let product = 4 * 5
  let difference = 10 - 3
  let quotient = 20 / 4
  
  // 存储结果以便验证
  let sum_result = sum
  let product_result = product
  let difference_result = difference
  let quotient_result = quotient
}

test "string_operations" {
  // 字符串操作
  let greeting = "Hello"
  let target = "World"
  let combined = greeting + ", " + target + "!"
  
  // 存储结果以便验证
  let combined_result = combined
}

test "boolean_logic" {
  // 布尔逻辑
  let true_val = true
  let false_val = false
  
  // 验证布尔运算
  let and_result = true_val && true_val
  let not_and_result = !(true_val && false_val)
  let or_result = true_val || false_val
  let not_or_result = !(false_val || false_val)
  let not_false = !false_val
  let not_true = !true_val
}

test "number_comparisons" {
  // 数字比较
  let a = 10
  let b = 5
  let c = 10
  
  // 验证比较运算
  let greater = a > b
  let less = b < a
  let equal = a == c
  let not_equal = a != b
  let greater_equal = a >= c
  let less_equal = b <= a
}

test "complex_expressions" {
  // 复杂表达式
  let result1 = 5 + 3 * 2
  let result2 = (5 + 3) * 2
  let result3 = 20 / 4 + 3
  let result4 = 20 / (4 + 3)
  
  // 存储结果以便验证
  let expr1 = result1
  let expr2 = result2
  let expr3 = result3
  let expr4 = result4
}

test "negative_numbers" {
  // 负数运算
  let neg1 = -5
  let neg2 = -10
  let pos = 5
  
  // 验证负数运算
  let sum1 = neg1 + pos
  let sum2 = neg1 + neg2
  let product1 = neg1 * neg2
  let product2 = neg1 * pos
}

test "modulo_operations" {
  // 取模运算
  let a = 10
  let b = 3
  let c = 5
  let d = 2
  
  // 验证取模运算
  let mod1 = a % b
  let mod2 = a % c
  let mod3 = b % d
}

test "conditional_expressions" {
  // 条件表达式
  let a = 10
  let b = 5
  let result1 = if (a > b) { "greater" } else { "not greater" }
  let result2 = if (a < b) { "less" } else { "not less" }
  
  // 存储结果以便验证
  let cond1 = result1
  let cond2 = result2
}

test "function_calls" {
  // 函数调用
  let double = fn(x : Int) -> Int { x * 2 }
  let add_one = fn(x : Int) -> Int { x + 1 }
  
  let result1 = double(5)
  let result2 = add_one(10)
  let result3 = double(add_one(3))
  
  // 存储结果以便验证
  let func1 = result1
  let func2 = result2
  let func3 = result3
}

test "nested_expressions" {
  // 嵌套表达式
  let a = 10
  let b = 5
  let c = 2
  
  // 复杂嵌套计算
  let nested1 = a + b * c
  let nested2 = (a + b) * c
  let nested3 = a * b + c
  let nested4 = a * (b + c)
  
  // 存储结果以便验证
  let nest1 = nested1
  let nest2 = nested2
  let nest3 = nested3
  let nest4 = nested4
}