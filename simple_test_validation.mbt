// 简单的测试验证文件
// 这个文件用于验证 MoonBit 测试语法是否正确

// 测试用例1: 基本算术运算
test "basic_arithmetic" {
  let add_result = 2 + 3
  let multiply_result = 2 * 3
  
  // 简单验证
  let _ = add_result + multiply_result
}

// 测试用例2: 字符串操作
test "string_operations" {
  let greet_result = "Hello, World!"
  let name = "MoonBit"
  let combined = greet_result + " " + name
  
  // 简单验证
  let _ = combined.length()
}

// 测试用例3: 布尔逻辑
test "boolean_logic" {
  let true_val = true
  let false_val = false
  let and_result = true_val && false_val
  let or_result = true_val || false_val
  
  // 简单验证
  let _ = if and_result { 1 } else { 0 }
  let _ = if or_result { 1 } else { 0 }
}

// 测试用例4: 条件分支
test "conditional_branches" {
  let x = 10
  let result = if x > 5 { "greater" } else { "less or equal" }
  
  // 简单验证
  let _ = result.length()
}

// 测试用例5: 循环结构
test "loop_structures" {
  let mut sum = 0
  let mut i = 0
  while (i < 5) {
    sum = sum + i
    i = i + 1
  }
  
  // 简单验证
  let _ = sum
}

// 测试用例6: 函数定义和调用
test "function_definition_and_call" {
  // 内部函数定义
  fn square(n : Int) -> Int {
    n * n
  }
  
  let result = square(5)
  
  // 简单验证
  let _ = result
}

// 测试用例7: 数组操作（如果支持）
test "array_operations" {
  // 假设 MoonBit 支持数组
  let numbers = [1, 2, 3, 4, 5]
  let first = numbers[0]
  
  // 简单验证
  let _ = first
}

// 测试用例8: 比较操作
test "comparison_operations" {
  let a = 10
  let b = 20
  
  let equal_result = a == b
  let not_equal_result = a != b
  let less_result = a < b
  let greater_result = a > b
  let less_equal_result = a <= b
  let greater_equal_result = a >= b
  
  // 简单验证
  let _ = if equal_result { 1 } else { 0 }
  let _ = if not_equal_result { 1 } else { 0 }
  let _ = if less_result { 1 } else { 0 }
  let _ = if greater_result { 1 } else { 0 }
  let _ = if less_equal_result { 1 } else { 0 }
  let _ = if greater_equal_result { 1 } else { 0 }
}

// 测试用例9: 类型转换（如果支持）
test "type_conversion" {
  let int_val = 42
  // 假设支持到字符串的转换
  // let string_val = int_val.to_string()
  
  // 简单验证
  let _ = int_val
}

// 测试用例10: 错误处理（如果支持）
test "error_handling" {
  let x = 10
  let y = 0
  
  // 简单验证，避免除零错误
  let result = if y != 0 { x / y } else { 0 }
  let _ = result
}