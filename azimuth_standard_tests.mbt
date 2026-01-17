// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，覆盖多种实用场景

test "array_operations" {
  // 测试数组操作
  let arr1 = [1, 2, 3, 4, 5]
  let arr2 = [10, 20, 30]
  let arr3 = []
  
  // 数组长度操作
  let len1 = Array.length(arr1)
  let len2 = Array.length(arr2)
  let len3 = Array.length(arr3)
  
  // 使用 _ 忽略结果，返回 Unit
  let _ = len1
  let _ = len2
  let _ = len3
}

test "boolean_logic" {
  // 测试布尔逻辑
  let true_val = true
  let false_val = false
  
  // 基本逻辑运算
  let and_result = true_val && false_val
  let or_result = true_val || false_val
  let not_result = !false_val
  
  // 复合逻辑运算
  let complex_and = true_val && false_val || true_val
  let complex_or = false_val || false_val && true_val
  
  // 使用 _ 忽略结果，返回 Unit
  let _ = and_result
  let _ = or_result
  let _ = not_result
  let _ = complex_and
  let _ = complex_or
}

test "conditional_expressions" {
  // 测试条件表达式
  let x = 10
  let y = 20
  
  // 基本条件表达式
  let max_val = if x > y { x } else { y }
  let min_val = if x < y { x } else { y }
  
  // 嵌套条件表达式
  let comparison = 
    if x > y { "x is greater" }
    else if x < y { "y is greater" }
    else { "x equals y" }
  
  // 使用 _ 忽略结果，返回 Unit
  let _ = max_val
  let _ = min_val
  let _ = comparison
}

test "string_manipulation" {
  // 测试字符串操作
  let str1 = "Hello"
  let str2 = "MoonBit"
  let str3 = "World"
  
  // 字符串连接
  let greeting = str1 + ", " + str2 + "!"
  let message = str1 + " " + str3
  
  // 字符串长度
  let len1 = String.length(str1)
  let len2 = String.length(str2)
  let len3 = String.length(message)
  
  // 使用 _ 忽略结果，返回 Unit
  let _ = greeting
  let _ = message
  let _ = len1
  let _ = len2
  let _ = len3
}

test "numeric_conversions" {
  // 测试数值转换
  let int_val = 42
  let float_val = 3.14
  
  // 基本运算
  let int_add = int_val + 10
  let int_mult = int_val * 2
  let int_div = int_val / 2
  
  // 浮点运算
  let float_add = float_val + 1.0
  let float_mult = float_val * 2.0
  let float_div = float_val / 2.0
  
  // 使用 _ 忽略结果，返回 Unit
  let _ = int_add
  let _ = int_mult
  let _ = int_div
  let _ = float_add
  let _ = float_mult
  let _ = float_div
}

test "pattern_matching" {
  // 测试模式匹配
  let option_val1 = Some(42)
  let option_val2 = None
  let list_val = [1, 2, 3]
  
  // Option 模式匹配
  let result1 = match option_val1 {
    Some(x) => x * 2
    None => 0
  }
  
  let result2 = match option_val2 {
    Some(x) => x * 2
    None => 0
  }
  
  // 列表模式匹配
  let result3 = match list_val {
    [] => 0
    [x] => x
    [x, y] => x + y
    [x, y, ...rest] => x + y
  }
  
  // 使用 _ 忽略结果，返回 Unit
  let _ = result1
  let _ = result2
  let _ = result3
}

test "function_composition" {
  // 测试函数组合
  let add_one = fn(x) { x + 1 }
  let multiply_two = fn(x) { x * 2 }
  
  // 函数组合
  let add_then_multiply = fn(x) { multiply_two(add_one(x)) }
  let multiply_then_add = fn(x) { add_one(multiply_two(x)) }
  
  // 应用组合函数
  let result1 = add_then_multiply(5)  // (5 + 1) * 2 = 12
  let result2 = multiply_then_add(5)  // (5 * 2) + 1 = 11
  
  // 使用 _ 忽略结果，返回 Unit
  let _ = result1
  let _ = result2
}

test "error_handling" {
  // 测试错误处理
  let divide = fn(a, b) {
    if b == 0 {
      Error("Division by zero")
    } else {
      Ok(a / b)
    }
  }
  
  // 正常情况
  let result1 = divide(10, 2)
  
  // 错误情况
  let result2 = divide(10, 0)
  
  // 处理结果
  let final_result1 = match result1 {
    Ok(value) => value * 2
    Error(_) => 0
  }
  
  let final_result2 = match result2 {
    Ok(value) => value * 2
    Error(_) => 0
  }
  
  // 使用 _ 忽略结果，返回 Unit
  let _ = result1
  let _ = result2
  let _ = final_result1
  let _ = final_result2
}

test "record_operations" {
  // 测试记录操作
  type Person = { name: String, age: Int, active: Bool }
  
  let person1 = { name: "Alice", age: 30, active: true }
  let person2 = { name: "Bob", age: 25, active: false }
  
  // 记录字段访问
  let name1 = person1.name
  let age1 = person1.age
  let active1 = person1.active
  
  // 记录更新
  let person1_updated = { person1 | age: 31 }
  let person2_activated = { person2 | active: true }
  
  // 使用 _ 忽略结果，返回 Unit
  let _ = name1
  let _ = age1
  let _ = active1
  let _ = person1_updated
  let _ = person2_activated
}

test "pipeline_operations" {
  // 测试管道操作
  let numbers = [1, 2, 3, 4, 5]
  
  // 管道操作链
  let result = numbers
    |> Array.map(fn(x) { x * 2 })
    |> Array.filter(fn(x) { x > 4 })
    |> Array.fold(fn(acc, x) { acc + x }, 0)
  
  // 分步操作
  let doubled = Array.map(fn(x) { x * 2 }, numbers)
  let filtered = Array.filter(fn(x) { x > 4 }, doubled)
  let summed = Array.fold(fn(acc, x) { acc + x }, filtered, 0)
  
  // 使用 _ 忽略结果，返回 Unit
  let _ = result
  let _ = doubled
  let _ = filtered
  let _ = summed
}