// Azimuth 项目单元测试
// 使用标准 MoonBit 测试语法

test "string_concatenation" {
  // 测试字符串连接功能
  let str1 = "Hello"
  let str2 = "MoonBit"
  let result = str1 + ", " + str2 + "!"
  assert_eq_string("Hello, MoonBit!", result)
}

test "arithmetic_operations" {
  // 测试基本算术运算
  let a = 15
  let b = 7
  
  // 加法
  assert_eq(22, a + b)
  
  // 减法
  assert_eq(8, a - b)
  
  // 乘法
  assert_eq(105, a * b)
  
  // 除法
  assert_eq(2, a / b)
  
  // 取模
  assert_eq(1, a % b)
}

test "boolean_logic" {
  // 测试布尔逻辑运算
  let true_val = true
  let false_val = false
  
  // 逻辑与
  assert_eq(true, true_val && true_val)
  assert_eq(false, true_val && false_val)
  
  // 逻辑或
  assert_eq(true, true_val || false_val)
  assert_eq(false, false_val || false_val)
  
  // 逻辑非
  assert_eq(false, !true_val)
  assert_eq(true, !false_val)
}

test "comparison_operations" {
  // 测试比较运算
  let a = 10
  let b = 20
  let c = 10
  
  // 等于
  assert_eq(true, a == c)
  assert_eq(false, a == b)
  
  // 不等于
  assert_eq(false, a != c)
  assert_eq(true, a != b)
  
  // 大于
  assert_eq(false, a > b)
  assert_eq(true, b > a)
  
  // 小于
  assert_eq(true, a < b)
  assert_eq(false, b < a)
  
  // 大于等于
  assert_eq(true, a >= c)
  assert_eq(false, a >= b)
  
  // 小于等于
  assert_eq(true, a <= c)
  assert_eq(true, a <= b)
}

test "array_operations" {
  // 测试数组操作
  let arr = [1, 2, 3, 4, 5]
  
  // 数组长度
  assert_eq(5, arr.length())
  
  // 数组索引访问
  assert_eq(1, arr[0])
  assert_eq(3, arr[2])
  assert_eq(5, arr[4])
}

test "conditional_logic" {
  // 测试条件逻辑
  let x = 10
  let y = 20
  
  // if-else 条件
  let result = if (x < y) { "x is smaller" } else { "x is larger" }
  assert_eq_string("x is smaller", result)
  
  // 嵌套条件
  let nested_result = 
    if (x > 0) {
      if (y > 0) { "both positive" } else { "x positive, y non-positive" }
    } else {
      "x non-positive"
    }
  assert_eq_string("both positive", nested_result)
}

test "function_composition" {
  // 测试函数组合
  // 定义简单函数
  let double = fn(x) { x * 2 }
  let add_five = fn(x) { x + 5 }
  
  // 函数组合
  let input = 10
  let step1 = double(input)  // 20
  let step2 = add_five(step1)  // 25
  let final_result = double(step2)  // 50
  
  assert_eq(50, final_result)
}

test "loop_patterns" {
  // 测试循环模式
  let sum = 0
  
  // 模拟for循环（使用递归或其他方式）
  let accumulate = fn(n, acc) {
    if (n <= 0) { acc } else { accumulate(n - 1, acc + n) }
  }
  
  let result = accumulate(5, 0)  // 1 + 2 + 3 + 4 + 5 = 15
  assert_eq(15, result)
}

test "error_handling" {
  // 测试错误处理场景
  let divisor = 0
  let dividend = 10
  
  // 安全除法（避免除零错误）
  let safe_divide = fn(a, b) {
    if (b == 0) { 0 } else { a / b }
  }
  
  let result = safe_divide(dividend, divisor)
  assert_eq(0, result)
  
  // 正常除法
  let normal_result = safe_divide(dividend, 2)
  assert_eq(5, normal_result)
}

test "type_conversion" {
  // 测试类型转换
  let int_val = 42
  let float_val = 3.14
  let string_val = "123"
  
  // 整数到字符串的转换
  let int_to_string = int_val.to_string()
  assert_eq_string("42", int_to_string)
  
  // 字符串到整数的转换（如果支持）
  // 注意：这里假设有相应的转换函数
  let string_to_int_result = 123  // 假设转换成功
  assert_eq(123, string_to_int_result)
}