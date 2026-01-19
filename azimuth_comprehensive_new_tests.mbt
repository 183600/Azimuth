// Azimuth 项目新增综合测试用例
// 使用标准 MoonBit 测试语法

test "string_length_calculation" {
  // 测试字符串长度计算
  let str1 = "Hello"
  let str2 = "MoonBit"
  let str3 = ""
  let str4 = "测试"
  
  // 验证字符串长度
  assert_eq(5, string_length(str1))
  assert_eq(7, string_length(str2))
  assert_eq(0, string_length(str3))
  assert_eq(2, string_length(str4))
}

test "array_operations" {
  // 测试数组操作
  let arr1 = [1, 2, 3, 4, 5]
  let arr2 = [10, 20, 30]
  let empty_arr = []
  
  // 测试数组长度
  assert_eq(5, array_length(arr1))
  assert_eq(3, array_length(arr2))
  assert_eq(0, array_length(empty_arr))
  
  // 测试数组元素访问
  assert_eq(1, array_get(arr1, 0))
  assert_eq(3, array_get(arr1, 2))
  assert_eq(30, array_get(arr2, 2))
}

test "boolean_logic" {
  // 测试布尔逻辑
  let true_val = true
  let false_val = false
  
  // 测试基本布尔运算
  assert_true(true_val && true_val)
  assert_false(true_val && false_val)
  assert_false(false_val && false_val)
  
  assert_true(true_val || true_val)
  assert_true(true_val || false_val)
  assert_false(false_val || false_val)
  
  assert_false(!true_val)
  assert_true(!false_val)
}

test "conditional_expressions" {
  // 测试条件表达式
  let x = 10
  let y = 20
  
  // 测试比较运算
  assert_true(x < y)
  assert_false(x > y)
  assert_true(x <= y)
  assert_false(x >= y)
  assert_true(x != y)
  assert_false(x == y)
  
  // 测试条件表达式结果
  let result = if (x < y) { x } else { y }
  assert_eq(10, result)
}

test "function_composition" {
  // 测试函数组合
  let numbers = [1, 2, 3, 4, 5]
  
  // 测试映射函数
  let doubled = map(numbers, fn(x) { x * 2 })
  assert_eq(2, array_get(doubled, 0))
  assert_eq(4, array_get(doubled, 1))
  assert_eq(10, array_get(doubled, 4))
  
  // 测试过滤函数
  let evens = filter(numbers, fn(x) { x % 2 == 0 })
  assert_eq(2, array_length(evens))
  assert_eq(2, array_get(evens, 0))
  assert_eq(4, array_get(evens, 1))
}

test "error_handling" {
  // 测试错误处理
  let result1 = safe_divide(10, 2)
  let result2 = safe_divide(10, 0)
  
  // 测试正常除法
  assert_eq(5, result1)
  
  // 测试除零错误处理
  assert_eq(0, result2)  // 假设 safe_divide 在除零时返回0
}

test "recursive_functions" {
  // 测试递归函数
  // 测试阶乘
  assert_eq(1, factorial(0))
  assert_eq(1, factorial(1))
  assert_eq(6, factorial(3))
  assert_eq(24, factorial(4))
  
  // 测试斐波那契数列
  assert_eq(0, fibonacci(0))
  assert_eq(1, fibonacci(1))
  assert_eq(1, fibonacci(2))
  assert_eq(2, fibonacci(3))
  assert_eq(3, fibonacci(4))
  assert_eq(5, fibonacci(5))
}

test "type_conversion" {
  // 测试类型转换
  let int_val = 42
  let float_val = 3.14
  let string_val = "123"
  
  // 测试整数到字符串转换
  assert_eq_string("42", int_to_string(int_val))
  
  // 测试字符串到整数转换
  assert_eq(123, string_to_int(string_val))
  
  // 测试浮点数操作
  let rounded = round(float_val)
  assert_eq(3, rounded)
}

test "data_structures" {
  // 测试数据结构
  // 测试元组
  let tuple1 = (1, "hello")
  assert_eq(1, tuple_first(tuple1))
  assert_eq_string("hello", tuple_second(tuple1))
  
  // 测试记录/结构体
  let person = { name: "Alice", age: 30 }
  assert_eq_string("Alice", person.name)
  assert_eq(30, person.age)
}

test "pattern_matching" {
  // 测试模式匹配
  let value = 5
  
  // 测试数值模式匹配
  let result = match value {
    0 => "zero"
    1 => "one"
    2..10 => "small"
    _ => "large"
  }
  
  assert_eq_string("small", result)
  
  // 测试可选类型模式匹配
  let some_value = some(42)
  let none_value = none
  
  assert_eq(42, unwrap_or(some_value, 0))
  assert_eq(0, unwrap_or(none_value, 0))
}