// Azimuth 项目新增标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，提供简洁而全面的测试覆盖

test "string_length_validation" {
  // 测试字符串长度验证
  let short_str = "Hi"
  let medium_str = "Hello, World!"
  let long_str = "This is a longer string with multiple words"
  
  // 计算字符串长度并验证
  let short_len = short_str.length()
  let medium_len = medium_str.length()
  let long_len = long_str.length()
  
  @assertion.assert_eq(2, short_len)?
  @assertion.assert_eq(13, medium_len)?
  @assertion.assert_eq(42, long_len)?
}

test "array_operations" {
  // 测试数组基本操作
  let numbers = [1, 2, 3, 4, 5]
  
  // 获取数组长度
  let length = numbers.length()
  @assertion.assert_eq(5, length)?
  
  // 访问数组元素
  let first = numbers[0]
  let last = numbers[4]
  @assertion.assert_eq(1, first)?
  @assertion.assert_eq(5, last)?
}

test "conditional_logic" {
  // 测试条件逻辑
  let x = 10
  let y = 20
  
  // 条件表达式测试
  let greater = if (x > y) { x } else { y }
  let smaller = if (x < y) { x } else { y }
  
  @assertion.assert_eq(20, greater)?
  @assertion.assert_eq(10, smaller)?
}

test "function_composition" {
  // 测试函数组合
  // 定义简单的转换函数
  let double = fn(x) { x * 2 }
  let add_five = fn(x) { x + 5 }
  
  // 组合函数：先加倍再加5
  let value = 10
  let result = add_five(double(value))
  
  @assertion.assert_eq(25, result)?
}

test "loop_accumulation" {
  // 测试循环累积
  let numbers = [1, 2, 3, 4, 5]
  let sum = ref 0
  
  // 使用循环计算总和
  for i = 0; i < numbers.length(); i = i + 1 {
    sum := sum + numbers[i]
  }
  
  @assertion.assert_eq(15, sum)?
}

test "pattern_matching" {
  // 测试模式匹配
  let value = 42
  let result = match value {
    0 => "zero"
    1 => "one"
    2..10 => "small"
    11..100 => "medium"
    _ => "large"
  }
  
  @assertion.assert_eq("medium", result)?
}

test "error_handling" {
  // 测试错误处理
  let safe_divide = fn(a, b) {
    if (b == 0) {
      { error: "Division by zero" }
    } else {
      { value: a / b }
    }
  }
  
  let result1 = safe_divide(10, 2)
  let result2 = safe_divide(10, 0)
  
  match result1 {
    { value: v } => @assertion.assert_eq(5, v)?
    { error: _ } => @assertion.assert_fail("Expected success")?
  }
  
  match result2 {
    { value: _ } => @assertion.assert_fail("Expected error")?
    { error: msg } => @assertion.assert_eq("Division by zero", msg)?
  }
}

test "data_structures" {
  // 测试数据结构
  // 定义一个简单的点结构
  type Point Point(Int, Int)
  
  let p1 = Point(3, 4)
  let p2 = Point(5, 12)
  
  // 计算两点之间的距离平方
  let distance_sq = match p1 {
    Point(x1, y1) => match p2 {
      Point(x2, y2) => (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)
    }
  }
  
  @assertion.assert_eq(89, distance_sq)?
}

test "recursive_function" {
  // 测试递归函数
  // 计算阶乘
  let factorial = fn(n) {
    if (n <= 1) {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  
  let result1 = factorial(0)
  let result2 = factorial(1)
  let result3 = factorial(5)
  
  @assertion.assert_eq(1, result1)?
  @assertion.assert_eq(1, result2)?
  @assertion.assert_eq(120, result3)?
}

test "higher_order_functions" {
  // 测试高阶函数
  let numbers = [1, 2, 3, 4, 5]
  
  // 使用 map 函数转换数组
  let doubled = numbers.map(fn(x) { x * 2 })
  let squared = numbers.map(fn(x) { x * x })
  
  // 使用 filter 函数筛选数组
  let evens = numbers.filter(fn(x) { x % 2 == 0 })
  let greater_than_three = numbers.filter(fn(x) { x > 3 })
  
  @assertion.assert_eq([2, 4, 6, 8, 10], doubled)?
  @assertion.assert_eq([1, 4, 9, 16, 25], squared)?
  @assertion.assert_eq([2, 4], evens)?
  @assertion.assert_eq([4, 5], greater_than_three)?
}