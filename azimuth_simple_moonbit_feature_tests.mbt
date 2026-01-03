// Azimuth Simple MoonBit Feature Tests
// 简单的MoonBit语言特性测试

// 测试1: 高级字符串操作
test "高级字符串操作测试" {
  let text = "Azimuth Telemetry System"
  
  // 测试字符串分割
  let words = text.split(" ")
  assert_eq(words.length(), 3)
  assert_eq(words[0], "Azimuth")
  assert_eq(words[1], "Telemetry")
  assert_eq(words[2], "System")
  
  // 测试字符串替换
  let replaced = text.replace("Telemetry", "Monitoring")
  assert_eq(replaced, "Azimuth Monitoring System")
  
  // 测试字符串截取
  let substring = text.substring(0, 7)
  assert_eq(substring, "Azimuth")
  
  // 测试字符串去除空格
  let spaced = "  hello world  "
  assert_eq(spaced.trim(), "hello world")
}

// 测试2: 数组高级操作
test "数组高级操作测试" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // 测试数组切片
  let slice = numbers.slice(2, 6)
  assert_eq(slice, [3, 4, 5, 6])
  
  // 测试数组连接
  let more_numbers = [11, 12, 13]
  let combined = numbers.concat(more_numbers)
  assert_eq(combined.length(), 13)
  assert_eq(combined[10], 11)
  
  // 测试数组查找
  let found = numbers.find(fn(x) { x > 5 })
  assert_eq(found, Some(6))
  
  let not_found = numbers.find(fn(x) { x > 20 })
  assert_eq(not_found, None)
  
  // 测试数组包含
  assert_true(numbers.contains(5))
  assert_false(numbers.contains(15))
}

// 测试3: 选项类型操作
test "选项类型操作测试" {
  let some_value = Some(42)
  let none_value = None[Int]
  
  // 测试选项类型匹配
  let result1 = match some_value {
    Some(x) => x * 2
    None => 0
  }
  assert_eq(result1, 84)
  
  let result2 = match none_value {
    Some(x) => x * 2
    None => 0
  }
  assert_eq(result2, 0)
  
  // 测试选项类型映射
  let mapped1 = some_value.map(fn(x) { x + 10 })
  assert_eq(mapped1, Some(52))
  
  let mapped2 = none_value.map(fn(x) { x + 10 })
  assert_eq(mapped2, None)
  
  // 测试选项类型默认值
  let with_default1 = some_value.with_default(0)
  assert_eq(with_default1, 42)
  
  let with_default2 = none_value.with_default(0)
  assert_eq(with_default2, 0)
}

// 测试4: 结果类型操作
test "结果类型操作测试" {
  let ok_result = Ok("success")
  let err_result = Err("error occurred")
  
  // 测试结果类型匹配
  let result1 = match ok_result {
    Ok(value) => "Got: " + value
    Err(msg) => "Error: " + msg
  }
  assert_eq(result1, "Got: success")
  
  let result2 = match err_result {
    Ok(value) => "Got: " + value
    Err(msg) => "Error: " + msg
  }
  assert_eq(result2, "Error: error occurred")
  
  // 测试结果类型映射
  let mapped1 = ok_result.map(fn(x) { x.length() })
  assert_eq(mapped1, Ok(7))
  
  let mapped2 = err_result.map(fn(x) { x.length() })
  assert_eq(mapped2, Err("error occurred"))
}

// 测试5: 元组操作
test "元组操作测试" {
  let pair = (10, "hello")
  let triple = (1, 2, 3)
  
  // 测试元组访问
  assert_eq(pair.0, 10)
  assert_eq(pair.1, "hello")
  
  assert_eq(triple.0, 1)
  assert_eq(triple.1, 2)
  assert_eq(triple.2, 3)
  
  // 测试元组比较
  let another_pair = (10, "hello")
  let different_pair = (20, "world")
  
  assert_eq(pair, another_pair)
  assert_true(pair != different_pair)
}

// 测试6: 记录类型操作
test "记录类型操作测试" {
  // 定义记录类型
  type Point {
    x : Int
    y : Int
  }
  
  type Person {
    name : String
    age : Int
    email : Option[String]
  }
  
  // 创建记录实例
  let point = { x: 10, y: 20 }
  let person = { name: "Alice", age: 30, email: Some("alice@example.com") }
  let person_no_email = { name: "Bob", age: 25, email: None[String] }
  
  // 访问记录字段
  assert_eq(point.x, 10)
  assert_eq(point.y, 20)
  
  assert_eq(person.name, "Alice")
  assert_eq(person.age, 30)
  assert_eq(person.email, Some("alice@example.com"))
  
  assert_eq(person_no_email.name, "Bob")
  assert_eq(person_no_email.age, 25)
  assert_eq(person_no_email.email, None)
  
  // 记录更新
  let updated_point = { point | y: 30 }
  assert_eq(updated_point.x, 10)
  assert_eq(updated_point.y, 30)
  
  let updated_person = { person | age: 31 }
  assert_eq(updated_person.name, "Alice")
  assert_eq(updated_person.age, 31)
  assert_eq(updated_person.email, Some("alice@example.com"))
}

// 测试7: 函数式编程特性
test "函数式编程特性测试" {
  // 高阶函数
  let apply_twice = fn(f, x) { f(f(x)) }
  
  let add_one = fn(x) { x + 1 }
  let result1 = apply_twice(add_one, 5)
  assert_eq(result1, 7)
  
  // 函数组合
  let compose = fn(f, g) { fn(x) { f(g(x)) } }
  
  let add_two = fn(x) { x + 2 }
  let multiply_by_three = fn(x) { x * 3 }
  
  let combined = compose(multiply_by_three, add_two)
  let result2 = combined(4)
  assert_eq(result2, 18) // (4 + 2) * 3 = 18
  
  // 柯里化
  let curried_add = fn(x) { fn(y) { x + y } }
  
  let add_five = curried_add(5)
  let result3 = add_five(10)
  assert_eq(result3, 15)
}

// 测试8: 递归函数
test "递归函数测试" {
  // 阶乘函数
  let factorial = fn(n) {
    if n <= 1 { 1 } else { n * factorial(n - 1) }
  }
  
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(5), 120)
  
  // 斐波那契数列
  let fibonacci = fn(n) {
    if n <= 1 { n } else { fibonacci(n - 1) + fibonacci(n - 2) }
  }
  
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(5), 5)
  assert_eq(fibonacci(6), 8)
  
  // 列表求和
  let list_sum = fn(lst) {
    match lst {
      [] => 0
      [head, ..rest] => head + list_sum(rest)
    }
  }
  
  assert_eq(list_sum([]), 0)
  assert_eq(list_sum([1, 2, 3, 4, 5]), 15)
}

// 测试9: 模式匹配高级特性
test "模式匹配高级特性测试" {
  // 嵌套模式匹配
  type Expr {
    Number(Int)
    Add(Expr, Expr)
    Multiply(Expr, Expr)
  }
  
  let eval = fn(expr) {
    match expr {
      Number(n) => n
      Add(left, right) => eval(left) + eval(right)
      Multiply(left, right) => eval(left) * eval(right)
    }
  }
  
  let expr1 = Add(Number(3), Number(5))
  let expr2 = Multiply(Number(4), Add(Number(2), Number(3)))
  
  assert_eq(eval(expr1), 8)
  assert_eq(eval(expr2), 20)
  
  // 守卫模式
  let describe_number = fn(n) {
    match n {
      x if x < 0 => "negative"
      0 => "zero"
      x if x > 0 && x < 10 => "small positive"
      _ => "large positive"
    }
  }
  
  assert_eq(describe_number(-5), "negative")
  assert_eq(describe_number(0), "zero")
  assert_eq(describe_number(5), "small positive")
  assert_eq(describe_number(15), "large positive")
}

// 测试10: 类型转换和类型检查
test "类型转换和类型检查测试" {
  // 数值类型转换
  let int_val = 42
  let float_val = int_val.to_float()
  
  assert_eq(int_val, 42)
  assert_true(float_val > 41.9 && float_val < 42.1)
  
  // 字符串转换
  let string_from_int = int_val.to_string()
  assert_eq(string_from_int, "42")
  
  let parsed_int = "123".parse_int()
  assert_eq(parsed_int, Some(123))
  
  let invalid_int = "abc".parse_int()
  assert_eq(invalid_int, None)
  
  // 类型断言
  let any_value = "hello" as Any
  let string_value = any_value.as_string()
  assert_eq(string_value, Some("hello"))
  
  let numeric_value = 123 as Any
  let not_string = numeric_value.as_string()
  assert_eq(not_string, None)
}