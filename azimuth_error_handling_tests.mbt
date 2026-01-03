// Azimuth 错误处理测试
// 专注于测试错误处理和异常管理功能

// 测试1: Option类型处理
test "Option类型处理测试" {
  let some_value = Some(42)
  let none_value = None
  
  assert_eq(option_is_some(some_value), true)
  assert_eq(option_is_none(some_value), false)
  assert_eq(option_unwrap_or(some_value, 0), 42)
  
  assert_eq(option_is_some(none_value), false)
  assert_eq(option_is_none(none_value), true)
  assert_eq(option_unwrap_or(none_value, 0), 0)
  
  // 测试map操作
  let mapped_some = option_map(some_value, fn(x) { x * 2 })
  assert_eq(mapped_some, Some(84))
  
  let mapped_none = option_map(none_value, fn(x) { x * 2 })
  assert_eq(mapped_none, None)
}

fn option_is_some[T](opt : Option[T]) -> Bool {
  match opt {
    Some(_) => true
    None => false
  }
}

fn option_is_none[T](opt : Option[T]) -> Bool {
  match opt {
    Some(_) => false
    None => true
  }
}

fn option_unwrap_or[T](opt : Option[T], default : T) -> T {
  match opt {
    Some(value) => value
    None => default
  }
}

fn option_map[T, U](opt : Option[T], f : (T) -> U) -> Option[U] {
  match opt {
    Some(value) => Some(f(value))
    None => None
  }
}

// 测试2: Result类型处理
test "Result类型处理测试" {
  let ok_value = Ok(42)
  let err_value = Err("Error message")
  
  assert_eq(result_is_ok(ok_value), true)
  assert_eq(result_is_err(ok_value), false)
  assert_eq(result_unwrap_or(ok_value, 0), 42)
  
  assert_eq(result_is_ok(err_value), false)
  assert_eq(result_is_err(err_value), true)
  assert_eq(result_unwrap_or(err_value, 0), 0)
  
  // 测试map操作
  let mapped_ok = result_map(ok_value, fn(x) { x * 2 })
  assert_eq(mapped_ok, Ok(84))
  
  let mapped_err = result_map(err_value, fn(x) { x * 2 })
  assert_eq(mapped_err, Err("Error message"))
}

type Result[T, E] {
  Ok(T)
  Err(E)
}

fn result_is_ok[T, E](res : Result[T, E]) -> Bool {
  match res {
    Ok(_) => true
    Err(_) => false
  }
}

fn result_is_err[T, E](res : Result[T, E]) -> Bool {
  match res {
    Ok(_) => false
    Err(_) => true
  }
}

fn result_unwrap_or[T, E](res : Result[T, E], default : T) -> T {
  match res {
    Ok(value) => value
    Err(_) => default
  }
}

fn result_map[T, E, U](res : Result[T, E], f : (T) -> U) -> Result[U, E] {
  match res {
    Ok(value) => Ok(f(value))
    Err(error) => Err(error)
  }
}

// 测试3: 除零错误处理
test "除零错误处理测试" {
  assert_eq(safe_divide(10, 2), Ok(5))
  assert_eq(safe_divide(10, 5), Ok(2))
  assert_eq(safe_divide(10, 1), Ok(10))
  
  let result1 = safe_divide(10, 0)
  assert_eq(result_is_err(result1), true)
  
  let result2 = safe_divide(0, 5)
  assert_eq(result2, Ok(0))
}

fn safe_divide(a : Int, b : Int) -> Result[Int, String] {
  if (b == 0) {
    Err("Division by zero")
  } else {
    Ok(a / b)
  }
}

// 测试4: 数组边界检查
test "数组边界检查测试" {
  let arr = [10, 20, 30, 40, 50]
  
  assert_eq(safe_array_get(arr, 0), Ok(10))
  assert_eq(safe_array_get(arr, 2), Ok(30))
  assert_eq(safe_array_get(arr, 4), Ok(50))
  
  let result1 = safe_array_get(arr, -1)
  assert_eq(result_is_err(result1), true)
  
  let result2 = safe_array_get(arr, 5)
  assert_eq(result_is_err(result2), true)
  
  let result3 = safe_array_get(arr, 10)
  assert_eq(result_is_err(result3), true)
  
  let empty_array = []
  let result4 = safe_array_get(empty_array, 0)
  assert_eq(result_is_err(result4), true)
}

fn safe_array_get[T](arr : Array[T], index : Int) -> Result[T, String] {
  if (index < 0 or index >= arr.length()) {
    Err("Index out of bounds")
  } else {
    Ok(arr[index])
  }
}

// 测试5: 字符串解析错误处理
test "字符串解析错误处理测试" {
  assert_eq(parse_int("123"), Ok(123))
  assert_eq(parse_int("0"), Ok(0))
  assert_eq(parse_int("-42"), Ok(-42))
  
  let result1 = parse_int("")
  assert_eq(result_is_err(result1), true)
  
  let result2 = parse_int("abc")
  assert_eq(result_is_err(result2), true)
  
  let result3 = parse_int("12a3")
  assert_eq(result_is_err(result3), true)
  
  let result4 = parse_int("123.45")
  assert_eq(result_is_err(result4), true)
}

fn parse_int(s : String) -> Result[Int, String] {
  if (s.length() == 0) {
    return Err("Empty string")
  }
  
  let is_negative = s[0] == '-'
  let start = if (is_negative) { 1 } else { 0 }
  let result = 0
  
  for i = start; i < s.length(); i = i + 1 {
    let char = s[i]
    if (char < '0' or char > '9') {
      return Err("Invalid character")
    }
    result = result * 10 + (char - '0')
  }
  
  if (is_negative) {
    Ok(-result)
  } else {
    Ok(result)
  }
}

// 测试6: 链式错误处理
test "链式错误处理测试" {
  let result1 = chain_operations(10, 2, 5)
  assert_eq(result1, Ok(1))  // (10 / 2) - 5 = 0
  
  let result2 = chain_operations(10, 0, 5)
  assert_eq(result_is_err(result2), true)  // 除零错误
  
  let result3 = chain_operations(10, 3, 15)
  assert_eq(result3, Ok(-12))  // (10 / 3) - 15 = -12 (整数除法)
}

fn chain_operations(a : Int, b : Int, c : Int) -> Result[Int, String] {
  let div_result = safe_divide(a, b)
  
  match div_result {
    Err(e) => Err(e)
    Ok(value) => {
      let result = value - c
      Ok(result)
    }
  }
}

// 测试7: 错误恢复机制
test "错误恢复机制测试" {
  assert_eq(recover_from_error(safe_divide(10, 2), 0), 5)
  assert_eq(recover_from_error(safe_divide(10, 0), 0), 0)  // 默认值
  assert_eq(recover_from_error(safe_divide(10, 0), 999), 999)  // 自定义默认值
  
  assert_eq(recover_from_error(parse_int("123"), -1), 123)
  assert_eq(recover_from_error(parse_int("abc"), -1), -1)  // 默认值
}

fn recover_from_error[T](res : Result[T, String], default : T) -> T {
  match res {
    Ok(value) => value
    Err(_) => default
  }
}

// 测试8: 错误组合处理
test "错误组合处理测试" {
  let results = [
    safe_divide(10, 2),
    safe_divide(20, 5),
    safe_divide(30, 0),  // 这个会失败
    safe_divide(40, 8)
  ]
  
  let (successes, failures) = partition_results(results)
  assert_eq(successes.length(), 3)
  assert_eq(failures.length(), 1)
  assert_eq(successes, [5, 4, 5])
}

fn partition_results[T](results : Array[Result[T, String]]) -> (Array[T], Array[String]) {
  let successes = []
  let failures = []
  
  for i = 0; i < results.length(); i = i + 1 {
    match results[i] {
      Ok(value) => successes.push(value)
      Err(error) => failures.push(error)
    }
  }
  
  (successes, failures)
}