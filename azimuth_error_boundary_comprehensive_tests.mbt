// Error Boundary Comprehensive Test Suite for Azimuth
// 错误边界综合测试套件 - 测试各种错误处理和边界情况

// Test 1: 数值边界测试
test "numeric boundary tests" {
  // 整数边界
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 边界加法
  assert_eq(max_int - 1, 2147483646)
  assert_eq(min_int + 1, -2147483647)
  
  // 边界乘法
  assert_eq(max_int * 0, 0)
  assert_eq(min_int * 0, 0)
  
  // 边界除法
  assert_eq(max_int / max_int, 1)
  assert_eq(min_int / min_int, 1)
  
  // 零边界
  assert_eq(0 + 0, 0)
  assert_eq(0 * 0, 0)
  assert_eq(0 - 0, 0)
  
  // 除零安全处理
  let safe_divide = fn(a, b) {
    if b == 0 {
      None
    } else {
      Some(a / b)
    }
  }
  
  assert_eq(safe_divide(10, 2), Some(5))
  assert_eq(safe_divide(10, 0), None)
}

// Test 2: 数组边界测试
test "array boundary tests" {
  // 空数组操作
  let empty_arr = []
  assert_eq(empty_arr.length(), 0)
  assert_true(empty_arr.is_empty())
  
  // 安全数组访问
  let safe_get = fn(arr, index) {
    if index >= 0 && index < arr.length() {
      Some(arr[index])
    } else {
      None
    }
  }
  
  let arr = [10, 20, 30, 40, 50]
  assert_eq(safe_get(arr, 0), Some(10))
  assert_eq(safe_get(arr, 4), Some(50))
  assert_eq(safe_get(arr, 5), None)
  assert_eq(safe_get(arr, -1), None)
  
  // 数组切片边界
  let safe_slice = fn(arr, start, end) {
    let len = arr.length()
    let safe_start = if start < 0 { 0 } else if start > len { len } else { start }
    let safe_end = if end < 0 { 0 } else if end > len { len } else { end }
    
    if safe_start > safe_end {
      []
    } else {
      arr.slice(safe_start, safe_end)
    }
  }
  
  assert_eq(safe_slice(arr, 1, 3), [20, 30])
  assert_eq(safe_slice(arr, -2, 10), [10, 20, 30, 40, 50])
  assert_eq(safe_slice(arr, 3, 1), [])
}

// Test 3: 字符串边界测试
test "string boundary tests" {
  // 空字符串操作
  let empty_str = ""
  assert_eq(empty_str.length(), 0)
  assert_true(empty_str.is_empty())
  
  // 字符串索引边界
  let safe_char_at = fn(str, index) {
    if index >= 0 && index < str.length() {
      Some(str[index])
    } else {
      None
    }
  }
  
  let test_str = "hello"
  assert_eq(safe_char_at(test_str, 0), Some('h'))
  assert_eq(safe_char_at(test_str, 4), Some('o'))
  assert_eq(safe_char_at(test_str, 5), None)
  assert_eq(safe_char_at(test_str, -1), None)
  
  // 字符串切片边界
  let safe_substring = fn(str, start, length) {
    let str_len = str.length()
    if start < 0 || start >= str_len || length <= 0 {
      ""
    } else {
      let end = if start + length > str_len { str_len } else { start + length }
      str.substring(start, end)
    }
  }
  
  assert_eq(safe_substring(test_str, 1, 3), "ell")
  assert_eq(safe_substring(test_str, 2, 10), "llo")
  assert_eq(safe_substring(test_str, 5, 2), "")
  assert_eq(safe_substring(test_str, -1, 2), "")
}

// Test 4: Option类型边界测试
test "option type boundary tests" {
  // None值处理
  let none_value = None
  
  // 安全解包
  let unwrap_or = fn(opt, default) {
    match opt {
      Some(v) => v
      None => default
    }
  }
  
  assert_eq(unwrap_or(Some(42), 0), 42)
  assert_eq(unwrap_or(None, 0), 0)
  
  // Option链式操作
  let chain_operations = fn(opt) {
    opt
      .map(fn(x) { x * 2 })
      .filter(fn(x) { x > 10 })
      .map(fn(x) { x + 5 })
  }
  
  assert_eq(chain_operations(Some(10)), Some(25))
  assert_eq(chain_operations(Some(5)), None)
  assert_eq(chain_operations(None), None)
  
  // Option组合
  let combine_options = fn(opt1, opt2) {
    match (opt1, opt2) {
      (Some(a), Some(b)) => Some((a, b))
      _ => None
    }
  }
  
  assert_eq(combine_options(Some(1), Some(2)), Some((1, 2)))
  assert_eq(combine_options(Some(1), None), None)
  assert_eq(combine_options(None, Some(2)), None)
  assert_eq(combine_options(None, None), None)
}

// Test 5: Result类型边界测试
test "result type boundary tests" {
  // 错误处理
  let safe_parse_int = fn(str) {
    if str == "" {
      Error("Empty string")
    } else if str.contains(".") {
      Error("Not an integer")
    } else {
      match str.to_int() {
        Some(n) => Ok(n)
        None => Error("Invalid number format")
      }
    }
  }
  
  assert_eq(safe_parse_int("42"), Ok(42))
  assert_eq(safe_parse_int(""), Error("Empty string"))
  assert_eq(safe_parse_int("3.14"), Error("Not an integer"))
  assert_eq(safe_parse_int("abc"), Error("Invalid number format"))
  
  // Result链式操作
  let chain_result_operations = fn(result) {
    result
      .map(fn(x) { x * 2 })
      .map_err(fn(e) { e + " (after mapping)" })
  }
  
  assert_eq(chain_result_operations(Ok(5)), Ok(10))
  assert_eq(chain_result_operations(Error("test")), Error("test (after mapping)"))
  
  // Result恢复
  let recover = fn(result, fallback) {
    match result {
      Ok(v) => v
      Error(e) => fallback(e)
    }
  }
  
  assert_eq(recover(Ok(10), fn(_) { 0 }), 10)
  assert_eq(recover(Error("error"), fn(_) { 0 }), 0)
}

// Test 6: 哈希表边界测试
test "hash map boundary tests" {
  // 空哈希表操作
  let empty_map = {}
  assert_eq(empty_map.size(), 0)
  assert_true(empty_map.is_empty())
  
  // 键边界检查
  let safe_get = fn(map, key) {
    if key == "" {
      None
    } else {
      map.get(key)
    }
  }
  
  let map = {"a": 1, "b": 2, "c": 3}
  assert_eq(safe_get(map, "a"), Some(1))
  assert_eq(safe_get(map, "d"), None)
  assert_eq(safe_get(map, ""), None)
  
  // 批量操作边界
  let batch_get = fn(map, keys) {
    keys.map(fn(key) { (key, map.get(key)) })
  }
  
  let keys = ["a", "b", "x", "y"]
  let results = batch_get(map, keys)
  assert_eq(results, [("a", Some(1)), ("b", Some(2)), ("x", None), ("y", None)])
}

// Test 7: 集合边界测试
test "set boundary tests" {
  // 空集合操作
  let empty_set = {}
  assert_eq(empty_set.size(), 0)
  assert_true(empty_set.is_empty())
  
  // 集合操作边界
  let set1 = {1, 2, 3}
  let set2 = {3, 4, 5}
  let empty_set = {}
  
  // 空集合并集
  let union_with_empty = set1.union(empty_set)
  assert_eq(union_with_empty, set1)
  
  // 空集合交集
  let intersection_with_empty = set1.intersection(empty_set)
  assert_eq(intersection_with_empty.size(), 0)
  
  // 空集合差集
  let difference_from_empty = empty_set.difference(set1)
  assert_eq(difference_from_empty.size(), 0)
  
  let difference_to_empty = set1.difference(empty_set)
  assert_eq(difference_to_empty, set1)
}

// Test 8: 递归边界测试
test "recursion boundary tests" {
  // 安全递归（防止栈溢出）
  let safe_recursive_sum = fn(arr, index, max_depth) {
    if max_depth <= 0 || index >= arr.length() {
      0
    } else {
      arr[index] + safe_recursive_sum(arr, index + 1, max_depth - 1)
    }
  }
  
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  assert_eq(safe_recursive_sum(numbers, 0, 10), 55)
  assert_eq(safe_recursive_sum(numbers, 0, 5), 15) // 只计算前5个
  
  // 尾递归优化
  let tail_recursive_sum = fn(arr, acc, index) {
    if index >= arr.length() {
      acc
    } else {
      tail_recursive_sum(arr, acc + arr[index], index + 1)
    }
  }
  
  assert_eq(tail_recursive_sum(numbers, 0, 0), 55)
  
  // 相互递归边界
  let is_even = fn(n) {
    if n == 0 {
      true
    } else if n == 1 {
      false
    } else if n < 0 {
      is_even(-n)
    } else {
      is_odd(n - 1)
    }
  }
  
  let is_odd = fn(n) {
    if n == 0 {
      false
    } else if n == 1 {
      true
    } else if n < 0 {
      is_odd(-n)
    } else {
      is_even(n - 1)
    }
  }
  
  assert_true(is_even(10))
  assert_false(is_even(11))
  assert_true(is_odd(11))
  assert_false(is_odd(10))
}

// Test 9: 类型转换边界测试
test "type conversion boundary tests" {
  // 安全类型转换
  let safe_to_int = fn(value) {
    match value {
      Int(n) => Ok(n)
      Float(f) => {
        if f >= -2147483648.0 && f <= 2147483647.0 && f.floor() == f {
          Ok(f.to_int())
        } else {
          Error("Float out of integer range")
        }
      }
      String(s) => {
        match s.to_int() {
          Some(n) => Ok(n)
          None => Error("Invalid integer string")
        }
      }
      _ => Error("Unsupported type")
    }
  }
  
  assert_eq(safe_to_int(Int(42)), Ok(42))
  assert_eq(safe_to_int(Float(3.0)), Ok(3))
  assert_eq(safe_to_int(Float(3.14)), Error("Float out of integer range"))
  assert_eq(safe_to_int(String("123")), Ok(123))
  assert_eq(safe_to_int(String("abc")), Error("Invalid integer string"))
  
  // 边界值转换
  assert_eq(safe_to_int(Float(2147483647.0)), Ok(2147483647))
  assert_eq(safe_to_int(Float(-2147483648.0)), Ok(-2147483648))
  assert_eq(safe_to_int(Float(2147483648.0)), Error("Float out of integer range"))
}

// Test 10: 资源边界测试
test "resource boundary tests" {
  // 内存分配边界
  let create_large_array = fn(size) {
    if size <= 0 || size > 1000000 {
      None
    } else {
      let mut arr = []
      for i in 0..size {
        arr.push(i)
      }
      Some(arr)
    }
  }
  
  assert_eq(create_large_array(0), None)
  assert_eq(create_large_array(-1), None)
  assert_eq(create_large_array(1000001), None)
  
  let small_array = create_large_array(10)
  match small_array {
    Some(arr) => {
      assert_eq(arr.length(), 10)
      assert_eq(arr[0], 0)
      assert_eq(arr[9], 9)
    }
    None => assert_true(false)
  }
  
  // 文件操作边界
  let safe_file_read = fn(path, max_size) {
    if path == "" {
      Error("Empty file path")
    } else if max_size <= 0 {
      Error("Invalid max size")
    } else {
      // 模拟文件读取
      let content = "file content"
      if content.length() > max_size {
        Error("File too large")
      } else {
        Ok(content)
      }
    }
  }
  
  assert_eq(safe_file_read("", 100), Error("Empty file path"))
  assert_eq(safe_file_read("test.txt", 0), Error("Invalid max size"))
  assert_eq(safe_file_read("test.txt", 5), Error("File too large"))
  assert_eq(safe_file_read("test.txt", 100), Ok("file content"))
}