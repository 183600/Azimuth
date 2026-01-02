// Azimuth Basic Language Test Suite
// 基本语言测试套件 - 包含基本的MoonBit语言功能测试

// Test 1: 数值操作
test "numeric operations" {
  // 整数运算
  assert_eq(2 + 3, 5)
  assert_eq(10 - 4, 6)
  assert_eq(3 * 4, 12)
  assert_eq(15 / 3, 5)
  assert_eq(15 % 4, 3)
  
  // 负数运算
  assert_eq(-5 + 3, -2)
  assert_eq(-10 - (-5), -5)
  assert_eq(-3 * 4, -12)
  assert_eq(-20 / 4, -5)
  
  // 边界条件
  assert_eq(0 + 0, 0)
  assert_eq(0 * 5, 0)
  assert_eq(5 * 0, 0)
}

// Test 2: 字符串操作
test "string operations" {
  let hello = "hello"
  let world = "world"
  
  // 字符串连接
  assert_eq(hello + " " + world, "hello world")
  
  // 字符串长度
  assert_eq(hello.length(), 5)
  assert_eq(world.length(), 5)
  assert_eq("".length(), 0)
  
  // 字符串比较
  assert_true(hello == "hello")
  assert_false(hello == "world")
  
  // 字符串包含检查
  assert_true("hello world".contains("hello"))
  assert_true("hello world".contains("world"))
  assert_false("hello world".contains("test"))
}

// Test 3: 布尔操作
test "boolean operations" {
  // 基本布尔值
  assert_true(true)
  assert_false(false)
  
  // 逻辑与
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && true)
  assert_false(false && false)
  
  // 逻辑或
  assert_true(true || true)
  assert_true(true || false)
  assert_true(false || true)
  assert_false(false || false)
  
  // 逻辑非
  assert_false(!true)
  assert_true(!false)
}

// Test 4: 数组操作
test "array operations" {
  // 创建数组
  let numbers = [1, 2, 3, 4, 5]
  let empty = []
  
  // 数组长度
  assert_eq(numbers.length(), 5)
  assert_eq(empty.length(), 0)
  
  // 数组访问
  assert_eq(numbers[0], 1)
  assert_eq(numbers[2], 3)
  assert_eq(numbers[4], 5)
  
  // 数组包含
  assert_true(numbers.contains(3))
  assert_false(numbers.contains(6))
  
  // 数组映射
  let doubled = numbers.map(fn(x) { x * 2 })
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  // 数组过滤
  let evens = numbers.filter(fn(x) { x % 2 == 0 })
  assert_eq(evens, [2, 4])
  
  // 数组求和
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(sum, 15)
}

// Test 5: Option类型操作
test "option type operations" {
  let some_value = Some(42)
  let none_value = None
  
  // 解构Some值
  match some_value {
    Some(v) => assert_eq(v, 42)
    None => assert_true(false)
  }
  
  // 解构None值
  match none_value {
    Some(v) => assert_true(false)
    None => assert_true(true)
  }
  
  // Option映射
  let doubled = some_value.map(fn(x) { x * 2 })
  match doubled {
    Some(v) => assert_eq(v, 84)
    None => assert_true(false)
  }
  
  let none_doubled = none_value.map(fn(x) { x * 2 })
  match none_doubled {
    Some(v) => assert_true(false)
    None => assert_true(true)
  }
  
  // Option扁平化
  let nested = Some(Some(42))
  let flattened = nested.flatten()
  match flattened {
    Some(v) => assert_eq(v, 42)
    None => assert_true(false)
  }
}

// Test 6: Result类型操作
test "result type operations" {
  let success = Ok(42)
  let failure = Error("Something went wrong")
  
  // 解构Ok值
  match success {
    Ok(v) => assert_eq(v, 42)
    Error(e) => assert_true(false)
  }
  
  // 解构Error值
  match failure {
    Ok(v) => assert_true(false)
    Error(e) => assert_eq(e, "Something went wrong")
  }
  
  // Result映射
  let mapped_success = success.map(fn(x) { x * 2 })
  match mapped_success {
    Ok(v) => assert_eq(v, 84)
    Error(e) => assert_true(false)
  }
  
  let mapped_failure = failure.map(fn(x) { x * 2 })
  match mapped_failure {
    Ok(v) => assert_true(false)
    Error(e) => assert_eq(e, "Something went wrong")
  }
}

// Test 7: 元组操作
test "tuple operations" {
  // 创建元组
  let pair = (1, "one")
  let triple = (1, "one", true)
  
  // 元组访问
  assert_eq(pair.0, 1)
  assert_eq(pair.1, "one")
  
  assert_eq(triple.0, 1)
  assert_eq(triple.1, "one")
  assert_eq(triple.2, true)
  
  // 元组比较
  assert_eq(pair, (1, "one"))
  assert_false(pair == (2, "two"))
  
  // 元组解构
  let (a, b) = pair
  assert_eq(a, 1)
  assert_eq(b, "one")
  
  let (x, y, z) = triple
  assert_eq(x, 1)
  assert_eq(y, "one")
  assert_eq(z, true)
}

// Test 8: 函数操作
test "function operations" {
  // 简单函数
  let add = fn(a, b) { a + b }
  assert_eq(add(2, 3), 5)
  
  // 高阶函数
  let apply_twice = fn(f, x) { f(f(x)) }
  let increment = fn(x) { x + 1 }
  assert_eq(apply_twice(increment, 5), 7)
  
  // 函数组合
  let compose = fn(f, g) { fn(x) { f(g(x)) } }
  let double = fn(x) { x * 2 }
  let add_one = fn(x) { x + 1 }
  let double_then_add = compose(add_one, double)
  assert_eq(double_then_add(5), 11)
}

// Test 9: 条件表达式
test "conditional expressions" {
  // if-then-else
  let x = 5
  let result = if x > 0 { "positive" } else { "non-positive" }
  assert_eq(result, "positive")
  
  let y = -3
  let result2 = if y > 0 { "positive" } else { "non-positive" }
  assert_eq(result2, "non-positive")
  
  // 嵌套条件
  let z = 0
  let result3 = if z > 0 { "positive" } else if z < 0 { "negative" } else { "zero" }
  assert_eq(result3, "zero")
}

// Test 10: 类型转换
test "type conversions" {
  // 字符串到整数
  let str_num = "42"
  let int_from_str = str_num.to_int()
  match int_from_str {
    Some(n) => assert_eq(n, 42)
    None => assert_true(false)
  }
  
  // 整数到字符串
  let int_num = 42
  let str_from_int = int_num.to_string()
  assert_eq(str_from_int, "42")
  
  // 字符串到浮点数
  let str_float = "3.14"
  let float_from_str = str_float.to_float()
  match float_from_str {
    Some(f) => assert_eq(f, 3.14)
    None => assert_true(false)
  }
  
  // 浮点数到字符串
  let float_num = 3.14
  let str_from_float = float_num.to_string()
  assert_eq(str_from_float, "3.14")
}