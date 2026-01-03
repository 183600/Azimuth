// Azimuth 简化功能测试套件
// 包含10个简化的测试用例，使用MoonBit基本语法

// 测试1: 字符串操作和验证
test "字符串操作和验证" {
  let str = "azimuth-telemetry"
  assert_eq(str.length(), 17)
  assert_eq(str.to_uppercase(), "AZIMUTH-TELEMETRY")
  assert_eq(str.to_lowercase(), "azimuth-telemetry")
  assert_true(str.contains("telemetry"))
  assert_false(str.contains("metrics"))
  assert_eq(str.substring(0, 7), "azimuth")
  
  // 测试字符串拼接
  let prefix = "azimuth"
  let suffix = "telemetry"
  let combined = prefix + "-" + suffix
  assert_eq(combined, "azimuth-telemetry")
  
  // 测试字符串分割
  let parts = combined.split("-")
  assert_eq(parts.length(), 2)
  assert_eq(parts[0], "azimuth")
  assert_eq(parts[1], "telemetry")
}

// 测试2: 数组操作和转换
test "数组操作和转换" {
  let numbers = [1, 2, 3, 4, 5]
  let doubled = numbers.map(fn(x) { x * 2 })
  let evens = numbers.filter(fn(x) { x % 2 == 0 })
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  
  assert_eq(doubled, [2, 4, 6, 8, 10])
  assert_eq(evens, [2, 4])
  assert_eq(sum, 15)
  assert_eq(numbers.length(), 5)
  assert_true(numbers.contains(3))
  assert_false(numbers.contains(10))
  
  // 测试数组连接
  let first = [1, 2, 3]
  let second = [4, 5, 6]
  let combined = first.concat(second)
  assert_eq(combined, [1, 2, 3, 4, 5, 6])
  
  // 测试数组切片
  let slice = combined.slice(2, 5)
  assert_eq(slice, [3, 4, 5])
}

// 测试3: Option类型处理
test "Option类型处理" {
  let some_value = Some(42)
  let none_value = None
  
  match some_value {
    Some(v) => assert_eq(v, 42)
    None => assert_true(false)
  }
  
  match none_value {
    Some(v) => assert_true(false)
    None => assert_true(true)
  }
  
  let doubled = some_value.map(fn(x) { x * 2 })
  match doubled {
    Some(v) => assert_eq(v, 84)
    None => assert_true(false)
  }
  
  // 测试Option的链式操作
  let result = some_value
    .map(fn(x) { x + 10 })
    .filter(fn(x) { x > 50 })
    .map(fn(x) { x / 2 })
  
  match result {
    Some(v) => assert_eq(v, 26)
    None => assert_true(false)
  }
}

// 测试4: Result类型操作
test "Result类型操作" {
  let success = Ok("success")
  let failure = Err("error")
  
  match success {
    Ok(value) => assert_eq(value, "success")
    Err(msg) => assert_true(false)
  }
  
  match failure {
    Ok(value) => assert_true(false)
    Err(msg) => assert_eq(msg, "error")
  }
  
  let mapped = success.map(fn(s) { s.length() })
  match mapped {
    Ok(len) => assert_eq(len, 7)
    Err(_) => assert_true(false)
  }
  
  // 测试Result的链式操作
  let chained = success
    .map(fn(s) { s.to_uppercase() })
    .map(fn(s) { s + "_PROCESSED" })
  
  match chained {
    Ok(value) => assert_eq(value, "SUCCESS_PROCESSED")
    Err(_) => assert_true(false)
  }
}

// 测试5: 数值操作和比较
test "数值操作和比较" {
  let int_val = 42
  let float_val = 3.14
  
  assert_eq(int_val + 8, 50)
  assert_eq(int_val * 2, 84)
  assert_eq(int_val / 2, 21)
  assert_eq(int_val % 5, 2)
  
  assert_true(int_val > 40)
  assert_true(int_val >= 42)
  assert_false(int_val < 40)
  assert_true(int_val <= 42)
  
  assert_eq(float_val + 1.0, 4.14)
  assert_eq(float_val * 2.0, 6.28)
  assert_true(float_val > 3.0)
  assert_true(float_val < 4.0)
  
  // 测试数值转换
  let int_to_float = int_val.to_float()
  assert_eq(int_to_float, 42.0)
  
  let float_to_int = float_val.to_int()
  assert_eq(float_to_int, 3)
}

// 测试6: 布尔逻辑操作
test "布尔逻辑操作" {
  assert_true(true && true)
  assert_false(true && false)
  assert_false(false && false)
  
  assert_true(true || true)
  assert_true(true || false)
  assert_false(false || false)
  
  assert_false(!true)
  assert_true(!false)
  
  let x = 10
  let y = 20
  assert_true(x < y && x > 5)
  assert_true(x > 5 || y < 15)
  
  // 测试布尔值转换
  let bool_from_int = (1 == 1)
  assert_true(bool_from_int)
  
  let bool_from_string = ("hello" == "world")
  assert_false(bool_from_string)
}

// 测试7: 元组操作
test "元组操作" {
  let pair = (42, "answer")
  assert_eq(pair.0, 42)
  assert_eq(pair.1, "answer")
  
  let triple = (1, 2, 3)
  assert_eq(triple.0 + triple.1 + triple.2, 6)
  
  let swapped = (pair.1, pair.0)
  assert_eq(swapped.0, "answer")
  assert_eq(swapped.1, 42)
  
  // 测试元组解构
  let (a, b) = pair
  assert_eq(a, 42)
  assert_eq(b, "answer")
  
  let (x, y, z) = triple
  assert_eq(x + y + z, 6)
}

// 测试8: 枚举和模式匹配
test "枚举和模式匹配" {
  enum Color {
    Red
    Green
    Blue
    RGB(Int, Int, Int)
  }
  
  let red = Color::Red
  let green = Color::Green
  let blue = Color::Blue
  let custom = Color::RGB(255, 128, 0)
  
  let red_name = match red {
    Color::Red => "red"
    Color::Green => "green"
    Color::Blue => "blue"
    Color::RGB(r, g, b) => "custom"
  }
  assert_eq(red_name, "red")
  
  let custom_sum = match custom {
    Color::RGB(r, g, b) => r + g + b
    _ => 0
  }
  assert_eq(custom_sum, 383)
  
  // 测试嵌套模式匹配
  let colors = [red, green, blue, custom]
  let primary_colors = colors.filter(fn(color) {
    match color {
      Color::Red => true
      Color::Green => true
      Color::Blue => true
      Color::RGB(_, _, _) => false
    }
  })
  
  assert_eq(primary_colors.length(), 3)
}

// 测试9: 循环和迭代
test "循环和迭代" {
  let mut sum = 0
  for i in 0..=10 {
    sum = sum + i
  }
  assert_eq(sum, 55)
  
  let mut product = 1
  for i in 1..=5 {
    product = product * i
  }
  assert_eq(product, 120)
  
  let squares = []
  for i in 1..=5 {
    squares = squares.push(i * i)
  }
  assert_eq(squares, [1, 4, 9, 16, 25])
  
  // 测试while循环
  let mut count = 0
  let mut result = []
  while count < 5 {
    result = result.push(count * 2)
    count = count + 1
  }
  assert_eq(result, [0, 2, 4, 6, 8])
}

// 测试10: 函数组合和高阶函数
test "函数组合和高阶函数" {
  let add_one = fn(x) { x + 1 }
  let double = fn(x) { x * 2 }
  let square = fn(x) { x * x }
  
  let compose = fn(f, g) { fn(x) { f(g(x)) } }
  
  let add_then_double = compose(double, add_one)
  let double_then_square = compose(square, double)
  
  assert_eq(add_then_double(5), 12)
  assert_eq(double_then_square(3), 36)
  
  let numbers = [1, 2, 3, 4, 5]
  let transformed = numbers
    .map(add_one)
    .filter(fn(x) { x % 2 == 0 })
    .map(double)
  
  assert_eq(transformed, [4, 8, 12])
  
  // 测试函数作为参数
  let apply_twice = fn(f, x) { f(f(x)) }
  let result = apply_twice(add_one, 10)
  assert_eq(result, 12)
  
  // 测试柯里化
  let curried_add = fn(a) { fn(b) { a + b } }
  let add_five = curried_add(5)
  assert_eq(add_five(10), 15)
}