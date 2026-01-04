// Azimuth MoonBit Language Fundamentals Test Suite
// 测试MoonBit语言基础特性，确保语言核心功能正常工作

test "模式匹配高级测试" {
  // 测试元组模式匹配
  let point = (3, 4)
  let distance = match point {
    (0, 0) => 0.0
    (x, y) => (x * x + y * y).to_float().sqrt()
  }
  assert_true(distance > 4.0 && distance < 6.0)
  
  // 测试枚举模式匹配
  enum Color {
    Red
    Green
    Blue
    RGB(Int, Int, Int)
  }
  
  let color = Color::RGB(255, 128, 0)
  let is_bright = match color {
    Color::Red => true
    Color::Green => true
    Color::Blue => false
    Color::RGB(r, g, b) => r > 200 || g > 200 || b > 200
  }
  assert_true(is_bright)
  
  // 测试Option类型模式匹配
  let maybe_value = Some(42)
  let doubled = match maybe_value {
    Some(v) => Some(v * 2)
    None => None
  }
  
  match doubled {
    Some(v) => assert_eq(v, 84)
    None => assert_true(false)
  }
}

test "函数式编程特性测试" {
  // 测试高阶函数
  let apply_twice = fn(f: (Int) -> Int, x: Int) -> Int {
    f(f(x))
  }
  
  let add_one = fn(x: Int) -> Int { x + 1 }
  let result = apply_twice(add_one, 5)
  assert_eq(result, 7)
  
  // 测试闭包捕获
  let multiplier = 3
  let multiply_by = fn(x: Int) -> Int { x * multiplier }
  assert_eq(multiply_by(4), 12)
  
  // 测试函数组合
  let compose = fn(f: (Int) -> Int, g: (Int) -> Int) -> (Int) -> Int {
    fn(x: Int) -> Int { f(g(x)) }
  }
  
  let add_two = fn(x: Int) -> Int { x + 2 }
  let square = fn(x: Int) -> Int { x * x }
  let add_then_square = compose(square, add_two)
  assert_eq(add_then_square(3), 25) // (3 + 2)² = 25
}

test "类型系统高级特性测试" {
  // 测试类型别名
  type UserId = Int
  type UserName = String
  
  let user_id: UserId = 12345
  let user_name: UserName = "张三"
  
  assert_eq(user_id, 12345)
  assert_eq(user_name.length(), 2)
  
  // 测试泛型函数
  let identity = fn[T](x: T) -> T { x }
  
  let int_result = identity(42)
  let string_result = identity("hello")
  
  assert_eq(int_result, 42)
  assert_eq(string_result, "hello")
  
  // 测试Result类型
  type Result[T, E] = {
    Ok: T
    Error: E
  }
  
  let safe_divide = fn(a: Int, b: Int) -> Result[Int, String] {
    if b == 0 {
      Result::Error("除零错误")
    } else {
      Result::Ok(a / b)
    }
  }
  
  match safe_divide(10, 2) {
    Result::Ok(result) => assert_eq(result, 5)
    Result::Error(_) => assert_true(false)
  }
  
  match safe_divide(10, 0) {
    Result::Ok(_) => assert_true(false)
    Result::Error(msg) => assert_eq(msg, "除零错误")
  }
}

test "集合操作高级测试" {
  // 测试数组过滤
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let even_numbers = []
  for i in 0..numbers.length() {
    if numbers[i] % 2 == 0 {
      even_numbers.push(numbers[i])
    }
  }
  assert_eq(even_numbers.length(), 5)
  assert_eq(even_numbers[0], 2)
  assert_eq(even_numbers[4], 10)
  
  // 测试数组映射
  let doubled = []
  for i in 0..numbers.length() {
    doubled.push(numbers[i] * 2)
  }
  assert_eq(doubled[0], 2)
  assert_eq(doubled[9], 20)
  
  // 测试数组归约
  let mut sum = 0
  for i in 0..numbers.length() {
    sum = sum + numbers[i]
  }
  assert_eq(sum, 55)
  
  // 测试数组查找
  let mut found = false
  let mut index = -1
  for i in 0..numbers.length() {
    if numbers[i] == 7 {
      found = true
      index = i
      break
    }
  }
  assert_true(found)
  assert_eq(index, 6)
}

test "字符串处理高级测试" {
  // 测试字符串分割
  let text = "apple,banana,cherry,date"
  let parts = []
  let mut current = ""
  for i in 0..text.length() {
    if text[i] == ',' {
      parts.push(current)
      current = ""
    } else {
      current = current + text[i].to_string()
    }
  }
  parts.push(current) // 添加最后一部分
  
  assert_eq(parts.length(), 4)
  assert_eq(parts[0], "apple")
  assert_eq(parts[1], "banana")
  assert_eq(parts[2], "cherry")
  assert_eq(parts[3], "date")
  
  // 测试字符串反转
  let original = "hello"
  let mut reversed = ""
  for i in 0..original.length() {
    reversed = original[i].to_string() + reversed
  }
  assert_eq(reversed, "olleh")
  
  // 测试字符串计数
  let sentence = "hello world hello moonbit"
  let mut hello_count = 0
  let words = []
  let mut current_word = ""
  
  for i in 0..sentence.length() {
    if sentence[i] == ' ' {
      if current_word.length() > 0 {
        words.push(current_word)
        current_word = ""
      }
    } else {
      current_word = current_word + sentence[i].to_string()
    }
  }
  if current_word.length() > 0 {
    words.push(current_word)
  }
  
  for i in 0..words.length() {
    if words[i] == "hello" {
      hello_count = hello_count + 1
    }
  }
  assert_eq(hello_count, 2)
}

test "递归算法测试" {
  // 测试递归阶乘
  let factorial = fn(n: Int) -> Int {
    if n <= 1 {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(5), 120)
  assert_eq(factorial(7), 5040)
  
  // 测试递归斐波那契
  let fibonacci = fn(n: Int) -> Int {
    if n <= 1 {
      n
    } else {
      fibonacci(n - 1) + fibonacci(n - 2)
    }
  }
  
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(5), 5)
  assert_eq(fibonacci(7), 13)
  
  // 测试递归最大公约数
  let gcd = fn(a: Int, b: Int) -> Int {
    if b == 0 {
      a
    } else {
      gcd(b, a % b)
    }
  }
  
  assert_eq(gcd(48, 18), 6)
  assert_eq(gcd(17, 13), 1)
  assert_eq(gcd(100, 25), 25)
}

test "数值算法测试" {
  // 测试素数判断
  let is_prime = fn(n: Int) -> Bool {
    if n <= 1 {
      false
    } else if n == 2 {
      true
    } else if n % 2 == 0 {
      false
    } else {
      let mut i = 3
      let mut prime = true
      while i * i <= n {
        if n % i == 0 {
          prime = false
          break
        }
        i = i + 2
      }
      prime
    }
  }
  
  assert_false(is_prime(0))
  assert_false(is_prime(1))
  assert_true(is_prime(2))
  assert_true(is_prime(3))
  assert_false(is_prime(4))
  assert_true(is_prime(5))
  assert_false(is_prime(9))
  assert_true(is_prime(11))
  assert_true(is_prime(13))
  assert_false(is_prime(15))
  
  // 测试最大值查找
  let numbers = [3, 7, 2, 9, 4, 1, 8, 5, 6]
  let mut max = numbers[0]
  for i in 1..numbers.length() {
    if numbers[i] > max {
      max = numbers[i]
    }
  }
  assert_eq(max, 9)
  
  // 测试最小值查找
  let mut min = numbers[0]
  for i in 1..numbers.length() {
    if numbers[i] < min {
      min = numbers[i]
    }
  }
  assert_eq(min, 1)
}

test "位操作测试" {
  // 测试基本位操作
  let a = 5  // 二进制: 101
  let b = 3  // 二进制: 011
  
  // 位与
  let and_result = a & b  // 101 & 011 = 001 = 1
  assert_eq(and_result, 1)
  
  // 位或
  let or_result = a | b   // 101 | 011 = 111 = 7
  assert_eq(or_result, 7)
  
  // 位异或
  let xor_result = a ^ b  // 101 ^ 011 = 110 = 6
  assert_eq(xor_result, 6)
  
  // 位取反
  let not_result = ~a     // ~101 = ...11111010 (补码表示)
  assert_eq(not_result, -6)
  
  // 左移
  let left_shift = a << 2  // 101 << 2 = 10100 = 20
  assert_eq(left_shift, 20)
  
  // 右移
  let right_shift = a >> 1 // 101 >> 1 = 10 = 2
  assert_eq(right_shift, 2)
  
  // 测试位操作应用
  let is_power_of_two = fn(n: Int) -> Bool {
    n > 0 && (n & (n - 1)) == 0
  }
  
  assert_true(is_power_of_two(1))
  assert_true(is_power_of_two(2))
  assert_true(is_power_of_two(4))
  assert_true(is_power_of_two(8))
  assert_true(is_power_of_two(16))
  assert_false(is_power_of_two(0))
  assert_false(is_power_of_two(3))
  assert_false(is_power_of_two(5))
  assert_false(is_power_of_two(12))
}

test "内存管理测试" {
  // 测试大型数组创建和操作
  let large_array = []
  for i in 0..10000 {
    large_array.push(i)
  }
  assert_eq(large_array.length(), 10000)
  assert_eq(large_array[0], 0)
  assert_eq(large_array[9999], 9999)
  
  // 测试数组切片
  let slice_start = 100
  let slice_end = 110
  let slice = []
  for i in slice_start..slice_end {
    slice.push(large_array[i])
  }
  assert_eq(slice.length(), 10)
  assert_eq(slice[0], 100)
  assert_eq(slice[9], 109)
  
  // 测试内存释放（通过重新赋值）
  let large_array = []  // 释放之前的内存
  assert_eq(large_array.length(), 0)
  
  // 测试深层嵌套结构
  type NestedStruct = {
    value: Int
    nested: NestedStruct?
  }
  
  let create_nested = fn(depth: Int) -> NestedStruct {
    if depth == 0 {
      { value: depth, nested: None }
    } else {
      { value: depth, nested: Some(create_nested(depth - 1)) }
    }
  }
  
  let nested = create_nested(100)
  assert_eq(nested.value, 100)
  
  // 访问嵌套结构
  let mut current = nested
  let mut depth_count = 0
  while true {
    match current.nested {
      Some(next) => {
        depth_count = depth_count + 1
        current = next
      }
      None => break
    }
  }
  assert_eq(depth_count, 100)
}