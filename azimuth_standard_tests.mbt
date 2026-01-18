// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法

// 测试加法功能
test "add_function_basic" {
  let result1 = add(2, 3)
  let result2 = add(0, 0)
  let result3 = add(-5, 10)
  let result4 = add(100, 200)
  
  // 验证结果
  assert_eq(result1, 5)
  assert_eq(result2, 0)
  assert_eq(result3, 5)
  assert_eq(result4, 300)
}

// 测试乘法功能
test "multiply_function_basic" {
  let result1 = multiply(3, 4)
  let result2 = multiply(0, 5)
  let result3 = multiply(-2, 6)
  let result4 = multiply(10, 10)
  
  // 验证结果
  assert_eq(result1, 12)
  assert_eq(result2, 0)
  assert_eq(result3, -12)
  assert_eq(result4, 100)
}

// 测试问候语功能
test "greet_function_basic" {
  let result1 = greet("World")
  let result2 = greet("MoonBit")
  let result3 = greet("")
  let result4 = greet("用户")
  
  // 验证结果
  assert_eq(result1, "Hello, World!")
  assert_eq(result2, "Hello, MoonBit!")
  assert_eq(result3, "Hello, !")
  assert_eq(result4, "Hello, 用户!")
}

// 测试边界条件
test "boundary_conditions" {
  // 测试大数加法
  let large_add = add(1000000, 2000000)
  assert_eq(large_add, 3000000)
  
  // 测试负数乘法
  let negative_multiply = multiply(-5, -5)
  assert_eq(negative_multiply, 25)
  
  // 测试混合运算
  let mixed = add(multiply(2, 3), multiply(4, 5))
  assert_eq(mixed, 26)
}

// 测试组合功能
test "combined_operations" {
  // 复杂计算测试
  let a = 10
  let b = 20
  let c = 30
  
  let result1 = add(multiply(a, b), c)
  let result2 = multiply(add(a, b), c)
  let result3 = add(add(a, b), add(c, a))
  
  assert_eq(result1, 230)  // 10*20 + 30
  assert_eq(result2, 900)  // (10+20)*30
  assert_eq(result3, 70)   // 10+20+30+10
}

// 测试字符串处理
test "string_processing" {
  let name1 = "Alice"
  let name2 = "Bob"
  
  let greeting1 = greet(name1)
  let greeting2 = greet(name2)
  
  // 验证字符串长度
  assert_eq(String.length(greeting1), 13)  // "Hello, Alice!"
  assert_eq(String.length(greeting2), 11)   // "Hello, Bob!"
  
  // 验证字符串包含
  assert_true(String.contains(greeting1, "Hello"))
  assert_true(String.contains(greeting1, name1))
}

// 测试错误处理场景
test "error_scenarios" {
  // 测试零值运算
  let zero_add = add(0, 100)
  let zero_multiply = multiply(0, 100)
  
  assert_eq(zero_add, 100)
  assert_eq(zero_multiply, 0)
  
  // 测试单值运算
  let single_add = add(42, 0)
  let single_multiply = multiply(42, 1)
  
  assert_eq(single_add, 42)
  assert_eq(single_multiply, 42)
}

// 测试特殊字符处理
test "special_characters" {
  let special_names = [
    "张三", "李四", "John-Doe", "O'Connor", "user@domain.com"
  ]
  
  for name in special_names {
    let greeting = greet(name)
    assert_true(String.length(greeting) > String.length(name))
    assert_true(String.contains(greeting, "Hello"))
    assert_true(String.contains(greeting, name))
  }
}

// 测试性能相关场景
test "performance_scenarios" {
  // 批量计算测试
  let numbers = [1, 2, 3, 4, 5]
  let mut sum = 0
  let mut product = 1
  
  for num in numbers {
    sum = add(sum, num)
    product = multiply(product, num)
  }
  
  assert_eq(sum, 15)      // 1+2+3+4+5
  assert_eq(product, 120) // 1*2*3*4*5
}