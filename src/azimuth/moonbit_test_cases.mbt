// MoonBit 测试用例
// 覆盖核心功能的各种场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-10, 10))
  assert_eq(-20, add(-15, -5))
  assert_eq(100, add(99, 1))
}

test "multiplication_edge_cases" {
  // 测试乘法的边界情况
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(-12345, multiply(-1, 12345))
  assert_eq(25, multiply(5, 5))
}

test "greet_function_special_chars" {
  // 测试 greet 函数处理特殊字符
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 空格!", greet("空格"))
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  let a = 10
  let b = 20
  let c = 30
  
  // ((a + b) * c) - (a * b)
  let sum_ab = add(a, b)
  let product_abc = multiply(sum_ab, c)
  let product_ab = multiply(a, b)
  let result = add(product_abc, -product_ab)
  
  assert_eq(700, result)
}

test "overflow_protection" {
  // 测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出时返回边界值
  assert_eq(max_val, add(max_val, 100))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -2))
}

test "string_concatenation_various" {
  // 测试各种字符串连接情况
  let single_char = "A"
  let empty_string = ""
  let mixed_chars = "AbC123"
  
  assert_eq_string("Hello, A!", greet(single_char))
  assert_eq_string("Hello, !", greet(empty_string))
  assert_eq_string("Hello, AbC123!", greet(mixed_chars))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 5)), add(multiply(x, y), multiply(x, 5)))
}

test "boundary_value_combinations" {
  // 测试边界值的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
}

test "real_world_shopping_cart" {
  // 测试实际应用：购物车计算
  let apple_price = 5
  let apple_quantity = 3
  let banana_price = 3
  let banana_quantity = 5
  
  // 计算总价
  let apple_total = multiply(apple_price, apple_quantity)
  let banana_total = multiply(banana_price, banana_quantity)
  let cart_total = add(apple_total, banana_total)
  
  assert_eq(30, cart_total)
}

test "assertion_functions_test" {
  // 测试断言函数
  assert_true(10 > 5)
  assert_false(5 > 10)
  assert_eq(42, multiply(6, 7))
  assert_eq_string("Hello, Test!", greet("Test"))
}