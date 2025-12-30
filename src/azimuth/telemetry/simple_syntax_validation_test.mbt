// 简单的语法验证测试

test "simple_syntax_validation" {
  // 测试基本语法
  
  let service_name = "test-service"
  let metric_value = 42.5
  
  assert_eq(service_name.length(), 12)
  assert_eq(metric_value > 40.0, true)
  
  // 测试数组操作
  let numbers = [1, 2, 3, 4, 5]
  let mut sum = 0
  
  let mut i = 0
  while i < numbers.length() {
    sum = sum + numbers[i]
    i = i + 1
  }
  
  assert_eq(sum, 15)
}

test "basic_math_operations" {
  // 测试基本数学运算
  
  let a = 10.0
  let b = 5.0
  
  let sum = a + b
  let product = a * b
  let quotient = a / b
  
  assert_eq(sum, 15.0)
  assert_eq(product, 50.0)
  assert_eq(quotient, 2.0)
  
  // 测试平方根近似（不用@sqrt）
  let number = 16.0
  let mut sqrt_approx = number / 2.0
  let mut iterations = 0
  
  while iterations < 5 {
    sqrt_approx = (sqrt_approx + number / sqrt_approx) / 2.0
    iterations = iterations + 1
  }
  
  assert_eq(sqrt_approx > 3.9 && sqrt_approx < 4.1, true)
}