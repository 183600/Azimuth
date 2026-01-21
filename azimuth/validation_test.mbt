// 简单测试验证 advanced_test_suite.mbt 中的测试用例
// 导入 azimuth 包中的函数

test "quick_validation_test" {
  // 快速验证测试用例的核心逻辑
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证基本功能
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq_string("Hello, Test!", greet("Test"))
  
  // 验证边界情况
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 验证除法
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(-1, divide_with_ceil(-1, 2))
  
  // 验证断言函数
  assert_true(true)
  assert_false(false)
}

test "performance_quick_test" {
  // 简化的性能测试
  let mut sum = 0
  for i = 1; i <= 10; i = i + 1 {
    sum = add(sum, i)
  }
  assert_eq(55, sum)
  
  let mut product = 1
  for i = 1; i <= 3; i = i + 1 {
    product = multiply(product, i)
  }
  assert_eq(6, product)
}

test "boundary_quick_test" {
  // 简化的边界测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  let result1 = add(max_val, min_val)
  let result2 = multiply(max_val, 0)
  let result3 = multiply(max_val, 1)
  
  assert_eq(-1, result1)
  assert_eq(0, result2)
  assert_eq(max_val, result3)
}

test "financial_quick_test" {
  // 简化的金融计算测试
  let principal = 10000
  let rate = 5
  let interest = divide_with_ceil(multiply(principal, 1), rate)
  let total = add(principal, interest)
  
  assert_eq(2000, interest)
  assert_eq(12000, total)
}

test "string_quick_test" {
  // 简化的字符串测试
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
}

test "math_sequence_quick_test" {
  // 简化的数学序列测试
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)
  let fib4 = add(fib2, fib3)
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  
  let fact1 = 1
  let fact2 = multiply(fact1, 2)
  let fact3 = multiply(fact2, 3)
  
  assert_eq(2, fact2)
  assert_eq(6, fact3)
}

test "error_recovery_quick_test" {
  // 简化的错误恢复测试
  let max_val = 2147483647
  let overflow = add(max_val, 1000)
  let normal = add(100, 200)
  
  assert_eq(max_val, overflow)
  assert_eq(300, normal)
}

test "precision_quick_test" {
  // 简化的精度测试
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(1, divide_with_ceil(3, 4))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(333334, divide_with_ceil(1000000, 3))
}