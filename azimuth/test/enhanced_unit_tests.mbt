// 增强的单元测试用例
// 测试边界值、特殊情况以及复合场景

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, add(0, max_val))
  assert_eq(max_val - 1, add(max_val - 1, 0))
  
  // 测试最小值边界
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(0, min_val))
  assert_eq(min_val + 1, add(min_val + 1, 0))
  
  // 测试相邻值相加
  assert_eq(max_val - 1, add(max_val - 2, 1))
  assert_eq(min_val + 1, add(min_val + 2, -1))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值乘以小数
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(-max_val, multiply(max_val, -1))
  assert_eq(0, multiply(max_val, 0))
  
  // 测试最小值的特殊情况
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(min_val, multiply(min_val, 2))
  
  // 测试2的幂次方乘法
  assert_eq(8, multiply(2, 4))
  assert_eq(16, multiply(4, 4))
  assert_eq(-8, multiply(-2, 4))
}

test "greet_function_special_cases" {
  // 测试字符串拼接的特殊情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, !", greet(""))
  
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello,   !", greet("  "))  // 空格
  assert_eq_string("Hello, \n\t!", greet("\n\t"))  // 换行和制表符
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(1, 2))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-11, 3))
  assert_eq(-4, divide_with_ceil(-11, -3))
  assert_eq(-3, divide_with_ceil(-12, 4))
  assert_eq(3, divide_with_ceil(-12, -4))
  
  // 零和边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(5, divide_with_ceil(5, 1))
  assert_eq(-5, divide_with_ceil(-5, 1))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 10
  let b = 20
  let c = -5
  
  // 复合加法测试
  assert_eq(30, add(add(a, b), c))
  assert_eq(25, add(a, add(b, c)))
  assert_eq(15, add(add(a, c), b))
  
  // 复合乘法测试
  assert_eq(200, multiply(multiply(a, b), 1))
  assert_eq(-100, multiply(multiply(a, c), 2))
  assert_eq(50, multiply(a, multiply(b, c) * -1))
  
  // 混合运算测试
  let sum_result = add(a, b)
  let product_result = multiply(a, b)
  assert_eq(30, sum_result)
  assert_eq(200, product_result)
  assert_eq(230, add(sum_result, product_result))
}

test "overflow_protection_comprehensive" {
  // 全面测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(max_val, add(max_val / 2, max_val / 2 + 1))
  assert_eq(min_val, add(min_val, min_val))
  assert_eq(min_val, add(min_val / 2, min_val / 2 - 1))
  
  // 乘法溢出保护
  assert_eq(max_val, multiply(max_val, max_val))
  assert_eq(max_val, multiply(46340, 46341))  // 接近sqrt(max_val)
  assert_eq(min_val, multiply(min_val, min_val))
  assert_eq(min_val, multiply(-46340, 46341))
}

test "string_operations_extended" {
  // 扩展字符串操作测试
  // 测试不同长度的字符串
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, ABC!", greet("ABC"))
  assert_eq_string("Hello, 这是一个很长的字符串测试!", greet("这是一个很长的字符串测试"))
  
  // 测试数字字符串
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 3.14159!", greet("3.14159"))
  
  // 测试混合内容
  assert_eq_string("Hello, Test123!", greet("Test123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}

test "mathematical_properties" {
  // 测试数学性质
  // 测试基本值
  let test_values = [0, 1, -1, 42, -42, 1000, -1000]
  
  // 测试第一个值
  let x = test_values[0]
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
  
  // 测试第二个值
  let x2 = test_values[1]
  assert_eq(x2, add(x2, 0))
  assert_eq(x2, add(0, x2))
  assert_eq(x2, multiply(x2, 1))
  assert_eq(x2, multiply(1, x2))
  assert_eq(0, multiply(x2, 0))
  assert_eq(0, multiply(0, x2))
  
  // 测试第三个值
  let x3 = test_values[2]
  assert_eq(x3, add(x3, 0))
  assert_eq(x3, add(0, x3))
  assert_eq(x3, multiply(x3, 1))
  assert_eq(x3, multiply(1, x3))
  assert_eq(0, multiply(x3, 0))
  assert_eq(0, multiply(0, x3))
  
  // 测试正数加法交换律
  let y = 42
  assert_eq(add(1, y), add(y, 1))
  assert_eq(add(42, y), add(y, 42))
}

test "error_handling_and_robustness" {
  // 测试错误处理和健壮性
  // 测试极端值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极端值加法
  assert_eq(max_val, add(max_val, 1000000))
  assert_eq(min_val, add(min_val, -1000000))
  
  // 极端值乘法
  assert_eq(max_val, multiply(1000000, 3000))
  assert_eq(min_val, multiply(-1000000, 3000))
  
  // 断言函数健壮性测试
  assert_true(1 == 1)
  assert_false(1 == 2)
  assert_eq_string("", "")
  assert_eq_string("a", "a")
}