// Azimuth 综合测试套件
// 使用标准 MoonBit 测试语法，包含不超过10个测试用例

test "add_function_basic_operations" {
  // 测试 add 函数的基本运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_basic_operations" {
  // 测试 multiply 函数的基本运算
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 用户!", greet("用户"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_function_positive_numbers" {
  // 测试 divide_with_ceil 函数的正数除法
  assert_eq(3, divide_with_ceil(10, 4))    // 10/4 = 2.5, 向上取整为 3
  assert_eq(2, divide_with_ceil(8, 4))     // 8/4 = 2, 无需向上取整
  assert_eq(1, divide_with_ceil(1, 1))     // 1/1 = 1
  assert_eq(5, divide_with_ceil(9, 2))     // 9/2 = 4.5, 向上取整为 5
  assert_eq(0, divide_with_ceil(0, 5))     // 0/5 = 0
}

test "divide_with_ceil_function_negative_numbers" {
  // 测试 divide_with_ceil 函数的负数除法
  assert_eq(-2, divide_with_ceil(-10, 4))   // -10/4 = -2.5, 向上取整为 -2
  assert_eq(-2, divide_with_ceil(10, -4))   // 10/-4 = -2.5, 向上取整为 -2
  assert_eq(3, divide_with_ceil(-10, -4))   // -10/-4 = 2.5, 向上取整为 3
  assert_eq(-1, divide_with_ceil(-1, 1))    // -1/1 = -1
  assert_eq(0, divide_with_ceil(0, -5))     // 0/-5 = 0
}

test "divide_with_ceil_function_edge_cases" {
  // 测试 divide_with_ceil 函数的边界情况
  assert_eq(0, divide_with_ceil(5, 0))      // 除数为0，返回0
  assert_eq(0, divide_with_ceil(0, 0))      // 除数和被除数都为0，返回0
  assert_eq(1, divide_with_ceil(1, 1))      // 最小正整数除法
  assert_eq(-1, divide_with_ceil(-1, 1))    // 最小负整数除法
  assert_eq(2147483647, divide_with_ceil(2147483647, 1))  // 最大整数除以1
}

test "complex_mathematical_expressions" {
  // 测试复杂的数学表达式
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(16, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(17, result2)
  
  // a + b * c
  let result3 = add(a, multiply(b, c))
  assert_eq(11, result3)
}

test "business_calculation_scenario" {
  // 测试商业计算场景
  let price = 100
  let quantity = 5
  let discount_rate = 10
  let tax_rate = 8
  
  // 计算小计
  let subtotal = multiply(price, quantity)
  assert_eq(500, subtotal)
  
  // 计算折扣
  let discount = divide_with_ceil(multiply(subtotal, discount_rate), 100)
  assert_eq(50, discount)
  
  // 计算税后总价
  let after_discount = add(subtotal, -discount)
  let tax = divide_with_ceil(multiply(after_discount, tax_rate), 100)
  let total = add(after_discount, tax)
  assert_eq(486, total)
}

test "string_processing_with_math" {
  // 测试字符串处理与数学运算结合
  let base_name = "User"
  let user_count = 42
  
  // 创建用户欢迎消息
  let greeting = greet(base_name)
  assert_eq_string("Hello, User!", greeting)
  
  // 计算用户ID范围
  let start_id = 1
  let end_id = add(start_id, user_count)
  assert_eq(43, end_id)
  
  // 计算每页用户数（假设每页10个用户）
  let users_per_page = 10
  let total_pages = divide_with_ceil(user_count, users_per_page)
  assert_eq(5, total_pages)
}

test "comprehensive_error_handling" {
  // 测试综合错误处理和边界情况
  // 测试大数运算
  let large_num = 1000000
  let small_num = 1
  
  assert_eq(1000001, add(large_num, small_num))
  assert_eq(1000000, multiply(large_num, small_num))
  
  // 测试负数运算
  let negative_num = -100
  let positive_num = 50
  
  assert_eq(-50, add(negative_num, positive_num))
  assert_eq(-5000, multiply(negative_num, positive_num))
  
  // 测试零值运算
  assert_eq(50, add(0, 50))
  assert_eq(0, multiply(0, 50))
  assert_eq(0, divide_with_ceil(0, 50))
  
  // 测试断言函数
  assert_true(true)
  assert_false(false)
}