// Azimuth 综合测试用例
// 补充现有测试未覆盖的场景，包含8个高质量测试用例

test "extreme_value_calculations" {
  // 测试极值计算
  assert_eq(4000000, add(2000000, 2000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(100001, divide_with_ceil(200001, 2))
  assert_eq(-4000000, add(-2000000, -2000000))
}

test "divide_with_ceil_comprehensive_negative" {
  // 全面测试负数除法的向上取整
  assert_eq(-2, divide_with_ceil(-4, 2))
  assert_eq(-1, divide_with_ceil(-3, 2))
  assert_eq(0, divide_with_ceil(-1, 2))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(2, divide_with_ceil(-6, -3))
  assert_eq(1, divide_with_ceil(-5, -3))
}

test "greet_function_comprehensive_strings" {
  // 全面测试字符串处理
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, hello world!", greet("hello world"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !@#$%^&*()!", greet("!@#$%^&*()"))
  assert_eq_string("Hello,   !", greet("   "))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  // 结合律
  let a = 7; let b = 11; let c = 13
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 单位元性质
  assert_eq(a, add(a, 0))
  assert_eq(a, multiply(a, 1))
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  // 除零处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数除法
  assert_eq(1, divide_with_ceil(2147483647, 2147483647))
  assert_eq(2, divide_with_ceil(2147483647, 2147483646))
}

test "complex_business_logic" {
  // 复杂业务逻辑测试
  // 库存管理场景
  let initial_stock = 1000
  let daily_sales = 75
  let days = 12
  let total_sold = multiply(daily_sales, days)
  let remaining_stock = add(initial_stock, -total_sold)
  let restock_batches = divide_with_ceil(remaining_stock, 200)
  
  assert_eq(900, total_sold)
  assert_eq(100, remaining_stock)
  assert_eq(1, restock_batches)
}

test "string_and_number_integration" {
  // 字符串和数字集成测试
  let user_id = 12345
  let user_name = "张三"
  let greeting = greet(user_name)
  
  // 验证问候包含用户名
  assert_true(String.length(greeting) > String.length(user_name))
  assert_eq_string("Hello, 张三!", greeting)
  
  // 复合计算
  let bonus_multiplier = 2
  let base_salary = 5000
  let total_salary = multiply(base_salary, bonus_multiplier)
  assert_eq(10000, total_salary)
}

test "edge_case_arithmetic_sequences" {
  // 边界情况算术序列
  // 斐波那契风格计算
  let fib_0 = 0
  let fib_1 = 1
  let fib_2 = add(fib_0, fib_1)
  let fib_3 = add(fib_1, fib_2)
  let fib_4 = add(fib_2, fib_3)
  
  assert_eq(1, fib_2)
  assert_eq(2, fib_3)
  assert_eq(3, fib_4)
  
  // 等差数列
  let first_term = 5
  let common_diff = 3
  let n_terms = 6
  let sum_n = multiply(divide_with_ceil(n_terms, 2), add(multiply(2, first_term), multiply(common_diff, add(n_terms, -1))))
  assert_eq(78, sum_n)  // 5+8+11+14+17+20 = 78
}