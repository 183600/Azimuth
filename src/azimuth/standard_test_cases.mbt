// 标准 MoonBit 测试用例
// 覆盖 azimuth 库的核心功能

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(15, 5), 3)
  assert_eq(divide_with_ceil(7, 2), 4)
  assert_eq(divide_with_ceil(100, 9), 12)
  assert_eq(divide_with_ceil(1, 1), 1)
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
  assert_eq(divide_with_ceil(-7, 2), -3)
  assert_eq(divide_with_ceil(7, -2), 3)
}

test "divide_with_ceil_zero_cases" {
  // 测试包含零的除法情况
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(0, 1), 0)
  assert_eq(divide_with_ceil(0, -3), 0)
  assert_eq(divide_with_ceil(5, 0), 0)
  assert_eq(divide_with_ceil(-5, 0), 0)
}

test "greet_function_variations" {
  // 测试问候函数的各种输入
  assert_eq_string(greet("Alice"), "Hello, Alice!")
  assert_eq_string(greet("Bob"), "Hello, Bob!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
  assert_eq_string(greet("!@#$"), "Hello, !@#$!")
}

test "arithmetic_function_combinations" {
  // 测试算术函数的组合使用
  let a = 12
  let b = 5
  let c = 3
  
  // 复合计算：(a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(result1, 51)
  
  // 复合计算：a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(result2, 63)
  
  // 复合计算：(a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(result3, 3)
}

test "mathematical_properties_verification" {
  // 测试数学性质的验证
  let x = 8
  let y = 6
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律
  let z = 4
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "boundary_and_limit_values" {
  // 测试边界和极限值
  // 大数运算
  assert_eq(add(999999, 1), 1000000)
  assert_eq(multiply(1000, 1000), 1000000)
  assert_eq(subtract(1000000, 1), 999999)
  
  // 小数运算
  assert_eq(add(1, 1), 2)
  assert_eq(multiply(1, 1), 1)
  assert_eq(divide_with_ceil(1, 1), 1)
  
  // 零值运算
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 0), 0)
  assert_eq(subtract(0, 0), 0)
}

test "practical_calculation_scenarios" {
  // 测试实际计算场景
  // 购物计算：商品价格 * 数量 + 税费
  let item_price = 25
  let quantity = 4
  let tax_rate = 10
  let subtotal = multiply(item_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax)
  
  assert_eq(subtotal, 100)
  assert_eq(tax, 10)
  assert_eq(total, 110)
  
  // 时间计算：小时转换为分钟
  let hours = 3
  let minutes_per_hour = 60
  let total_minutes = multiply(hours, minutes_per_hour)
  
  assert_eq(total_minutes, 180)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  // 除数为零的情况
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(-10, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  
  // 极端值测试
  assert_eq(divide_with_ceil(1, 1000000), 1)
  assert_eq(divide_with_ceil(1000000, 1), 1000000)
  
  // 负数边界测试
  assert_eq(divide_with_ceil(-1, 2), 0)
  assert_eq(divide_with_ceil(1, -2), 0)
  assert_eq(divide_with_ceil(-1, -2), 1)
}

test "complex_algorithmic_implementation" {
  // 测试复杂算法实现
  // 计算阶乘的简化版本（仅限小数）
  let n = 5
  let factorial = multiply(multiply(multiply(multiply(1, 2), 3), 4), 5)
  assert_eq(factorial, 120)
  
  // 计算斐波那契数列的第n项（简化版）
  let fib_n = 7
  let fib_prev = 8    // F(6)
  let fib_prev_prev = 5  // F(5)
  let fib_current = add(fib_prev, fib_prev_prev)
  assert_eq(fib_current, 13)
  
  // 计算等差数列的和
  let first_term = 3
  let common_diff = 2
  let num_terms = 5
  let last_term = add(first_term, multiply(common_diff, subtract(num_terms, 1)))
  let sum = divide_with_ceil(multiply(num_terms, add(first_term, last_term)), 2)
  
  assert_eq(last_term, 11)
  assert_eq(sum, 35)
}