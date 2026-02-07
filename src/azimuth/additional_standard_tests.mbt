// 额外的标准 MoonBit 测试用例
// 为 azimuth 项目添加的核心功能测试

test "add_function_large_numbers" {
  // 测试加法函数的大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(0, add(-1000000, 1000000))
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(1000001, add(1000000, 1))
  assert_eq(1, add(0, 1))
}

test "multiply_function_precision" {
  // 测试乘法函数的精度
  assert_eq(1, multiply(1, 1))
  assert_eq(25, multiply(5, 5))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(-100000000, multiply(-10000, 10000))
  assert_eq(100000000, multiply(-10000, -10000))
}

test "divide_with_ceil_fractional_cases" {
  // 测试向上取整除法函数的小数情况
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(34, divide_with_ceil(100, 3))
  assert_eq(34, divide_with_ceil(101, 3))
}

test "subtract_function_borrow_simulation" {
  // 测试减法函数的借位模拟
  assert_eq(1, subtract(1000, 999))
  assert_eq(0, subtract(1000, 1000))
  assert_eq(999, subtract(1000, 1))
  assert_eq(1001, subtract(1000, -1))
  assert_eq(-1, subtract(0, 1))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 
	!", greet("\n\t"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, a&b*c!", greet("a&b*c"))
}

test "complex_financial_calculation" {
  // 测试复杂金融计算场景
  let principal = 10000
  let rate1 = 5
  let rate2 = 3
  let years = 2
  
  // 第一年利息
  let interest1 = divide_with_ceil(multiply(principal, rate1), 100)
  let total1 = add(principal, interest1)
  
  // 第二年利息（基于新的总额）
  let interest2 = divide_with_ceil(multiply(total1, rate2), 100)
  let final_total = add(total1, interest2)
  
  // 总收益
  let total_profit = subtract(final_total, principal)
  
  assert_eq(500, interest1)
  assert_eq(10500, total1)
  assert_eq(315, interest2)
  assert_eq(10815, final_total)
  assert_eq(815, total_profit)
}

test "algorithm_efficiency_simulation" {
  // 测试算法效率模拟
  let n = 100
  
  // O(1) 常数时间
  let constant_result = multiply(2, 3)
  
  // O(n) 线性时间模拟
  let linear_sum = divide_with_ceil(multiply(n, add(n, 1)), 2)
  
  // O(n²) 二次时间模拟
  let quadratic_result = multiply(n, n)
  
  // O(log n) 对数时间模拟（简化）
  let log_result = divide_with_ceil(n, 2)
  
  assert_eq(6, constant_result)
  assert_eq(5050, linear_sum)
  assert_eq(10000, quadratic_result)
  assert_eq(50, log_result)
}

test "data_structure_operations" {
  // 测试数据结构操作模拟
  let initial_size = 0
  let operations = 10
  
  // 模拟栈操作：push 5 次，pop 3 次
  let push_operations = 5
  let pop_operations = 3
  
  let after_push = add(initial_size, push_operations)
  let after_pop = subtract(after_push, pop_operations)
  
  // 模拟队列操作：enqueue 7 次，dequeue 4 次
  let enqueue_operations = 7
  let dequeue_operations = 4
  
  let after_enqueue = add(after_pop, enqueue_operations)
  let after_dequeue = subtract(after_enqueue, dequeue_operations)
  
  // 验证最终状态
  assert_eq(5, after_push)
  assert_eq(2, after_pop)
  assert_eq(9, after_enqueue)
  assert_eq(5, after_dequeue)
}